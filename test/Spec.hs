module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, nominalDay)
import Oglyad.App (runApp)
import Oglyad.Check (checkNames, parseCheckName)
import Oglyad.Check.BuildLag qualified as BuildLag
import Oglyad.Check.Drift qualified as Drift
import Oglyad.Check.DuplicateName qualified as DuplicateName
import Oglyad.Check.FedoraOverlap qualified as FedoraOverlap
import Oglyad.Check.Stale qualified as Stale
import Oglyad.Ignore
import Oglyad.Process (childEnv, run)
import Oglyad.Report (Report (..), renderJson, renderMarkdown, renderText)
import Oglyad.Source.Monorepo (PkgDir (..), findPackageDirs, hclString, lastCommits, macroFingerprint, specFingerprint)
import Oglyad.Source.Repodata (RpmEntry (..), decompressAndParse, fetchRepo, latestBySource, parsePrimary, srpmName)
import Oglyad.Source.Rhai (parseUpdateRhai)
import Oglyad.Types
import Oglyad.Version
import System.Directory (createDirectoryIfMissing, createDirectoryLink, createFileLink)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (byteStringInput, proc, readProcess, runProcess_, setEnv, setStdin)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "parseEVR" $ do
    it "parses version-release" $
      parseEVR "1.2.3-1.fc44" `shouldBe` Just (EVR 0 "1.2.3" "1.fc44")
    it "parses an epoch" $
      parseEVR "2:1.0-3" `shouldBe` Just (EVR 2 "1.0" "3")
    it "treats (none) as epoch 0" $
      parseEVR "(none):1.0-3" `shouldBe` Just (EVR 0 "1.0" "3")
    it "rejects a missing release" $
      parseEVR "1.0" `shouldBe` Nothing

  -- vectors from rpm's tests/rpmvercmp.at
  describe "rpmvercmp" $ do
    mapM_
      vercmpCase
      [ ("1.0", "1.0", EQ),
        ("1.0", "2.0", LT),
        ("2.0.1", "2.0", GT),
        ("2.0.1a", "2.0.1", GT),
        ("5.5p1", "5.5p2", LT),
        ("5.5p10", "5.5p1", GT),
        ("10xyz", "10.1xyz", LT),
        ("xyz10", "xyz10.1", LT),
        ("xyz.4", "8", LT),
        ("1.05", "1.5", EQ),
        ("2.0", "2_0", EQ),
        ("2a", "2.0", LT),
        ("1.0~rc1", "1.0", LT),
        ("1.0~rc1", "1.0~rc2", LT),
        ("1.0~rc1~git123", "1.0~rc1", LT),
        ("1.0^", "1.0", GT),
        ("1.0^git1", "1.01", LT),
        ("1.0^git1~pre", "1.0^git1", LT),
        ("1.0~rc1^git1", "1.0~rc1", GT),
        ("1.0^git1", "1.0^git2", LT),
        ("1.0^20160101", "1.0.1", LT)
      ]
    it "is reflexive" $ property $ \(V a) -> rpmvercmp a a == EQ
    it "is antisymmetric" $ property $ \(V a) (V b) ->
      rpmvercmp a b == flipOrd (rpmvercmp b a)

  describe "normalizeVersion" $ do
    it "drops a leading v" $ normalizeVersion "v1.2.3" `shouldBe` "1.2.3"
    it "drops a name prefix" $ normalizeVersion "jq-1.7.1" `shouldBe` "1.7.1"
    it "drops a monorepo tag prefix" $ normalizeVersion "v2/v1.0.28" `shouldBe` "1.0.28"
    it "turns dashes into dots like anda" $ normalizeVersion "2024-01-05" `shouldBe` "2024.01.05"
    it "spells prereleases with a tilde" $ normalizeVersion "6.0.0-alpha1" `shouldBe` "6.0.0~alpha1"

  describe "compareUpstream" $ do
    mapM_
      (\(up, spec, o) -> it (T.unpack up <> " vs spec " <> T.unpack spec) $ compareUpstream up spec `shouldBe` o)
      [ ("v1.3.0", "1.2.0", GT),
        ("v1.2.0", "1.2.0", EQ),
        ("1.2.0", "1.3.0", LT),
        ("6.0.0-alpha1", "6.0.0~alpha1", EQ),
        ("6.0.0", "6.0.0~alpha1", GT),
        ("0.4.2", "0.4~2", EQ),
        ("2026-07-27", "20260727", EQ),
        ("2.005R", "2.005", EQ),
        ("3.17.0-ogc1", "3.17.0^1", EQ),
        ("6.7.3", "v6.7.3", EQ),
        ("0.42.0", "0.41.0^20261031.9e2ed65", GT),
        -- a snapshot taken after the release it's based on isn't drift
        ("0.41.0", "0.41.0^20261031.9e2ed65", EQ)
      ]

  describe "srpmName" $ do
    it "strips version and release" $
      srpmName "zed-0.190.0-1.fc44.src.rpm" `shouldBe` Just "zed"
    it "keeps dashes in the name" $
      srpmName "python-foo-bar-1.0-2.fc44.src.rpm" `shouldBe` Just "python-foo-bar"

  describe "parsePrimary" $ do
    let entries = either (const []) id (parsePrimary primaryFixture)
        firstOf f = map f (take 1 entries)
    it "finds every package" $ map rpmName entries `shouldBe` ["zed", "zed-cli"]
    it "reads the EVR" $ firstOf rpmEVR `shouldBe` [EVR 0 "0.190.0" "1.fc44"]
    it "reads the build time" $ firstOf rpmBuildTime `shouldBe` [posix 1794200000]
    it "reads the source rpm" $ firstOf rpmSourceRpm `shouldBe` [Just "zed-0.190.0-1.fc44.src.rpm"]
    it "groups binaries by source package" $
      Map.keys (latestBySource entries) `shouldBe` ["zed"]
    it "rejects garbage" $ parsePrimary "<metadata><package>" `shouldSatisfy` isLeft
    it "decompresses zstd through the zstd command" $ do
      -- the same fixture, compressed by the real zstd tool
      (_, compressed, _) <- readProcess (setStdin (byteStringInput primaryFixture) (proc "zstd" ["-c", "-q"]))
      r <- decompressAndParse "repodata/abc-primary.xml.zst" compressed
      fmap (map rpmName) r `shouldBe` Right ["zed", "zed-cli"]
    forM_ [("gzip", ".gz"), ("xz", ".xz")] $ \(tool, ext) ->
      it ("decompresses " <> tool <> " through the " <> tool <> " command") $ do
        (_, compressed, _) <- readProcess (setStdin (byteStringInput primaryFixture) (proc tool ["-c", "-q"]))
        r <- decompressAndParse ("repodata/abc-primary.xml" <> T.pack ext) compressed
        fmap (map rpmName) r `shouldBe` Right ["zed", "zed-cli"]
    it "reports corrupt zstd instead of crashing" $ do
      r <- decompressAndParse "repodata/abc-primary.xml.zst" "definitely not zstd"
      r `shouldSatisfy` isLeft

  describe "hclString" $ do
    let hcl = "project pkg {\n  rpm {\n    spec = \"zed.spec\"\n  }\n  labels {\n    subrepo = \"extras\"\n  }\n}\n"
    it "finds the spec" $ hclString "spec" hcl `shouldBe` Just "zed.spec"
    it "finds the subrepo" $ hclString "subrepo" hcl `shouldBe` Just "extras"
    it "doesn't match key prefixes" $ hclString "sub" hcl `shouldBe` Nothing

  describe "parseUpdateRhai" $ do
    it "understands gh()" $
      parseUpdateRhai "rpm.version(gh(\"zed-industries/zed\"));\n" `shouldBe` GitHub "zed-industries/zed"
    it "understands two-arg helpers" $
      parseUpdateRhai "rpm.version(gitea(\"gitea.com\", \"gitea/tea\"));" `shouldBe` Gitea "gitea.com" "gitea/tea"
    it "maps codeberg to gitea" $
      parseUpdateRhai "rpm.version(codeberg(\"a/b\"))" `shouldBe` Gitea "codeberg.org" "a/b"
    it "follows let bindings" $
      parseUpdateRhai "let v = gh_tag(\"x/y\");\nif v.len > 0 {\n  rpm.version(v);\n}" `shouldBe` GitHubTag "x/y"
    it "follows rpm.global(\"ver\", ...)" $
      parseUpdateRhai nightly `shouldBe` GitHub "OpenTabletDriver/OpenTabletDriver"
    it "spots commit tracking" $
      parseUpdateRhai "rpm.global(\"commit\", gh_commit(\"a/b\"));\nif rpm.changed() { rpm.release(); }" `shouldBe` CommitTracked
    it "keeps raw backtick regexes intact" $
      parseUpdateRhai "let v = gh_tag(\"a/b\");\nif `[\\d.]+-beta\\.\\d+`.find_all(v).len == 0 {\n  rpm.version(v);\n}" `shouldBe` GitHubTag "a/b"
    it "follows variables through other calls" $
      parseUpdateRhai "let tag = gh_tag(\"x/ghidra\");\nlet v = find(`Ghidra_([\\d.]+)_build`, tag, 1);\nrpm.version(v);" `shouldBe` GitHubTag "x/ghidra"
    it "marks distro-following versions as derived" $
      parseUpdateRhai "import \"andax/bump_extras.rhai\" as bump;\nrpm.version(bump::madoguchi(\"zig-master\", labels.branch));" `shouldBe` Derived "bump::madoguchi"
    it "ignores comments" $
      parseUpdateRhai "// gh(\"wrong/one\")\nrpm.version(pypi(\"west\"));" `shouldBe` PyPI "west"
    it "falls back to Unknown" $
      parseUpdateRhai "let x = get(\"https://example.com\").json();\nrpm.version(x.version);" `shouldSatisfy` isUnknown

  describe "drift check" $ do
    it "warns when upstream is newer" $
      severities (Drift.check snapshot (pkg {pkgUpstreamVer = Just "v1.3.0"})) `shouldBe` [Warn]
    it "is quiet when versions match" $
      Drift.check snapshot (pkg {pkgUpstreamVer = Just "v1.2.0"}) `shouldBe` []
    it "ignores snapshots ahead of the last release" $
      Drift.check snapshot (pkg {pkgSpecEVR = Just (EVR 0 "1.2.0^20261101git1234abc" "1.fc44"), pkgUpstreamVer = Just "1.2.0"}) `shouldBe` []
    it "is quiet without upstream data" $
      Drift.check snapshot pkg `shouldBe` []

  describe "fedora-overlap check" $ do
    let withFedora = snapshot {snapFedora = Set.singleton "zed"}
    it "warns when Fedora ships the same package" $
      severities (FedoraOverlap.check withFedora pkg) `shouldBe` [Warn]
    it "ignores packages in subrepos" $
      FedoraOverlap.check withFedora (pkg {pkgSubrepo = Just "multimedia"}) `shouldBe` []
    it "is quiet when Fedora doesn't have it" $
      FedoraOverlap.check snapshot pkg `shouldBe` []

  describe "duplicate-name check" $ do
    let other = pkg {pkgPath = "anda/devs/zed/nightly"}
        twoOf a b = snapshot {snapPackages = Map.fromList [(pkgPath a, a), (pkgPath b, b)]}
    it "flags two dirs building the same name into one repo" $
      severities (DuplicateName.check (twoOf pkg other) pkg) `shouldBe` [Warn]
    it "allows the same name in different subrepos" $
      DuplicateName.check (twoOf pkg other {pkgSubrepo = Just "extras"}) pkg `shouldBe` []
    it "is quiet for unique names" $
      DuplicateName.check snapshot pkg `shouldBe` []

  describe "report" $ do
    let f path = Finding "build-lag" "zed" (Just path) Warn "x"
    it "shows the directory when a name is ambiguous" $
      renderText Info (report [f "anda/a", f "anda/b"]) `shouldSatisfy` T.isInfixOf "zed (anda/a): x"
    it "keeps it short otherwise" $
      renderText Info (report [f "anda/a"]) `shouldSatisfy` T.isInfixOf "zed: x"
    it "counts ignored findings in the summary" $
      renderText Info ((report [f "anda/a"]) {rIgnored = [f "anda/b"]}) `shouldSatisfy` T.isInfixOf "(1 ignored)"
    it "wraps json in a versioned envelope" $ do
      let o = case decode (renderJson (report [f "anda/a"])) of
            Just (Object x) -> x
            _ -> KM.empty
      KM.lookup "schema" o `shouldBe` Just (Number 1)
      KM.lookup "summary" o `shouldSatisfy` maybe False (\case Object s -> KM.lookup "warn" s == Just (Number 1); _ -> False)
      KM.lookup "findings" o `shouldSatisfy` maybe False (\case Array a -> length a == 1; _ -> False)
    it "escapes pipes in markdown tables" $
      renderMarkdown Info (report [(f "anda/a") {fMessage = "a|b"}]) `shouldSatisfy` T.isInfixOf "a\\|b"
    it "folds info findings away in markdown" $
      renderMarkdown Info (report [(f "anda/a") {fSeverity = Info}]) `shouldSatisfy` T.isInfixOf "<details><summary>Info (1)"

  describe "ignores" $ do
    let f check path name = Finding check name (Just path) Warn "x"
        dup = f "duplicate-name" "anda/langs/zig/master-bootstrap" "zig-master"
        lag = f "build-lag" "anda/devs/zed/stable" "zed"
        file rules = IgnoreFile rules
        rule check target = IgnoreRule check target "because"
        apply rules labels = applyIgnores checkNames checkNames (file rules) (Map.fromList labels)
    it "parses a valid file" $
      parseIgnoreFile checkNames "{\"ignore\": [{\"check\": \"stale\", \"package\": \"zed\", \"reason\": \"fine\"}]}"
        `shouldBe` Right (file [IgnoreRule "stale" (ByName "zed") "fine"])
    forM_
      [ ("an empty reason", "{\"ignore\": [{\"check\": \"stale\", \"package\": \"zed\", \"reason\": \" \"}]}"),
        ("an unknown check", "{\"ignore\": [{\"check\": \"stael\", \"package\": \"zed\", \"reason\": \"x\"}]}"),
        ("both path and package", "{\"ignore\": [{\"check\": \"stale\", \"package\": \"zed\", \"path\": \"anda/x\", \"reason\": \"x\"}]}")
      ]
      $ \(what, raw) -> it ("rejects " <> what) $ parseIgnoreFile checkNames raw `shouldSatisfy` either (const True) (const False)
    it "ignores by path" $ do
      let Applied {..} = apply [rule "duplicate-name" (ByPath "anda/langs/zig/master-bootstrap")] [] [dup, lag]
      (apKept, apIgnored, apProblems) `shouldBe` ([lag], [dup], [])
    it "ignores by package name" $ do
      let Applied {..} = apply [rule "build-lag" (ByName "zed")] [] [dup, lag]
      (apKept, apIgnored) `shouldBe` ([dup], [lag])
    it "ignores by anda.hcl label" $ do
      let Applied {..} = apply [] [("anda/langs/zig/master-bootstrap", ["duplicate-name"])] [dup, lag]
      (apKept, apIgnored) `shouldBe` ([lag], [dup])
    it "only matches the named check" $ do
      let Applied {..} = apply [rule "stale" (ByName "zed")] [] [lag]
      apKept `shouldBe` [lag]
    it "reports entries that match nothing" $ do
      let Applied {..} = apply [rule "stale" (ByName "gone")] [] [lag]
      map fCheck apProblems `shouldBe` ["unused-ignore"]
    it "doesn't call entries unused when their check didn't run" $ do
      let Applied {..} = applyIgnores checkNames ["build-lag"] (file [rule "stale" (ByName "gone")]) Map.empty [lag]
      apProblems `shouldBe` []
    it "flags unknown check names in labels" $ do
      let Applied {..} = apply [] [("anda/x", ["stael"])] []
      map fSeverity apProblems `shouldBe` [Warn]

  describe "build-lag check" $ do
    it "says never published when the repo was fetched" $
      severities (BuildLag.check snapshot pkg) `shouldBe` [Error]
    it "stays quiet when the repo fetch failed" $
      BuildLag.check snapshot pkg {pkgRepoChecked = False} `shouldBe` []

  describe "child processes" $ do
    it "never get secrets from our environment" $ do
      let env = childEnv [("PATH", "/usr/bin"), ("HOME", "/home/x"), ("GITHUB_TOKEN", "secret"), ("GH_TOKEN", "secret"), ("AWS_SECRET_ACCESS_KEY", "s"), ("LC_ALL", "C")]
      map fst env `shouldMatchList` ["GIT_TERMINAL_PROMPT", "PATH", "HOME", "LC_ALL"]
    it "can't read GITHUB_TOKEN even when spec-like code asks for it" $
      withEnv "GITHUB_TOKEN" "oglyad-test-secret" $ do
        r <- run "sh" ["-c", "env"]
        r `shouldSatisfy` either (const False) (not . T.isInfixOf "oglyad-test-secret")

  describe "filesystem edge cases" $ do
    it "fingerprints a package with a dangling symlink instead of crashing" $
      withSystemTempDirectory "oglyad-fp" $ \root -> do
        createDirectoryIfMissing True (root </> "anda/a")
        writeFile (root </> "anda/a/a.spec") "Name: a"
        createFileLink "does-not-exist" (root </> "anda/a/broken")
        key <- specFingerprint "m" ".fc44" root "anda/a"
        T.length key `shouldBe` 64
    it "doesn't follow a symlink loop" $
      withSystemTempDirectory "oglyad-loop" $ \root -> do
        createDirectoryIfMissing True (root </> "anda/a")
        writeFile (root </> "anda/a/anda.hcl") "project pkg {}"
        createDirectoryLink ".." (root </> "anda/a/loop")
        dirs <- findPackageDirs root
        map (fmap pdPath) dirs `shouldBe` [Right "anda/a"]
        _ <- specFingerprint "m" ".fc44" root "anda/a"
        pure ()

  describe "parseCheckName" $ do
    it "accepts known checks" $ parseCheckName "build-lag" `shouldBe` Right "build-lag"
    it "rejects typos instead of silently running nothing" $
      parseCheckName "buildlag" `shouldSatisfy` either (const True) (const False)

  describe "failure handling" $ do
    it "turns a missing executable into a Left" $ do
      r <- run "oglyad-definitely-not-a-real-command" []
      r `shouldSatisfy` either (const True) (const False)
    it "turns an unreachable repo into a Left instead of throwing" $ do
      -- port 9 (discard) on loopback: refused immediately, no network needed
      r <- runApp (snapConfig snapshot) (fetchRepo "http://127.0.0.1:9/repo")
      r `shouldSatisfy` either (const True) (const False)

  describe "macroFingerprint" $
    it "changes when ~/.rpmmacros changes" $
      withSystemTempDirectory "oglyad-home" $ \home ->
        withEnv "HOME" home $ withEnv "XDG_CONFIG_HOME" (home </> ".config") $ do
          fpBefore <- macroFingerprint
          writeFile (home </> ".rpmmacros") "%dist .fc99\n"
          fpAfter <- macroFingerprint
          fpAfter `shouldNotBe` fpBefore

  describe "lastCommits" $
    it "reads commit times, including UTC ones git prints with a Z" $
      withSystemTempDirectory "oglyad-git" $ \repo -> do
        let git env args =
              runProcess_ . System.Process.Typed.setEnv (env <> [("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_CONFIG_NOSYSTEM", "1")]) $
                proc "git" (["-C", repo, "-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"] <> args)
            commitAt date dir = do
              createDirectoryIfMissing True (repo </> dir)
              writeFile (repo </> dir </> "anda.hcl") date
              git [] ["add", "-A"]
              git [("GIT_COMMITTER_DATE", date), ("GIT_AUTHOR_DATE", date)] ["commit", "-q", "-m", dir]
        git [] ["init", "-q"]
        commitAt "2026-01-02T03:04:05Z" "anda/a"
        commitAt "2026-02-03T04:05:06+08:00" "anda/b"
        m <- lastCommits repo ["anda/a", "anda/b"]
        Map.lookup "anda/a" m `shouldBe` Just (UTCTime (fromGregorian 2026 1 2) (3 * 3600 + 4 * 60 + 5))
        Map.lookup "anda/b" m `shouldBe` Just (UTCTime (fromGregorian 2026 2 2) (20 * 3600 + 5 * 60 + 6))

  describe "stale check" $ do
    let built daysAgo = Just (RepoBuild (EVR 0 "1.2.0" "1.fc44") (addUTCTime (negate (daysAgo * nominalDay)) now))
    it "is quiet for recent builds" $
      Stale.check snapshot (pkg {pkgRepoBuild = built 10}) `shouldBe` []
    it "notes old builds" $
      severities (Stale.check snapshot (pkg {pkgRepoBuild = built 400})) `shouldBe` [Info]
    it "warns when old and upstream moved on" $
      severities (Stale.check snapshot (pkg {pkgRepoBuild = built 400, pkgUpstreamVer = Just "1.3.0"})) `shouldBe` [Warn]
  where
    nightly =
      "rpm.global(\"commit\", gh_commit(\"OpenTabletDriver/OpenTabletDriver\"));\n\
      \if rpm.changed() {\n\
      \    let v = gh(\"OpenTabletDriver/OpenTabletDriver\");\n\
      \    v.crop(1);\n\
      \    rpm.global(\"ver\", v);\n\
      \}\n"
    isLeft = either (const True) (const False)
    isUnknown = \case Unknown _ -> True; _ -> False

vercmpCase :: (Text, Text, Ordering) -> Spec
vercmpCase (a, b, o) =
  it (T.unpack a <> " vs " <> T.unpack b) $ do
    rpmvercmp a b `shouldBe` o
    rpmvercmp b a `shouldBe` flipOrd o

flipOrd :: Ordering -> Ordering
flipOrd = compare EQ

severities :: [Finding] -> [Severity]
severities = map fSeverity

-- * fixtures

now :: UTCTime
now = UTCTime (fromGregorian 2026 11 9) 0

posix :: Integer -> UTCTime
posix s = addUTCTime (fromIntegral s) (UTCTime (fromGregorian 1970 1 1) 0)

pkg :: Package
pkg =
  Package
    { pkgName = "zed",
      pkgPath = "anda/devs/zed/stable",
      pkgSubrepo = Nothing,
      pkgSpecEVR = Just (EVR 0 "1.2.0" "1.fc44"),
      pkgRepoBuild = Nothing,
      pkgLastCommit = Nothing,
      pkgUpstream = Just (GitHub "zed-industries/zed"),
      pkgUpstreamVer = Nothing,
      pkgUpstreamErr = Nothing,
      pkgRepoChecked = True
    }

snapshot :: Snapshot
snapshot =
  Snapshot
    { snapPackages = Map.singleton (pkgPath pkg) pkg,
      snapFedora = Set.empty,
      snapNow = now,
      snapConfig =
        Config
          { cfgMonorepo = ".",
            cfgTerraRepo = "",
            cfgFedoraRepos = [],
            cfgRelease = 44,
            cfgDist = ".fc44",
            cfgStaleAfter = 180 * nominalDay,
            cfgUpstream = False,
            cfgCacheFor = 0,
            cfgConcurrency = 1,
            cfgGitHubToken = Nothing
          },
      snapHead = Nothing,
      snapIgnoreLabels = Map.empty
    }

report :: [Finding] -> Report
report fs =
  Report
    { rVersion = "0",
      rGenerated = now,
      rMonorepoHead = Just "abc",
      rTerraRepo = "https://repos.fyralabs.com/terra44",
      rUpstream = False,
      rFindings = fs,
      rIgnored = []
    }

primaryFixture :: LBS8.ByteString
primaryFixture =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
  \<metadata xmlns=\"http://linux.duke.edu/metadata/common\" xmlns:rpm=\"http://linux.duke.edu/metadata/rpm\" packages=\"2\">\n\
  \<package type=\"rpm\">\n\
  \  <name>zed</name>\n\
  \  <arch>x86_64</arch>\n\
  \  <version epoch=\"0\" ver=\"0.190.0\" rel=\"1.fc44\"/>\n\
  \  <summary>an editor &amp; more</summary>\n\
  \  <time file=\"1794200100\" build=\"1794200000\"/>\n\
  \  <format>\n\
  \    <rpm:license>GPL-3.0-or-later</rpm:license>\n\
  \    <rpm:sourcerpm>zed-0.190.0-1.fc44.src.rpm</rpm:sourcerpm>\n\
  \    <rpm:provides><rpm:entry name=\"zed\"/></rpm:provides>\n\
  \  </format>\n\
  \</package>\n\
  \<package type=\"rpm\">\n\
  \  <name>zed-cli</name>\n\
  \  <arch>x86_64</arch>\n\
  \  <version epoch=\"0\" ver=\"0.190.0\" rel=\"1.fc44\"/>\n\
  \  <time file=\"1794200100\" build=\"1794200001\"/>\n\
  \  <format><rpm:sourcerpm>zed-0.190.0-1.fc44.src.rpm</rpm:sourcerpm></format>\n\
  \</package>\n\
  \</metadata>\n"

-- | version-ish strings: digits, letters and the separators rpm cares about
newtype V = V Text deriving stock (Show)

instance Arbitrary V where
  arbitrary = V . T.pack <$> listOf (elements "0123456789abz.~^_")
  shrink (V t) = V . T.pack <$> shrink (T.unpack t)

-- | set an environment variable for the duration of an action
withEnv :: String -> String -> IO a -> IO a
withEnv key val act =
  bracket
    (lookupEnv key <* System.Environment.setEnv key val)
    (maybe (unsetEnv key) (System.Environment.setEnv key))
    (const act)
