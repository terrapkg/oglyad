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
import Oglyad.Check.UnparsedRhai qualified as UnparsedRhai
import Oglyad.Ignore
import Oglyad.Process (childEnv, run)
import Oglyad.Regex qualified as Re
import Oglyad.Release
import Oglyad.Report (ReleaseInfo (..), Report (..), renderJson, renderMarkdown, renderText)
import Oglyad.Source.Anda (scriptErrors, stripAnsi)
import Oglyad.Source.Monorepo (PkgDir (..), checkoutRef, findPackageDirs, hclString, lastCommits, macroFingerprint, specFingerprint, versionMacros)
import Oglyad.Source.Repodata (RpmEntry (..), decompressAndParse, fetchRepo, latestBySource, parseMetalink, parsePrimary, srpmName)
import Oglyad.Source.Rhai (parseUpdateRhai, parseUpdateRhaiWith)
import Oglyad.Source.RhaiEval (Simulated (..), simulateVersion)
import Oglyad.Source.Upstream (isSemver)
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
        ("0.41.0", "0.41.0^20261031.9e2ed65", EQ),
        -- snapshot suffixes don't stop a respelled version from matching
        ("2.0.0-alpha-1-20250926", "2.0.0~alpha~1~20250926.gitfc62ae7", EQ),
        ("0.4-2", "0.4~2^20260805git.d5d9f60", EQ),
        ("1.3", "1.2^20260101git.abc1234", GT),
        ("1.2", "1.2~rc1^20260101git.abc1234", GT),
        -- a ~git snapshot comes before the release, so it's behind it
        ("2.0", "2.0~git20250101.abc1234", GT)
      ]

  describe "isSemver" $ do
    it "takes plain versions" $ isSemver "v0.17.1" `shouldBe` True
    it "takes prereleases spelled with a tilde" $ isSemver "0.17.0~preview.128" `shouldBe` True
    it "rejects leading zeros, like Jane Street's old scheme" $ isSemver "113.24.00" `shouldBe` False
    it "rejects two-part versions" $ isSemver "1.2" `shouldBe` False

  describe "regex" $ do
    let firstCaps p t = either (const Nothing) (either (const Nothing) id . (`Re.captures` t)) (Re.compile p)
    it "is leftmost-first, like Rust" $
      -- POSIX engines like regex-tdfa take the longest, "ab"
      firstCaps "(a|ab)" "ab" `shouldBe` Just [Just "a", Just "a"]
    it "supports lazy quantifiers" $
      firstCaps "<(.+?)>" "<a><b>" `shouldBe` Just [Just "<a>", Just "a"]
    it "reports groups that didn't take part" $
      firstCaps "(a)|(b)" "b" `shouldBe` Just [Just "b", Nothing, Just "b"]
    it "anchors lines only in multi-line mode" $ do
      firstCaps "(?m)^Version:\\s*(\\S+)$" "Name: x\nVersion: 1.2\n" `shouldBe` Just [Just "Version: 1.2", Just "1.2"]
      firstCaps "^Version" "Name: x\nVersion: 1.2" `shouldBe` Nothing
    it "skips an empty match right after the previous one" $
      (Re.compile "a*" >>= (`Re.capturesAll` "ab")) `shouldBe` Right [[Just "a"], [Just ""]]
    it "expands replacements like Rust" $
      (Re.compile "v(\\d)" >>= \re -> Re.replaceAll re "<$1|${1}x|$1x|$$>" "v1") `shouldBe` Right "<1|1x||$>"
    it "stays linear on patterns that blow up naive backtracking" $
      (Re.compile "^(\\s[.-]?|\\s??(\\D?\\b(a|b)|\\S?\\D??\\.??){2}1??a?){0,}b+(a){2}" >>= (`Re.captures` "v1.2.3-rc1"))
        `shouldBe` Right Nothing
    -- these three came out of fuzzing against the real crate. they only agree
    -- with Rust because the NFA has the same shape and a visited set
    it "takes an empty first iteration of x* like Rust" $
      firstCaps "(a*)?[^a]+?" "v" `shouldBe` Just [Just "v", Just ""]
    it "doesn't loop again after an empty iteration" $
      firstCaps "b??(x*?)*" "xx" `shouldBe` Just [Just "", Just ""]
    it "keeps the last non-empty iteration's captures" $
      firstCaps "(a*){0,}a*" "a" `shouldBe` Just [Just "a", Just "a"]
    it "rejects what it doesn't support" $
      either (const True) (const False) (Re.compile "\\p{Greek}") `shouldBe` True

  describe "simulateVersion" $ do
    let gh = GitHub "a/b"
        sim tag script = simulateVersion gh tag script
    it "passes a plain rpm.version through" $
      sim "v1.2.3" "rpm.version(gh(\"a/b\"));" `shouldBe` Sets "v1.2.3"
    it "skips tags a starts_with guard rejects" $ do
      let script = "let v = gh(\"a/b\");\nif v.starts_with(\"cli-v\") {\n    v.crop(5);\n    rpm.version(v);\n}"
      sim "desktop-v2026.10.4" script `shouldBe` NoUpdate
      sim "cli-v2026.10.4" script `shouldBe` Sets "2026.10.4"
    it "runs else branches" $ do
      let script = "let ver = gh(\"a/b\");\nif ver.starts_with(\"vm-\") {\n\tver.crop(3);\n\trpm.version(ver);\n} else {\n\tprint(`graalvm: ${ver}?`);\n}"
      sim "jdk-25.0.1" script `shouldBe` NoUpdate
    it "compares strings like Rhai" $ do
      let script = "let v = gh(\"a/b\");\nv.crop(1);\nif v < \"5\" {\n  rpm.version(v);\n}"
      sim "v5.4.1" script `shouldBe` NoUpdate
      sim "v4.8.0" script `shouldBe` Sets "4.8.0"
    it "applies pop and replace, preferring rpm.version over the ver global" $ do
      let script = "let v = gh(\"a/b\");\nv.crop(1);\nrpm.global(\"ver\", v);\nif v.ends_with(\"-release\") {\n  v.pop(8);\n}\nv.replace(\"-\", \"~\");\nrpm.version(v);"
      sim "v0.0.17-beta-release" script `shouldBe` Sets "0.0.17~beta"
    it "treats rpm.changed() as true and follows truncate" $ do
      let script = "rpm.global(\"commit\", gh_commit(\"a/b\"));\nif rpm.changed() {\n    rpm.release();\n    let v = gh(\"a/b\");\n    v.truncate(6);\n    v.crop(1);\n    rpm.global(\"ver\", v);\n}"
      sim "v1.0.1-iot-arl-twl-asl-v6.12" script `shouldBe` Sets "1.0.1"
    it "gives up on scripts that rewrite the spec through rpm.f" $
      sim "v1.2.3" "let v = gh(\"a/b\");\nrpm.f = sub(`(?m)^Version:.*$`, \"Version: \" + v, rpm.f);" `shouldBe` CantTell
    it "knows a script that never sets Version won't update" $
      sim "vUDK2018" "let v = gh(\"a/b\");\nrpm.global(\"release_tag\", v);\nv.crop(3);\nrpm.global(\"shortcommit\", v);" `shouldBe` NoUpdate
    it "stops at terminate()" $
      sim "0.4.0" "terminate();\nrpm.version(gh(\"a/b\"));" `shouldBe` NoUpdate
    it "uses find() as a guard inside try" $ do
      let script = "let v = gh(\"a/b\");\ntry {\n\tfind(\"(?m)^v[\\\\d.]+$\", v, 0);\n\trpm.version(v);\n}\ncatch {}"
      sim "v1.3.7-windows-alpha" script `shouldBe` NoUpdate
      sim "v1.3.8" script `shouldBe` Sets "v1.3.8"
    it "extracts with find() groups" $
      sim "Ghidra_11.4_build" "let tag = gh(\"a/b\");\nlet v = find(`Ghidra_([\\d.]+)_build`, tag, 1);\nrpm.version(v);" `shouldBe` Sets "11.4"
    it "counts find_all() matches in method form" $ do
      let script = "let v = gh(\"a/b\");\nif `[\\d.]+-beta\\.\\d+`.find_all(v).len == 0 {\n  rpm.version(v);\n}"
      sim "1.2.3-beta.4" script `shouldBe` NoUpdate
      sim "1.2.3" script `shouldBe` Sets "1.2.3"
    it "rewrites with sub() and indexes find_all() results" $ do
      sim "release-1_2_3" "let v = sub(`_`, \".\", gh(\"a/b\"));\nrpm.version(sub(`^release-`, \"\", v));" `shouldBe` Sets "1.2.3"
      sim "v1.2 v1.3" "let m = find_all(`v([\\d.]+)`, gh(\"a/b\"));\nrpm.version(m[-1][1]);" `shouldBe` Sets "1.3"
    it "fills in template strings" $
      sim "pkg-1.2" "let name = \"pkg\";\nlet t = gh(\"a/b\");\nrpm.version(find(`${name}-([\\d.]+)`, t, 1));" `shouldBe` Sets "1.2"
    it "gives up when find() throws with nothing to catch it" $
      sim "nope" "rpm.version(find(`v([\\d.]+)`, gh(\"a/b\"), 1));" `shouldBe` CantTell
    it "gives up on calls it can't model" $
      sim "1.2" "let v = gh(\"a/b\");\nrpm.version(mystery(v));" `shouldBe` CantTell
    it "gives up on scripts with unbalanced brackets" $
      sim "0.2.1" "if rpm.changed() {\n    let v = gh(\"a/b\"));\n    v.crop(1);\n    rpm.global(\"ver\", v);\n}" `shouldBe` CantTell
    it "ignores other helpers that don't feed Version" $
      sim "v0.14.1" "rpm.version(gh(\"a/b\"));\nif rpm.changed() {\n    rpm.global(\"drawio_ver\", gh(\"c/d\").crop(1));\n}" `shouldBe` Sets "v0.14.1"

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
    it "reads bare values like nightly = 1" $ hclString "nightly" "labels {\n  nightly = 2 # slot\n}\n" `shouldBe` Just "2"

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
    it "follows kmod-common siblings instead of calling them unparsed" $
      parseUpdateRhai "let c = sh(\"cat anda/system/x/kmod-common/x.spec | grep 'Version: ' | sed -E 's/Version: +//'\", #{});\nc.trim();\nrpm.version(c);"
        `shouldBe` Derived "another spec in the repo"
    it "calls a commit set from get() commit tracking" $
      parseUpdateRhai "let json = get(\"https://api.github.com/repos/a/b/commits/main\").json();\nrpm.global(\"commit\", json.sha);"
        `shouldBe` CommitTracked
    it "doesn't call a scraped version commit tracking because it records the commit" $
      parseUpdateRhai "let r = get(\"https://api.github.com/repos/a/b/tags\").json_arr();\nrpm.version(r[0].name);\nif rpm.changed() {\n  rpm.global(\"commit\", r[0].commit.sha);\n}"
        `shouldSatisfy` isUnknown
    it "knows a release-bump-only script doesn't set a version" $
      parseUpdateRhaiWith (Just ["fedora", "rhel"]) "import \"andax/bump_extras.rhai\" as bump;\nopen_file(\"RELEASE.txt\", \"w\").write(bump::as_bodhi_ver(labels.branch));\nrpm.release();"
        `shouldBe` NoVersion
    it "counts globals the Version: line uses as setting the version" $ do
      let ente = "let tags = git_tags(\"https://github.com/ente-io/ente\").filter(|t| t.starts_with(\"cli-v\"));\nrpm.global(\"tag\", tags[tags.len - 1]);"
      parseUpdateRhaiWith (Just ["tag"]) ente `shouldSatisfy` isUnknown
      parseUpdateRhaiWith (Just ["other"]) ente `shouldBe` NoVersion
    it "doesn't guess NoVersion without the spec's Version: line" $
      parseUpdateRhai "rpm.release();" `shouldSatisfy` isUnknown

  describe "anda logs" $ do
    it "pulls the reason out of a script exception" $
      scriptErrors
        "  2026-11-09T22:25:38.348511Z ERROR andax::run: Script Exception \8212\8212 lib/libfreeaptx\n\
        \   \9500\9472\9552 Rhai: Syntax error: Expecting ';' to terminate this statement (line 5, position 39)\n\
        \    in andax::run::traceback with proj=\"lib/libfreeaptx\"\n"
        `shouldBe` ["Rhai: Syntax error: Expecting ';' to terminate this statement (line 5, position 39)"]
    it "finds nothing in a clean log" $
      scriptErrors "  2026-11-09T22:25:34Z DEBUG anda_config::config: Reading hcl file: \"anda.hcl\"\n" `shouldBe` []
    it "strips color codes" $
      stripAnsi "   0: \ESC[91mProject not found: x\ESC[0m" `shouldBe` "   0: Project not found: x"

  describe "unparsed-rhai with anda" $
    it "stays quiet once anda answered for the script" $ do
      let unknown = pkg {pkgUpstream = Just (Unknown "let x = get(\"u\");")}
      map fCheck (UnparsedRhai.check snapshot unknown) `shouldBe` ["unparsed-rhai"]
      UnparsedRhai.check snapshot unknown {pkgUpstreamVer = Just "1.3.0"} `shouldBe` []

  describe "versionMacros" $ do
    it "finds %name, %{name} and %{?name}" $
      versionMacros "Name: x\nVersion:  %{?fedora:%{fedora}}%{?rhel:%{rhel}}\n" `shouldBe` Just ["fedora", "fedora", "rhel", "rhel"]
    it "finds macros inside shell expansions" $
      versionMacros "%global tag cli-v0.3.0\nVersion: %(echo %tag | sed 's/^cli-v//')\n" `shouldBe` Just ["tag"]
    it "is Nothing without a Version: line" $
      versionMacros "Name: x\n" `shouldBe` Nothing

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
    it "doesn't hold nightlies to the latest stable release" $ do
      let snap = pkg {pkgNightly = True, pkgSpecEVR = Just (EVR 0 "0.41.0^20261031.9e2ed65" "1.fc44"), pkgUpstreamVer = Just "v0.42.0"}
      Drift.check snapshot snap `shouldBe` []
      Drift.drift snap `shouldBe` Nothing
    it "still flags a mangled version on a nightly" $
      severities (Drift.check snapshot (pkg {pkgNightly = True, pkgUpstreamVer = Just ".0.1"})) `shouldBe` [Warn]
    it "warns when update.rhai mangles the version" $
      map fMessage (Drift.check snapshot (pkg {pkgSpecEVR = Just (EVR 0 ".0.1^20261029.git.8ce4f26" "1.fc44"), pkgUpstreamVer = Just ".0.1"}))
        `shouldBe` ["update.rhai would set the version to \".0.1\", which doesn't start with a letter or digit"]

  describe "unparsed-rhai check"
    $ it "flags scripts with unbalanced brackets"
    $ severities (UnparsedRhai.check snapshot (pkg {pkgRhai = Just "let v = gh_tag(\"a/b\"));\nrpm.version(v);"})) `shouldBe` [Warn]

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
    let f path = Finding "build-lag" "zed" (Just path) Warn "x" []
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
      KM.lookup "schema" o `shouldBe` Just (Number 2)
      KM.lookup "summary" o `shouldSatisfy` maybe False (\case Object s -> KM.lookup "warn" s == Just (Number 1); _ -> False)
      KM.lookup "findings" o `shouldSatisfy` maybe False (\case Array a -> length a == 1; _ -> False)
    it "escapes pipes in markdown tables" $
      renderMarkdown Info (report [(f "anda/a") {fMessage = "a|b"}]) `shouldSatisfy` T.isInfixOf "a\\|b"
    it "folds info findings away in markdown" $
      renderMarkdown Info (report [(f "anda/a") {fSeverity = Info}]) `shouldSatisfy` T.isInfixOf "<details><summary>Info (1)"

  describe "ignores" $ do
    let f check path name = Finding check name (Just path) Warn "x" []
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
      (apKept, apIgnored, apUnused, apProblems) `shouldBe` ([lag], [dup], [], [])
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
      map fCheck apUnused `shouldBe` ["unused-ignore"]
    it "doesn't call entries unused when their check didn't run" $ do
      let Applied {..} = applyIgnores checkNames ["build-lag"] (file [rule "stale" (ByName "gone")]) Map.empty [lag]
      apUnused `shouldBe` []
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
        dirs <- findPackageDirs root "anda"
        map (fmap pdPath) dirs `shouldBe` [Right "anda/a"]
        _ <- specFingerprint "m" ".fc44" root "anda/a"
        pure ()

  describe "nightly labels" $
    it "treats nightly and quaterdaily as branch-head builds, not weekly" $
      withSystemTempDirectory "oglyad-labels" $ \root -> do
        let pkgDir name key = do
              createDirectoryIfMissing True (root </> "anda" </> name)
              writeFile (root </> "anda" </> name </> "anda.hcl") ("project pkg {\n  labels {\n    " <> key <> " = 1\n  }\n}\n")
        pkgDir "n" "nightly"
        pkgDir "q" "quaterdaily"
        pkgDir "w" "weekly"
        dirs <- findPackageDirs root "anda"
        [(pdPath d, pdNightly d) | Right d <- dirs] `shouldBe` [("anda/n", True), ("anda/q", True), ("anda/w", False)]

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
    it "turns an unreachable metalink into a Left too" $ do
      r <- runApp (snapConfig snapshot) (fetchRepo "http://127.0.0.1:9/metalink?repo=terra44")
      r `shouldSatisfy` either (const True) (const False)

  describe "metalinks"
    $ it "reads checksums, alternates included, and mirrors by preference"
    $ parseMetalink metalinkFixture
      `shouldBe` Right (["aaa", "bbb"], ["https://fast.example/terra44/repodata/repomd.xml", "https://repos.fyralabs.com/terra44/repodata/repomd.xml"])

  describe "macroFingerprint" $
    it "changes when ~/.rpmmacros changes" $
      withSystemTempDirectory "oglyad-home" $ \home ->
        withEnv "HOME" home $ withEnv "XDG_CONFIG_HOME" (home </> ".config") $ do
          fpBefore <- macroFingerprint
          writeFile (home </> ".rpmmacros") "%dist .fc99\n"
          fpAfter <- macroFingerprint
          fpAfter `shouldNotBe` fpBefore

  describe "releases" $ do
    it "parses --release spellings" $
      map parseReleaseArg ["43", "f44", "rawhide", "frawhide", "el10", ""]
        `shouldBe` [Right (NumberedArg 43), Right (NumberedArg 44), Right RawhideArg, Right RawhideArg, Left "unknown release \"el10\", expected a number like 43, f43, or rawhide", Left "unknown release \"\", expected a number like 43, f43, or rawhide"]
    it "spells rawhide's dist, branch and repos the way Terra does" $ do
      let raw = Release "rawhide" 46
      (releaseDist terra raw, releaseBranch terra raw, repoName terra raw, releaseLabel raw) `shouldBe` (".fcrawhide", "frawhide", "terrarawhide", "rawhide")
      fedoraReposFor raw `shouldBe` ["https://dl.fedoraproject.org/pub/fedora/linux/development/rawhide/Everything/x86_64/os"]
      length (fedoraReposFor (Release "43" 43)) `shouldBe` 2
    it "spells releases the way Ultramarine does" $ do
      let r44 = Release "44" 44
      (releaseDist ultramarine r44, releaseBranch ultramarine r44, repoName ultramarine r44) `shouldBe` (".um44", "um44", "um44")
      lookupDistro "ultramarine" `shouldBe` Right ultramarine
      lookupDistro "umbrella" `shouldSatisfy` either (const True) (const False)
    it "fills in repo url templates, subrepos included" $ do
      let cfg url = (snapConfig snapshot) {cfgRepoName = "terra44", cfgRepoUrl = repoUrlFor url (Release "44" 44)}
      repoUrl (cfg defaultRepoUrl) (Just "extras") `shouldBe` "https://tetsudou.fyralabs.com/metalink?repo=terra44-extras&arch=x86_64"
      repoUrl (cfg "https://example.com/um{release}/x86_64") Nothing `shouldBe` "https://example.com/um44/x86_64"
      repoUrl (cfg "https://example.com/terra44") (Just "extras") `shouldBe` "https://example.com/terra44-extras"
    it "takes Fedora releases from Bodhi, rawhide last" $
      parseBodhiReleases bodhiFixture `shouldBe` Right [Release "43" 43, Release "44" 44, Release "45" 45, Release "rawhide" 46]
    it "complains when Bodhi lists nothing useful" $
      parseBodhiReleases "{\"releases\": []}" `shouldSatisfy` either (const True) (const False)

  describe "mergeFindings" $ do
    let r43 = Release "43" 43
        r44 = Release "44" 44
        lag dist = Finding "build-lag" "zed" (Just "anda/zed") Error ("never published (spec is at 1.2-1" <> dist <> ")") []
        drift = Finding "drift" "zed" (Just "anda/zed") Warn "upstream has 1.3, spec is at 1.2" []
    it "merges findings that only differ by dist" $
      mergeFindings terra [(r43, [lag ".fc43"]), (r44, [lag ".fc44"])]
        `shouldBe` [(lag "%{dist}") {fReleases = ["43", "44"]}]
    it "merges by the distro's own dist" $
      mergeFindings ultramarine [(r43, [lag ".um43"]), (r44, [lag ".um44"])]
        `shouldBe` [(lag "%{dist}") {fReleases = ["43", "44"]}]
    it "keeps the real message for a single release" $
      mergeFindings terra [(r43, [drift]), (r44, [lag ".fc44"])]
        `shouldBe` [drift {fReleases = ["43"]}, (lag ".fc44") {fReleases = ["44"]}]
    it "tags findings with only the releases that have them" $
      map fReleases (mergeFindings terra [(r43, [drift]), (r44, [drift, lag ".fc44"])]) `shouldBe` [["43", "44"], ["44"]]

  describe "release tags in reports" $ do
    let infos = [ReleaseInfo n ("f" <> n) Nothing Nothing "" | n <- ["43", "44"]]
        f rels = Finding "drift" "zed" (Just "anda/zed") Warn "x" rels
        two fs = (report fs) {rReleases = infos}
    it "says which releases a finding is in when it isn't all of them" $
      renderText Info (two [f ["44"]]) `shouldSatisfy` T.isInfixOf "zed [f44]: x"
    it "says nothing when it's in every release" $
      renderText Info (two [f ["43", "44"]]) `shouldSatisfy` T.isInfixOf "zed: x"

  describe "checkoutRef" $
    it "extracts a branch without touching the checkout, once per commit" $
      withSystemTempDirectory "oglyad-ref" $ \tmp -> do
        let repo = tmp </> "repo"
            cache = tmp </> "cache"
            git args =
              runProcess_ . System.Process.Typed.setEnv [("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_CONFIG_NOSYSTEM", "1")] $
                proc "git" (["-C", repo, "-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false"] <> args)
            commitFile body = do
              writeFile (repo </> "anda" </> "a" </> "anda.hcl") body
              git ["add", "-A"]
              git ["commit", "-q", "-m", body]
        createDirectoryIfMissing True (repo </> "anda" </> "a")
        git ["init", "-q"]
        commitFile "old"
        git ["branch", "f43"]
        commitFile "new"
        old <- lookupEnv "XDG_CACHE_HOME"
        System.Environment.setEnv "XDG_CACHE_HOME" cache
        r1 <- checkoutRef repo "f43"
        r2 <- checkoutRef repo "f43"
        missing <- checkoutRef repo "f99"
        maybe (unsetEnv "XDG_CACHE_HOME") (System.Environment.setEnv "XDG_CACHE_HOME") old
        case r1 of
          Left e -> expectationFailure (T.unpack e)
          Right (dir, _) -> do
            readFile (dir </> "anda" </> "a" </> "anda.hcl") >>= (`shouldBe` "old")
            readFile (repo </> "anda" </> "a" </> "anda.hcl") >>= (`shouldBe` "new")
        r2 `shouldBe` r1
        missing `shouldSatisfy` either (const True) (const False)

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
        m <- lastCommits repo "anda" "HEAD" ["anda/a", "anda/b"]
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
      pkgNightly = False,
      pkgSpecPath = Just "anda/devs/zed/stable/zed.spec",
      pkgFingerprint = "",
      pkgSpecEVR = Just (EVR 0 "1.2.0" "1.fc44"),
      pkgRepoBuild = Nothing,
      pkgLastCommit = Nothing,
      pkgUpstream = Just (GitHub "zed-industries/zed"),
      pkgRhai = Nothing,
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
            cfgRoot = "anda",
            cfgSpecRef = Nothing,
            cfgBranch = "f44",
            cfgRepoName = "terra44",
            cfgRepoUrl = defaultRepoUrl,
            cfgFedoraRepos = [],
            cfgRelease = 44,
            cfgDist = ".fc44",
            cfgStaleAfter = 180 * nominalDay,
            cfgUpstream = False,
            cfgCacheFor = 0,
            cfgRunStart = now,
            cfgConcurrency = 1,
            cfgGitHubToken = Nothing
          },
      snapHead = Nothing,
      snapIgnoreLabels = Map.empty
    }

-- | trimmed from tetsudou's answer for terra44, with a second mirror
metalinkFixture :: LBS8.ByteString
metalinkFixture =
  "<?xml version=\"1.0\" encoding=\"utf-8\"?><metalink version=\"3.0\" xmlns=\"http://www.metalinker.org/\" xmlns:mm0=\"http://fedorahosted.org/mirrormanager\" type=\"dynamic\" generator=\"tetsudou\"><files><file name=\"repomd.xml\">\
  \<verification><hash type=\"md5\">x</hash><hash type=\"sha256\">aaa</hash></verification>\
  \<mm0:alternates><mm0:alternate><verification><hash type=\"sha256\">bbb</hash></verification></mm0:alternate></mm0:alternates>\
  \<resources maxconnections=\"1\">\
  \<url type=\"https\" protocol=\"https\" location=\"DE\" preference=\"100\">https://repos.fyralabs.com/terra44/repodata/repomd.xml</url>\
  \<url type=\"https\" protocol=\"https\" location=\"US\" preference=\"200\">https://fast.example/terra44/repodata/repomd.xml</url>\
  \</resources></file></files></metalink>"

bodhiFixture :: LBS8.ByteString
bodhiFixture =
  "{\"releases\": [\
  \{\"name\": \"F46\", \"version\": \"46\", \"branch\": \"rawhide\", \"id_prefix\": \"FEDORA\", \"state\": \"pending\"},\
  \{\"name\": \"F44\", \"version\": \"44\", \"branch\": \"f44\", \"id_prefix\": \"FEDORA\", \"state\": \"current\"},\
  \{\"name\": \"F45C\", \"version\": \"45\", \"branch\": \"f45\", \"id_prefix\": \"FEDORA-CONTAINER\", \"state\": \"current\"},\
  \{\"name\": \"ELN\", \"version\": \"eln\", \"branch\": \"eln\", \"id_prefix\": \"FEDORA\", \"state\": \"pending\"},\
  \{\"name\": \"F43\", \"version\": \"43\", \"branch\": \"f43\", \"id_prefix\": \"FEDORA\", \"state\": \"current\"},\
  \{\"name\": \"F45\", \"version\": \"45\", \"branch\": \"f45\", \"id_prefix\": \"FEDORA\", \"state\": \"current\"}\
  \]}"

report :: [Finding] -> Report
report fs =
  Report
    { rVersion = "0",
      rGenerated = now,
      rReleases = [ReleaseInfo "44" "f44" (Just "origin/f44") (Just "abc") "https://repos.fyralabs.com/terra44"],
      rDistro = "terra",
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
