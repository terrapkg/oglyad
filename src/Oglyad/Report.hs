-- | rendering findings as terminal text, json for scripts, or markdown for
-- GitHub job summaries
module Oglyad.Report
  ( Report (..),
    ReleaseInfo (..),
    Format (..),
    render,
    renderText,
    renderJson,
    renderMarkdown,
  )
where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Oglyad.Types

data Format = Text | Json | Markdown
  deriving stock (Eq, Show)

-- | everything a report needs besides the findings themselves
data Report = Report
  { rVersion :: Text,
    rGenerated :: UTCTime,
    -- | every release this run checked, in order
    rReleases :: [ReleaseInfo],
    rUpstream :: Bool,
    rFindings :: [Finding],
    rIgnored :: [Finding]
  }

-- | what one release was checked against
data ReleaseInfo = ReleaseInfo
  { -- | "43", "rawhide", as findings' fReleases spell it
    riName :: Text,
    -- | "f43", "rawhide", for people
    riLabel :: Text,
    -- | the git ref the specs came from, Nothing for the working tree
    riRef :: Maybe Text,
    riCommit :: Maybe Text,
    riTerraRepo :: Text
  }

render :: Format -> Severity -> Report -> LBS.ByteString
render fmt minSev r = case fmt of
  Text -> utf8 (renderText minSev r)
  Markdown -> utf8 (renderMarkdown minSev r)
  Json -> renderJson r <> "\n"
  where
    utf8 = LBS.fromStrict . TE.encodeUtf8

-- * text

renderText :: Severity -> Report -> Text
renderText minSev r@Report {..} =
  T.unlines (map line (shown minSev rFindings) <> ["", summaryLine rFindings rIgnored])
  where
    amb = ambiguousNames rFindings
    line f@Finding {..} =
      T.justifyLeft 6 ' ' (sevText fSeverity)
        <> T.justifyLeft 16 ' ' fCheck
        <> label amb f
        <> releaseTag r f
        <> ": "
        <> fMessage

summaryLine :: [Finding] -> [Finding] -> Text
summaryLine fs ignored =
  count Error <> " errors, " <> count Warn <> " warnings, " <> count Info <> " info" <> ignoredPart
  where
    count s = tshow (length (filter ((== s) . fSeverity) fs))
    ignoredPart = if null ignored then "" else " (" <> tshow (length ignored) <> " ignored)"

-- * json

-- | a stable envelope. bump schema on any breaking change to field names or
-- meanings, so scripts reading this can tell
renderJson :: Report -> LBS.ByteString
renderJson Report {..} =
  encode $
    object
      [ "schema" .= (2 :: Int),
        "oglyad" .= rVersion,
        "generated" .= rGenerated,
        "releases"
          .= [ object
                 [ "release" .= riName,
                   "label" .= riLabel,
                   "ref" .= riRef,
                   "commit" .= riCommit,
                   "terra_repo" .= riTerraRepo
                 ]
             | ReleaseInfo {..} <- rReleases
             ],
        "upstream" .= rUpstream,
        "summary"
          .= object
            [ "error" .= count Error,
              "warn" .= count Warn,
              "info" .= count Info,
              "ignored" .= length rIgnored
            ],
        "findings" .= map findingJson rFindings,
        "ignored" .= map findingJson rIgnored
      ]
  where
    count s = length (filter ((== s) . fSeverity) rFindings)
    findingJson Finding {..} =
      object
        [ "check" .= fCheck,
          "package" .= fPackage,
          "path" .= fPath,
          "severity" .= toJSON fSeverity,
          "message" .= fMessage,
          "releases" .= fReleases
        ]

-- * markdown

-- | sized for a GitHub job summary: counts first, errors and warnings in
-- full, info folded away
renderMarkdown :: Severity -> Report -> Text
renderMarkdown minSev r@Report {..} =
  T.unlines $
    [ "# oglyad report",
      "",
      T.intercalate
        " · "
        ( [T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" rGenerated)]
            <> [ riLabel <> " " <> maybe "(unknown commit)" (\h -> "`" <> T.take 10 h <> "`") riCommit
               | ReleaseInfo {..} <- rReleases
               ]
            <> [ if rUpstream then "upstream checks on" else "upstream checks off",
                 "oglyad " <> rVersion
               ]
        ),
      "",
      "**" <> summaryLine rFindings rIgnored <> "**",
      "",
      "| check | errors | warnings | info |",
      "|---|---:|---:|---:|"
    ]
      <> [ "| `" <> c <> "` | " <> n Error c <> " | " <> n Warn c <> " | " <> n Info c <> " |"
         | c <- Set.toList (Set.fromList (map fCheck rFindings))
         ]
      <> section Error "Errors" False
      <> section Warn "Warnings" False
      <> (if minSev <= Info then section Info "Info" True else [])
  where
    amb = ambiguousNames rFindings
    n s c = tshow (length [() | f <- rFindings, fSeverity f == s, fCheck f == c])
    section sev title folded =
      case [f | f <- shown minSev rFindings, fSeverity f == sev] of
        [] -> []
        fs ->
          let heading = title <> " (" <> tshow (length fs) <> ")"
              table =
                ["", "| package | check | message |", "|---|---|---|"]
                  <> [ "| " <> cell (label amb f <> releaseTag r f) <> " | `" <> fCheck f <> "` | " <> cell (fMessage f) <> " |"
                     | f <- fs
                     ]
           in if folded
                then ["", "<details><summary>" <> heading <> "</summary>"] <> table <> ["", "</details>"]
                else ["", "## " <> heading] <> table
    -- table cells can't contain raw pipes or newlines
    cell = T.replace "|" "\\|" . T.replace "\n" " "

-- * shared

shown :: Severity -> [Finding] -> [Finding]
shown minSev =
  sortOn (\f -> (Down (fSeverity f), fCheck f, fPackage f)) . filter ((>= minSev) . fSeverity)

-- | the package name, plus its directory when the name belongs to several
label :: Set.Set Text -> Finding -> Text
label ambiguous Finding {..} =
  fPackage <> maybe "" (\path -> if fPackage `Set.member` ambiguous then " (" <> path <> ")" else "") fPath

-- | " [f43, f44]" for a finding that only some of the checked releases
-- have. nothing when it's in all of them, or only one release was checked
releaseTag :: Report -> Finding -> Text
releaseTag Report {rReleases} Finding {fReleases}
  | length rReleases <= 1 = ""
  | all (`elem` fReleases) (map riName rReleases) = ""
  | otherwise = " [" <> T.intercalate ", " [riLabel ri | ri <- rReleases, riName ri `elem` fReleases] <> "]"

-- | names that show up with more than one directory
ambiguousNames :: [Finding] -> Set.Set Text
ambiguousNames fs =
  Map.keysSet . Map.filter ((> 1) . Set.size) $
    Map.fromListWith (<>) [(fPackage f, Set.fromList (maybe [] pure (fPath f))) | f <- fs]

sevText :: Severity -> Text
sevText = \case
  Info -> "info"
  Warn -> "warn"
  Error -> "error"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
