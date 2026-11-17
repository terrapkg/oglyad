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

data Report = Report
  { rVersion :: Text,
    rGenerated :: UTCTime,
    rReleases :: [ReleaseInfo],
    rDistro :: Text,
    rUpstream :: Bool,
    rFindings :: [Finding],
    rIgnored :: [Finding]
  }

data ReleaseInfo = ReleaseInfo
  { -- | "43", as fReleases spells it
    riName :: Text,
    -- | "f43", for people
    riLabel :: Text,
    -- | Nothing for the working tree
    riRef :: Maybe Text,
    riCommit :: Maybe Text,
    riRepo :: Text
  }

render :: Format -> Severity -> Report -> LBS.ByteString
render fmt minSev r = case fmt of
  Text -> utf8 (renderText minSev r)
  Markdown -> utf8 (renderMarkdown minSev r)
  Json -> renderJson r <> "\n"
  where
    utf8 = LBS.fromStrict . TE.encodeUtf8

renderText :: Severity -> Report -> Text
renderText minSev r@Report {..} =
  T.unlines (map line (shown minSev rFindings) <> ["", summaryLine rFindings rIgnored])
  where
    amb = ambiguousNames rFindings
    line f@Finding {..} =
      T.justifyLeft 6 ' ' (sevText fSeverity)
        <> T.justifyLeft 16 ' ' fCheck
        <> packageLabel amb f
        <> releaseTag r f
        <> ": "
        <> fMessage

summaryLine :: [Finding] -> [Finding] -> Text
summaryLine fs ignored =
  count Error <> " errors, " <> count Warn <> " warnings, " <> count Info <> " info" <> ignoredPart
  where
    count s = tshow (length (filter ((== s) . fSeverity) fs))
    ignoredPart = if null ignored then "" else " (" <> tshow (length ignored) <> " ignored)"

-- | bump on any breaking change to field names or meanings, so scripts reading
-- the json can tell
schemaVersion :: Int
schemaVersion = 2

renderJson :: Report -> LBS.ByteString
renderJson Report {..} =
  encode $
    object
      [ "schema" .= schemaVersion,
        "oglyad" .= rVersion,
        "generated" .= rGenerated,
        "distro" .= rDistro,
        "releases"
          .= [ object
                 [ "release" .= riName,
                   "label" .= riLabel,
                   "ref" .= riRef,
                   "commit" .= riCommit,
                   -- named for Terra, but it's whichever distro's repo was checked
                   "terra_repo" .= riRepo
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

-- | sized for a GitHub job summary, so info is folded away
renderMarkdown :: Severity -> Report -> Text
renderMarkdown minSev r@Report {..} =
  T.unlines $
    [ "# oglyad report: " <> rDistro,
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
                  <> [ "| " <> escapeCell (packageLabel amb f <> releaseTag r f) <> " | `" <> fCheck f <> "` | " <> escapeCell (fMessage f) <> " |"
                     | f <- fs
                     ]
           in if folded
                then ["", "<details><summary>" <> heading <> "</summary>"] <> table <> ["", "</details>"]
                else ["", "## " <> heading] <> table
    escapeCell = T.replace "|" "\\|" . T.replace "\n" " "

shown :: Severity -> [Finding] -> [Finding]
shown minSev =
  sortOn (\f -> (Down (fSeverity f), fCheck f, fPackage f)) . filter ((>= minSev) . fSeverity)

packageLabel :: Set.Set Text -> Finding -> Text
packageLabel ambiguous Finding {..} =
  fPackage <> maybe "" (\path -> if fPackage `Set.member` ambiguous then " (" <> path <> ")" else "") fPath

releaseTag :: Report -> Finding -> Text
releaseTag Report {rReleases} Finding {fReleases}
  | length rReleases <= 1 = ""
  | all (`elem` fReleases) (map riName rReleases) = ""
  | otherwise = " [" <> T.intercalate ", " [riLabel ri | ri <- rReleases, riName ri `elem` fReleases] <> "]"

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
