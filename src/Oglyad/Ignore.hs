module Oglyad.Ignore
  ( IgnoreRule (..),
    Target (..),
    IgnoreFile (..),
    parseIgnoreFile,
    Applied (..),
    applyIgnores,
  )
where

import Data.Aeson (FromJSON (..), Value, eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Types

data Target
  = ByPath Text
  | ByName Text
  deriving stock (Eq, Show)

data IgnoreRule = IgnoreRule
  { irCheck :: Text,
    irTarget :: Target,
    irReason :: Text
  }
  deriving stock (Eq, Show)

newtype IgnoreFile = IgnoreFile [IgnoreRule]
  deriving stock (Eq, Show)

instance FromJSON IgnoreFile where
  parseJSON = withObject "ignore file" $ \o -> IgnoreFile <$> (o .: "ignore" >>= traverse rule)
    where
      rule :: Value -> Parser IgnoreRule
      rule = withObject "ignore entry" $ \o -> do
        check <- o .: "check"
        path <- o .:? "path"
        name <- o .:? "package"
        reason <- o .: "reason"
        target <- case (path, name) of
          (Just p, Nothing) -> pure (ByPath p)
          (Nothing, Just n) -> pure (ByName n)
          _ -> fail ("entry for " <> T.unpack check <> " needs exactly one of \"path\" or \"package\"")
        if T.null (T.strip reason)
          then fail ("entry for " <> T.unpack check <> " has an empty reason. say why it's ok")
          else pure (IgnoreRule check target reason)

parseIgnoreFile :: [Text] -> ByteString -> Either String IgnoreFile
parseIgnoreFile known raw = do
  f@(IgnoreFile rules) <- eitherDecodeStrict raw
  case [irCheck r | r <- rules, irCheck r `notElem` known] of
    [] -> Right f
    bad -> Left ("unknown checks in ignore file: " <> T.unpack (T.intercalate ", " bad))

data Applied = Applied
  { apKept :: [Finding],
    apIgnored :: [Finding],
    -- | kept apart from apProblems because with several releases, an entry is
    -- only unused if it matched nothing in any of them
    apUnused :: [Finding],
    -- | unknown check names in anda.hcl labels
    apProblems :: [Finding]
  }

applyIgnores :: [Text] -> [Text] -> IgnoreFile -> Map FilePath [Text] -> [Finding] -> Applied
applyIgnores knownChecks checksRun (IgnoreFile rules) ignoresByPath findings =
  Applied
    { apKept = kept,
      apIgnored = ignored,
      apUnused = unused,
      apProblems = badLabels
    }
  where
    (ignored, kept) = partition isIgnored findings
    isIgnored f = byLabel f || any (`matches` f) rules
    byLabel f = case fPath f of
      Just path -> fCheck f `elem` Map.findWithDefault [] (T.unpack path) ignoresByPath
      Nothing -> False
    matches r f =
      irCheck r == fCheck f && case irTarget r of
        ByPath p -> fPath f == Just p
        ByName n -> fPackage f == n
    unused =
      [ Finding
          { fCheck = "unused-ignore",
            fPackage = describe (irTarget r),
            fPath = case irTarget r of ByPath p -> Just p; ByName _ -> Nothing,
            fSeverity = Info,
            fMessage = "ignore file entry for " <> irCheck r <> " matches nothing anymore, remove it",
            fReleases = []
          }
      | r <- rules,
        -- an entry for a check that didn't run is idle, not unused
        irCheck r `elem` checksRun,
        not (any (matches r) findings)
      ]
    badLabels =
      [ Finding
          { fCheck = "unused-ignore",
            fPackage = T.pack path,
            fPath = Just (T.pack path),
            fSeverity = Warn,
            fMessage = "oglyad_ignore in anda.hcl names unknown check " <> c,
            fReleases = []
          }
      | (path, cs) <- Map.toList ignoresByPath,
        c <- cs,
        c `notElem` knownChecks
      ]
    describe = \case
      ByPath p -> p
      ByName n -> n
