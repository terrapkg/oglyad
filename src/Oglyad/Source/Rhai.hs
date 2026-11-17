-- | figures out where an update.rhai gets its version from by lexing it,
-- since running it would mean reimplementing andax
module Oglyad.Source.Rhai
  ( parseUpdateRhai,
    parseUpdateRhaiWith,
    Token (..),
    tokenize,
    sourceCall,
    balanced,
  )
where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Oglyad.Types (CrateField (..), UpstreamSource (..))
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

data Token
  = TIdent Text
  | TStr Text
  | TNum Text
  | TSym Char
  | -- | a backtick string Rhai interpolates ${...} in
    TTmpl Text
  deriving stock (Eq, Show)

type Parser = Parsec Void Text

parseUpdateRhai :: Text -> UpstreamSource
parseUpdateRhai = parseUpdateRhaiWith Nothing

-- | a script that sets none of the macros the spec's Version: line uses (see
-- 'Oglyad.Source.Monorepo.versionMacros'), and not ver or version either,
-- doesn't track an upstream version at all
parseUpdateRhaiWith :: Maybe [Text] -> Text -> UpstreamSource
parseUpdateRhaiWith macros src = case tokenize src of
  Nothing -> Unknown (T.strip src)
  Just toks -> case resolveVersion toks of
    Just s -> s
    Nothing
      | hasCommitCall toks -> CommitTracked
      -- a commit from somewhere else, like get(...).json().sha. plenty of
      -- scripts also record the commit of the release they just scraped, so
      -- this only counts when no version is set
      | null (versionExprs toks), not (null (globalExprs ["commit"] toks)) -> CommitTracked
      | Just ms <- macros,
        null (versionExprs toks),
        null (globalExprs ms toks) ->
          NoVersion
      -- no rpm.version we could follow, but the script only ever calls one
      -- source helper, so that's almost certainly it
      | [s] <- nub (allSources toks) -> s
      | otherwise -> Unknown (T.strip src)

tokenize :: Text -> Maybe [Token]
tokenize = either (const Nothing) Just . parse (sc *> many tokenP <* eof) "update.rhai"

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

tokenP :: Parser Token
tokenP =
  L.lexeme sc $
    choice
      [ TStr <$> quoted '"',
        raw,
        charLiteral,
        TIdent <$> takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_'),
        TNum <$> takeWhile1P (Just "number") isDigit,
        TSym <$> satisfy (not . isSpace)
      ]
  where
    -- backtick strings are raw in Rhai, so regexes like `[\d.]+` stay intact.
    -- they interpolate ${...} though, so those get their own token
    raw = (\t -> if "${" `T.isInfixOf` t then TTmpl t else TStr t) <$> (char '`' *> takeWhileP Nothing (/= '`') <* char '`')
    charLiteral = TStr <$> quoted '\''
    -- unknown escapes are kept as-is, the text only needs to be roughly right
    quoted q = T.pack <$> (char q *> manyTill (escaped <|> anySingle) (char q))
    escaped = char '\\' *> fmap unescape anySingle
    unescape = \case
      'n' -> '\n'
      't' -> '\t'
      c -> c

-- | Rhai won't run a script with unpaired brackets, so this catches a broken
-- update.rhai without a real parser
balanced :: [Token] -> Bool
balanced = go []
  where
    go stack = \case
      [] -> null stack
      TSym c : ts
        | Just close <- lookup c pairs -> go (close : stack) ts
        | c `elem` (")]}" :: String) -> case stack of
            s : rest | s == c -> go rest ts
            _ -> False
      _ : ts -> go stack ts
    pairs = [('(', ')'), ('[', ']'), ('{', '}')]

resolveVersion :: [Token] -> Maybe UpstreamSource
resolveVersion toks = listToMaybe (mapMaybe (resolveExpr toks 5) (versionExprs toks))

versionExprs :: [Token] -> [[Token]]
versionExprs toks = go toks <> globalExprs ["ver", "version"] toks
  where
    go = \case
      [] -> []
      TIdent "rpm" : TSym '.' : TIdent "version" : TSym '(' : rest -> takeArgument rest : go rest
      _ : rest -> go rest

globalExprs :: [Text] -> [Token] -> [[Token]]
globalExprs keys = \case
  [] -> []
  TIdent "rpm" : TSym '.' : TIdent f : TSym '(' : TStr key : TSym ',' : rest
    | f `elem` ["global", "define"],
      key `elem` keys ->
        takeArgument rest : globalExprs keys rest
  _ : rest -> globalExprs keys rest

takeArgument :: [Token] -> [Token]
takeArgument = go (0 :: Int)
  where
    go _ [] = []
    go depth (t : ts) = case t of
      TSym c
        | c `elem` ("([{" :: String) -> t : go (depth + 1) ts
        | c `elem` (")]}" :: String) -> if depth == 0 then [] else t : go (depth - 1) ts
        | c == ',' && depth == 0 -> []
      _ -> t : go depth ts

-- | fuel stops let-cycles
resolveExpr :: [Token] -> Int -> [Token] -> Maybe UpstreamSource
resolveExpr toks fuel expr
  | fuel <= 0 = Nothing
  | otherwise = case expr of
      _ | Just (s, _) <- sourceCall expr -> Just s
      -- helpers from the shared andax/*.rhai modules, like bump::madoguchi(...)
      TIdent m : TSym ':' : TSym ':' : TIdent f : TSym '(' : _ -> Just (Derived (m <> "::" <> f))
      TIdent f : TSym '(' : _ | f `elem` derivedHelpers -> Just (Derived f)
      -- kmod packages copy Version from their kmod-common sibling with
      -- sh("cat anda/.../x.spec | grep Version ...")
      TIdent "sh" : TSym '(' : TStr cmd : _
        | "cat " `T.isPrefixOf` T.stripStart cmd,
          ".spec" `T.isInfixOf` cmd ->
            Just (Derived "another spec in the repo")
      -- some other call like find(`regex`, tag, 1)
      TIdent _ : TSym '(' : rest ->
        listToMaybe (mapMaybe (resolveExpr toks (fuel - 1)) (splitArgs rest))
      TIdent v : _ -> do
        bound <- firstLetBinding v toks
        resolveExpr toks (fuel - 1) bound
      _ -> Nothing

splitArgs :: [Token] -> [[Token]]
splitArgs toks = case takeArgument toks of
  [] -> []
  arg ->
    arg : case drop (length arg) toks of
      TSym ',' : rest -> splitArgs rest
      _ -> []

firstLetBinding :: Text -> [Token] -> Maybe [Token]
firstLetBinding v = \case
  [] -> Nothing
  TIdent "let" : TIdent v' : TSym '=' : rest | v == v' -> Just (takeWhile (/= TSym ';') rest)
  _ : rest -> firstLetBinding v rest

sourceCall :: [Token] -> Maybe (UpstreamSource, [Token])
sourceCall = \case
  TIdent f : TSym '(' : TStr a : TSym ',' : TStr b : TSym ')' : rest
    | Just s <- twoArg f a b -> Just (s, rest)
  TIdent f : TSym '(' : TStr a : TSym ')' : rest
    | Just s <- oneArg f a -> Just (s, rest)
  _ -> Nothing
  where
    oneArg f a = case f of
      "gh" -> Just (GitHub a)
      "gh_tag" -> Just (GitHubTag a)
      "gitlab" -> Just (GitLab "gitlab.com" a)
      "gitlab_tag" -> Just (GitLabTag "gitlab.com" a)
      "codeberg" -> Just (Gitea "codeberg.org" a)
      "sourcehut" -> Just (GitTags ("https://git.sr.ht/" <> a))
      "tangled" -> Just (GitTags ("https://tangled.org/" <> a))
      "codeberg_tag" -> Just (GiteaTag "codeberg.org" a)
      "pypi" -> Just (PyPI a)
      "crates" -> Just (Crates MaxStable a)
      "crates_max" -> Just (Crates Max a)
      "crates_newest" -> Just (Crates Newest a)
      "npm" -> Just (Npm a)
      "hackage" -> Just (Hackage a)
      "gems" -> Just (Gems a)
      "hex" -> Just (Hex a)
      "cran" -> Just (Cran a)
      "opam" -> Just (Opam a)
      _ -> Nothing
    twoArg f a b = case f of
      "gitlab" -> Just (GitLab a b)
      "gitlab_tag" -> Just (GitLabTag a b)
      "gitea" -> Just (Gitea a b)
      "gitea_tag" -> Just (GiteaTag a b)
      "forgejo" -> Just (Gitea a b)
      "forgejo_tag" -> Just (GiteaTag a b)
      "ansible_galaxy" -> Just (AnsibleGalaxy a b)
      _ -> Nothing

derivedHelpers :: [Text]
derivedHelpers =
  [ "bodhi",
    "bodhi_vr",
    "madoguchi",
    "alma",
    "nvidia_driver_version",
    "nvidia_legacy_version",
    "nvidia_component_version"
  ]

allSources :: [Token] -> [UpstreamSource]
allSources = \case
  [] -> []
  ts@(_ : rest) -> maybe id ((:) . fst) (sourceCall ts) (allSources rest)

hasCommitCall :: [Token] -> Bool
hasCommitCall = \case
  TIdent f : TSym '(' : _ | "_commit" `T.isSuffixOf` f -> True
  _ : rest -> hasCommitCall rest
  [] -> False
