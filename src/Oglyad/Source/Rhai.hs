-- | figures out where an update.rhai gets its version from, without running it.
--
-- evaluating Rhai would mean reimplementing andax, so instead we:
--
--   1. lex the script into tokens with megaparsec (strings, identifiers,
--      punctuation, comments dropped)
--   2. find what's passed to `rpm.version(...)` or `rpm.global("ver", ...)`
--   3. if that's a variable, follow its `let` binding
--   4. if it's (or starts with) a known andax helper like gh("owner/repo"),
--      that's the source
--
-- versions that follow another distro or package (bodhi, madoguchi, nvidia)
-- become 'Derived'. anything fancier, like custom get_json scraping, ends up as
-- 'Unknown', and the unparsed-rhai check lists those
module Oglyad.Source.Rhai
  ( parseUpdateRhai
  , Token (..)
  , tokenize
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (nub)
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
  deriving stock (Eq, Show)

type Parser = Parsec Void Text

parseUpdateRhai :: Text -> UpstreamSource
parseUpdateRhai src = case tokenize src of
  Nothing -> Unknown (T.strip src)
  Just toks -> case resolveVersion toks of
    Just s -> s
    Nothing
      | hasCommitCall toks -> CommitTracked
      -- no rpm.version we could follow, but the script only ever calls one
      -- source helper, so that's almost certainly it
      | [s] <- nub (allSources toks) -> s
      | otherwise -> Unknown (T.strip src)

-- * lexing

tokenize :: Text -> Maybe [Token]
tokenize = either (const Nothing) Just . parse (sc *> many tokenP <* eof) "update.rhai"

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

tokenP :: Parser Token
tokenP =
  L.lexeme sc $
    choice
      [ TStr <$> quoted '"'
      , TStr <$> raw -- template strings, ${} left as-is
      , TStr <$> quoted '\'' -- char literals
      , TIdent <$> takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_')
      , TNum <$> takeWhile1P (Just "number") isDigit
      , TSym <$> satisfy (not . isSpace)
      ]
  where
    -- backtick strings are raw in Rhai, so regexes like `[\d.]+` stay intact
    raw = char '`' *> takeWhileP Nothing (/= '`') <* char '`'
    -- in quoted strings a backslash escapes whatever follows it. we only care
    -- about the text being roughly right, so unknown escapes are kept as-is
    quoted q = T.pack <$> (char q *> manyTill (escaped <|> anySingle) (char q))
    escaped = char '\\' *> fmap unescape anySingle
    unescape = \case
      'n' -> '\n'
      't' -> '\t'
      c -> c

-- * analysis

-- | follow rpm.version(...) / rpm.global("ver", ...) back to a source helper
resolveVersion :: [Token] -> Maybe UpstreamSource
resolveVersion toks = listToMaybe (mapMaybe (resolveExpr toks 5) (versionExprs toks))

-- | every expression handed to something that sets the package version
versionExprs :: [Token] -> [[Token]]
versionExprs = \case
  [] -> []
  TIdent "rpm" : TSym '.' : TIdent "version" : TSym '(' : rest ->
    argsOf rest <> versionExprs rest
  TIdent "rpm" : TSym '.' : TIdent f : TSym '(' : TStr key : TSym ',' : rest
    | f `elem` ["global", "define"]
    , key `elem` ["ver", "version"] ->
        argsOf' rest <> versionExprs rest
  _ : rest -> versionExprs rest
  where
    -- rpm.version(EXPR)
    argsOf rest = [takeBalanced rest]
    -- rpm.global("ver", EXPR), we're already past the comma
    argsOf' rest = [takeBalanced rest]

-- | tokens up to the ')' that closes the call we're inside
takeBalanced :: [Token] -> [Token]
takeBalanced = go (0 :: Int)
  where
    go _ [] = []
    go depth (t : ts) = case t of
      TSym c
        | c `elem` ("([{" :: String) -> t : go (depth + 1) ts
        | c `elem` (")]}" :: String) -> if depth == 0 then [] else t : go (depth - 1) ts
        | c == ',' && depth == 0 -> []
      _ -> t : go depth ts

-- | what does this expression get its value from? fuel stops let-cycles
resolveExpr :: [Token] -> Int -> [Token] -> Maybe UpstreamSource
resolveExpr toks fuel expr
  | fuel <= 0 = Nothing
  | otherwise = case expr of
      -- a helper call, possibly with a method chain after it
      _ | Just (s, _) <- sourceCall expr -> Just s
      -- a helper from one of the shared andax/*.rhai modules, e.g.
      -- bump::madoguchi(...) or nvidia::nvidia_driver_version()
      TIdent m : TSym ':' : TSym ':' : TIdent f : TSym '(' : _ -> Just (Derived (m <> "::" <> f))
      TIdent f : TSym '(' : _ | f `elem` derivedHelpers -> Just (Derived f)
      -- some other call like find(`regex`, tag, 1): the version comes from
      -- whichever argument we can trace back to a source
      TIdent _ : TSym '(' : rest ->
        listToMaybe (mapMaybe (resolveExpr toks (fuel - 1)) (splitArgs rest))
      -- a variable, possibly with methods: `v` or `v.crop(1)`
      TIdent v : _ -> do
        bound <- letBinding v toks
        resolveExpr toks (fuel - 1) bound
      _ -> Nothing

-- | split the arguments of a call we're inside, up to its closing ')'
splitArgs :: [Token] -> [[Token]]
splitArgs toks = case takeBalanced toks of
  [] -> []
  arg -> arg : case drop (length arg) toks of
    TSym ',' : rest -> splitArgs rest
    _ -> []

-- | the right-hand side of `let v = ...;` (first binding wins)
letBinding :: Text -> [Token] -> Maybe [Token]
letBinding v = \case
  [] -> Nothing
  TIdent "let" : TIdent v' : TSym '=' : rest | v == v' -> Just (takeWhile (/= TSym ';') rest)
  _ : rest -> letBinding v rest

-- | recognize an andax source helper at the start of a token list
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

-- | helpers whose version follows something other than an upstream release
derivedHelpers :: [Text]
derivedHelpers =
  [ "bodhi", "bodhi_vr", "madoguchi", "alma"
  , "nvidia_driver_version", "nvidia_legacy_version", "nvidia_component_version"
  ]

allSources :: [Token] -> [UpstreamSource]
allSources = \case
  [] -> []
  ts@(_ : rest) -> maybe id ((:) . fst) (sourceCall ts) (allSources rest)

-- | does the script call any *_commit helper (gh_commit, gitlab_commit, ...)?
hasCommitCall :: [Token] -> Bool
hasCommitCall = \case
  TIdent f : TSym '(' : _ | "_commit" `T.isSuffixOf` f -> True
  _ : rest -> hasCommitCall rest
  [] -> False
