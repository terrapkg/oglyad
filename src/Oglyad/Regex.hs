-- | the dialect of Rust's regex crate that andax's find(), find_all() and
-- sub() use (andax/fns/kokoro.rs in FyraLabs/anda).
--
-- it mirrors Rust's BoundedBacktracker: a Thompson NFA shaped like
-- regex-automata builds it, backtracked with a visited set of (state,
-- position) pairs. the visited set also decides which captures win when a
-- repeated group can match empty, so the NFA shape has to match Rust's to
-- get the same answers. it was checked against the real crate on every regex
-- in terrapkg/packages and ~300k fuzzed ones.
--
-- anything the parser doesn't handle, like \p{...}, nested classes or class
-- operations, is a compile error, which callers treat as "can't tell"
module Oglyad.Regex
  ( Regex,
    compile,
    captures,
    capturesAll,
    replaceAll,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.ST (runST)
import Data.Char (GeneralCategory (..), generalCategory, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isPunctuation, isSpace, isSymbol, toLower, toUpper)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Text.Read (readMaybe)

type Multiline = Bool

type WantBoundary = Bool

type Greedy = Bool

data Node
  = One (Char -> Bool)
  | LineStart Multiline
  | LineEnd Multiline
  | WordBoundary WantBoundary
  | Group (Maybe Int) Node
  | Alt [Node]
  | Seq [Node]
  | Repeat Int (Maybe Int) Greedy Node

data Regex = Regex
  { reProg :: Prog,
    reGroups :: Int,
    reNames :: [(Text, Int)]
  }

data Flags = Flags {fI, fM, fS :: Bool}

data ParseState = ParseState
  { stRest :: String,
    stGroupCount :: Int,
    stGroupNames :: [(Text, Int)]
  }

type Parse a = ParseState -> Either Text (a, ParseState)

compile :: Text -> Either Text Regex
compile src = do
  (node, st) <- alternation (Flags False False False) (ParseState (T.unpack src) 0 [])
  case stRest st of
    [] -> Right (Regex (toProg (stGroupCount st) node) (stGroupCount st) (stGroupNames st))
    c : _ -> Left ("unexpected " <> T.singleton c)

-- | a|b|c, up to a ')' or the end. (?flags) partway through applies to the
-- rest of the group, alternatives included, like Rust
alternation :: Flags -> Parse Node
alternation flags st0 = go flags st0 []
  where
    go fl st acc = do
      ((node, fl'), st') <- sequence' fl st []
      case stRest st' of
        '|' : rest -> go fl' st' {stRest = rest} (node : acc)
        _ -> Right (alt (reverse (node : acc)), st')
    alt [n] = n
    alt ns = Alt ns

sequence' :: Flags -> ParseState -> [Node] -> Either Text ((Node, Flags), ParseState)
sequence' fl st acc = case stRest st of
  [] -> done
  '|' : _ -> done
  ')' : _ -> done
  '(' : '?' : rest
    | (fs, ')' : rest') <- span (`elem` ("imsxUR-" :: String)) rest,
      not (null fs) -> do
        fl' <- setFlags fl fs
        sequence' fl' st {stRest = rest'} acc
  _ -> do
    (atom, st1) <- atomP fl st
    (node, st2) <- quantifier atom st1
    sequence' fl st2 (node : acc)
  where
    done = Right ((Seq (reverse acc), fl), st)

setFlags :: Flags -> String -> Either Text Flags
setFlags fl0 = go True fl0
  where
    go _ fl [] = Right fl
    go on fl (c : cs) = case c of
      '-' -> go False fl cs
      'i' -> go on fl {fI = on} cs
      'm' -> go on fl {fM = on} cs
      's' -> go on fl {fS = on} cs
      _ -> Left ("unsupported flag " <> T.singleton c)

quantifier :: Node -> Parse Node
quantifier atom st = case stRest st of
  '*' : rest -> lazy 0 Nothing rest
  '+' : rest -> lazy 1 Nothing rest
  '?' : rest -> lazy 0 (Just 1) rest
  '{' : rest
    | (a, rest1) <- span isDigit rest,
      not (null a) ->
        case rest1 of
          '}' : rest2 -> bounded a (Just a) rest2
          ',' : '}' : rest2 -> bounded a Nothing rest2
          ',' : rest2
            | (b, '}' : rest3) <- span isDigit rest2, not (null b) -> bounded a (Just b) rest3
          _ -> Left "bad repetition"
  _ -> Right (atom, st)
  where
    bounded a b rest = case (readMaybe a, traverse readMaybe b) of
      (Just lo, Just hi) | maybe True (>= lo) hi -> lazy lo hi rest
      _ -> Left "bad repetition"
    lazy lo hi = \case
      '?' : rest -> Right (Repeat lo hi False atom, st {stRest = rest})
      rest -> Right (Repeat lo hi True atom, st {stRest = rest})

atomP :: Flags -> Parse Node
atomP fl st = case stRest st of
  '(' : '?' : ':' : rest -> group Nothing st {stRest = rest}
  '(' : '?' : 'P' : '<' : rest -> named rest
  '(' : '?' : '<' : rest -> named rest
  '(' : '?' : _ -> Left "unsupported group"
  '(' : rest ->
    let n = stGroupCount st + 1
     in group (Just n) st {stRest = rest, stGroupCount = n}
  '[' : rest -> do
    (p, rest') <- bracketClass fl rest
    Right (One p, st {stRest = rest'})
  '.' : rest -> Right (One (\c -> fS fl || c /= '\n'), st {stRest = rest})
  '^' : rest -> Right (LineStart (fM fl), st {stRest = rest})
  '$' : rest -> Right (LineEnd (fM fl), st {stRest = rest})
  '\\' : rest -> do
    (node, rest') <- escape fl rest
    Right (node, st {stRest = rest'})
  c : rest
    | c `elem` ("*+?{" :: String) -> Left "repetition with nothing to repeat"
    | otherwise -> Right (One (charEq fl c), st {stRest = rest})
  [] -> Left "unexpected end"
  where
    group idx s = do
      (inner, s') <- alternation fl s
      case stRest s' of
        ')' : rest -> Right (Group idx inner, s' {stRest = rest})
        _ -> Left "unclosed group"
    named rest = case span (\c -> isAlphaNum c || c == '_') rest of
      (name, '>' : rest')
        | not (null name) ->
            let n = stGroupCount st + 1
             in group (Just n) st {stRest = rest', stGroupCount = n, stGroupNames = (T.pack name, n) : stGroupNames st}
      _ -> Left "bad group name"

charEq :: Flags -> Char -> Char -> Bool
charEq fl c
  | fI fl = \x -> toLower x == toLower c || toUpper x == toUpper c
  | otherwise = (== c)

escape :: Flags -> String -> Either Text (Node, String)
escape fl = \case
  'b' : rest -> Right (WordBoundary True, rest)
  'B' : rest -> Right (WordBoundary False, rest)
  'A' : rest -> Right (LineStart False, rest)
  'z' : rest -> Right (LineEnd False, rest)
  s -> do
    (p, rest) <- classEscape fl s
    Right (One p, rest)

classEscape :: Flags -> String -> Either Text (Char -> Bool, String)
classEscape fl = \case
  'd' : rest -> Right (isUnicodeDigit, rest)
  'D' : rest -> Right (not . isUnicodeDigit, rest)
  'w' : rest -> Right (isWord, rest)
  'W' : rest -> Right (not . isWord, rest)
  's' : rest -> Right (isSpace, rest)
  'S' : rest -> Right (not . isSpace, rest)
  'n' : rest -> lit '\n' rest
  't' : rest -> lit '\t' rest
  'r' : rest -> lit '\r' rest
  'f' : rest -> lit '\f' rest
  'v' : rest -> lit '\v' rest
  'x' : a : b : rest | isHexDigit a && isHexDigit b, Just n <- readMaybe ("0x" <> [a, b]) -> lit (toEnum n) rest
  c : rest
    | isPunctuation c || isSymbol c || c == ' ' -> lit c rest
  _ -> Left "unsupported escape"
  where
    lit c rest = Right (charEq fl c, rest)

isUnicodeDigit :: Char -> Bool
isUnicodeDigit c = generalCategory c == DecimalNumber

isWord :: Char -> Bool
isWord c = isAlphaNum c || c == '_'

bracketClass :: Flags -> String -> Either Text (Char -> Bool, String)
bracketClass fl s0 = do
  let (negated, s1) = case s0 of
        '^' : r -> (True, r)
        r -> (False, r)
  -- a ']' right at the start is a literal
  (items, rest) <- case s1 of
    ']' : r -> go [charEq fl ']'] r
    r -> go [] r
  let p c = any ($ c) items
  Right (if negated then not . p else p, rest)
  where
    go acc = \case
      ']' : rest -> Right (acc, rest)
      '[' : ':' : rest
        | (name, ':' : ']' : rest') <- span isAsciiLower rest -> do
            p <- posix name
            go (p : acc) rest'
      '[' : _ -> Left "nested classes aren't supported"
      '&' : '&' : _ -> Left "class operations aren't supported"
      '-' : '-' : _ -> Left "class operations aren't supported"
      '~' : '~' : _ -> Left "class operations aren't supported"
      s -> do
        (lo, rest) <- classItem s
        case (lo, rest) of
          (ItemChar c, '-' : rest') | take 1 rest' /= "]" -> do
            (hi, rest'') <- classItem rest'
            case hi of
              ItemChar d | c <= d -> go (range c d : acc) rest''
              _ -> Left "bad class range"
          (ItemChar c, _) -> go (charEq fl c : acc) rest
          (ItemSet p, _) -> go (p : acc) rest
    classItem = \case
      '\\' : rest -> case rest of
        c : rest' | isPunctuation c || isSymbol c || c == ' ' -> Right (ItemChar c, rest')
        'n' : rest' -> Right (ItemChar '\n', rest')
        't' : rest' -> Right (ItemChar '\t', rest')
        _ -> do
          (p, rest') <- classEscape fl rest
          Right (ItemSet p, rest')
      c : rest -> Right (ItemChar c, rest)
      [] -> Left "unclosed class"
    range lo hi
      | fI fl = \c -> any (\x -> lo <= x && x <= hi) [c, toLower c, toUpper c]
      | otherwise = \c -> lo <= c && c <= hi
    posix = \case
      "alnum" -> Right isAlphaNumAscii
      "alpha" -> Right (\c -> isAsciiLower c || isAsciiUpper c)
      "digit" -> Right isDigit
      "lower" -> Right isAsciiLower
      "upper" -> Right isAsciiUpper
      "space" -> Right (`elem` (" \t\n\r\f\v" :: String))
      "xdigit" -> Right isHexDigit
      "punct" -> Right (\c -> c < '\x80' && (isPunctuation c || isSymbol c))
      _ -> Left "unknown posix class"
    isAlphaNumAscii c = isDigit c || isAsciiLower c || isAsciiUpper c

data ClassItem = ItemChar Char | ItemSet (Char -> Bool)

data Look = AtLineStart Bool | AtLineEnd Bool | AtWordBoundary Bool

data State
  = SChar (Char -> Bool) !Int
  | -- | alternatives in preference order
    SUnion [Int]
  | SCapture !Int !Int
  | SLook Look !Int
  | SMatch

data Prog = Prog
  { progStates :: V.Vector State,
    progStart :: Int,
    -- | a start and an end per group, group 0 being the whole match
    progSlots :: Int
  }

data Build = Build {nextId :: !Int, builtStates :: IntMap State}

new :: State -> Build -> (Int, Build)
new st b@Build {..} = (nextId, b {nextId = nextId + 1, builtStates = IM.insert nextId st builtStates})

reserve :: Build -> (Int, Build)
reserve b@Build {..} = (nextId, b {nextId = nextId + 1})

fill :: Int -> State -> Build -> Build
fill i st b@Build {..} = b {builtStates = IM.insert i st builtStates}

toProg :: Int -> Node -> Prog
toProg groups node =
  let (matchId, b1) = new SMatch (Build 0 IM.empty)
      (end0, b2) = new (SCapture 1 matchId) b1
      (body, b3) = nfa node end0 b2
      (start, Build {nextId = n, builtStates = states}) = new (SCapture 0 body) b3
   in Prog (V.generate n (states IM.!)) start (2 * (groups + 1))

-- | the repetition shapes follow regex-automata's thompson compiler
-- (c_at_least, c_bounded, c_zero_or_one), since the visited set makes the
-- shape observable
nfa :: Node -> Int -> Build -> (Int, Build)
nfa node next b = case node of
  One p -> new (SChar p next) b
  LineStart multi -> new (SLook (AtLineStart multi) next) b
  LineEnd multi -> new (SLook (AtLineEnd multi) next) b
  WordBoundary want -> new (SLook (AtWordBoundary want) next) b
  Group Nothing inner -> nfa inner next b
  Group (Just g) inner ->
    let (close, b1) = new (SCapture (2 * g + 1) next) b
        (body, b2) = nfa inner close b1
     in new (SCapture (2 * g) body) b2
  Seq ns -> foldr (\n k nx bb -> let (s, bb') = k nx bb in nfa n s bb') (,) ns next b
  Alt ns ->
    let (starts, b1) = foldr (\n (acc, bb) -> let (s, bb') = nfa n next bb in (s : acc, bb')) ([], b) ns
     in new (SUnion starts) b1
  Repeat lo hi greedy inner -> case hi of
    Just 1
      | lo == 0 ->
          let (x, b1) = nfa inner next b
           in new (SUnion (order x next)) b1
    Just m
      | lo == m -> copies lo next b
      | otherwise ->
          -- the optional copies nest: once one is skipped, the rest are too
          let (opt, b1) = foldr (\_ (nx, bb) -> let (x, bb') = nfa inner nx bb in new (SUnion (order x next)) bb') (next, b) [1 .. m - lo]
           in copies lo opt b1
    Nothing
      | lo == 0 && minLen inner > 0 ->
          let (u, b1) = reserve b
              (x, b2) = nfa inner u b1
           in (u, fill u (SUnion (order x next)) b2)
      | lo == 0 ->
          -- x* where x can match empty is compiled as (x+)?, which keeps
          -- the preference order right (rust-lang/regex#779)
          let (plus, b1) = reserve b
              (x, b2) = nfa inner plus b1
              b3 = fill plus (SUnion (order x next)) b2
           in new (SUnion (order x next)) b3
      | otherwise ->
          let (loop, b1) = reserve b
              (lastCopy, b2) = nfa inner loop b1
              b3 = fill loop (SUnion (order lastCopy next)) b2
           in copies (lo - 1) lastCopy b3
    where
      order x nx = if greedy then [x, nx] else [nx, x]
      copies n nx bb = foldr (\_ (s, bb') -> nfa inner s bb') (nx, bb) [1 .. n]

minLen :: Node -> Int
minLen = \case
  One _ -> 1
  LineStart _ -> 0
  LineEnd _ -> 0
  WordBoundary _ -> 0
  Group _ n -> minLen n
  Seq ns -> sum (map minLen ns)
  Alt ns -> if null ns then 0 else minimum (map minLen ns)
  Repeat lo _ _ n -> lo * minLen n

-- | Rust switches to another engine past its limit. we only ever see version
-- strings, so refusing is fine
visitedLimit :: Int
visitedLimit = 20000000

data Frame = Step !Int !Int | Restore !Int !Int

-- | the visited set lives for the whole search, like Rust's. a (state,
-- position) that failed from one start fails from every later one too
searchFrom :: Regex -> VU.Vector Char -> Int -> Either Text (Maybe (VU.Vector Int))
searchFrom re input from
  | V.length states * width > visitedLimit = Left "text too long for the regex engine"
  | otherwise = Right $ runST $ do
      visited <- MVU.replicate (V.length states * width) False
      slots <- MVU.replicate (progSlots prog) (-1)
      let tryAt at
            | at > len = pure Nothing
            | otherwise =
                run [Step (progStart prog) at] >>= \case
                  Nothing -> tryAt (at + 1)
                  found -> pure found
          run = \case
            [] -> pure Nothing
            Restore slot old : rest -> MVU.write slots slot old >> run rest
            Step sid at : rest -> step sid at rest
          step sid at rest = do
            let key = sid * width + at
            seen <- MVU.read visited key
            if seen
              then run rest
              else do
                MVU.write visited key True
                case states V.! sid of
                  SChar p nx
                    | at < len && p (input VU.! at) -> step nx (at + 1) rest
                    | otherwise -> run rest
                  SUnion [] -> run rest
                  SUnion (x : xs) -> step x at (map (`Step` at) xs <> rest)
                  SCapture slot nx -> do
                    old <- MVU.read slots slot
                    MVU.write slots slot at
                    step nx at (Restore slot old : rest)
                  SLook l nx
                    | look l at -> step nx at rest
                    | otherwise -> run rest
                  SMatch -> Just <$> VU.freeze slots
      tryAt from
  where
    prog = reProg re
    states = progStates prog
    len = VU.length input
    width = len + 1
    look l at = case l of
      AtLineStart multi -> at == 0 || (multi && input VU.! (at - 1) == '\n')
      AtLineEnd multi -> at == len || (multi && input VU.! at == '\n')
      AtWordBoundary want -> (wordAt (at - 1) /= wordAt at) == want
    wordAt i = i >= 0 && i < len && isWord (input VU.! i)

data Match = Match {matchStart, matchEnd :: Int, matchSlots :: VU.Vector Int}

toMatch :: VU.Vector Int -> Match
toMatch slots = Match (slots VU.! 0) (slots VU.! 1) slots

-- | every non-overlapping match, like Rust's captures_iter. an empty match
-- right where the previous one ended is skipped
matches :: Regex -> VU.Vector Char -> Either Text [Match]
matches re input = go 0 Nothing
  where
    go from lastEnd
      | from > VU.length input = Right []
      | otherwise =
          searchFrom re input from >>= \case
            Nothing -> Right []
            Just slots
              | s == e && Just e == lastEnd -> go (s + 1) lastEnd
              | otherwise -> (m :) <$> go (if s == e then e + 1 else e) (Just e)
              where
                m@Match {matchStart = s, matchEnd = e} = toMatch slots

groupsOf :: Regex -> VU.Vector Char -> Match -> [Maybe Text]
groupsOf re input Match {matchSlots = slots} =
  [ if a >= 0 && b >= 0 then Just (T.pack (VU.toList (VU.slice a (b - a) input))) else Nothing
  | g <- [0 .. reGroups re],
    let a = slots VU.! (2 * g)
        b = slots VU.! (2 * g + 1)
  ]

chars :: Text -> VU.Vector Char
chars = VU.fromList . T.unpack

captures :: Regex -> Text -> Either Text (Maybe [Maybe Text])
captures re t = fmap (groupsOf re input . toMatch) <$> searchFrom re input 0
  where
    input = chars t

capturesAll :: Regex -> Text -> Either Text [[Maybe Text]]
capturesAll re t = map (groupsOf re input) <$> matches re input
  where
    input = chars t

-- | Rust's replace_all: $1, ${1}, $name and ${name} expand to groups (empty
-- when missing), $$ is a dollar sign. $name takes the longest run of
-- letters, digits and underscores, so "$1a" is the group named "1a"
replaceAll :: Regex -> Text -> Text -> Either Text Text
replaceAll re rep t = T.concat . go 0 <$> matches re input
  where
    input = chars t
    slice a b = T.pack (VU.toList (VU.slice a (b - a) input))
    go pos [] = [slice pos (VU.length input)]
    go pos (m@Match {matchStart = s, matchEnd = e} : ms) = slice pos s : expand (groupsOf re input m) : go e ms
    expand groups = T.pack (expandS groups (T.unpack rep))
    expandS groups = \case
      [] -> []
      '$' : '$' : rest -> '$' : expandS groups rest
      '$' : '{' : rest
        | (name, '}' : rest') <- span (/= '}') rest, not (null name) -> lookupGroup groups name <> expandS groups rest'
      '$' : rest
        | (name, rest') <- span (\c -> isAlphaNum c || c == '_') rest, not (null name) -> lookupGroup groups name <> expandS groups rest'
      c : rest -> c : expandS groups rest
    lookupGroup groups name =
      let idx = readMaybe name <|> lookup (T.pack name) (reNames re)
       in maybe "" T.unpack (idx >>= \i -> if i < length groups then groups !! i else Nothing)
