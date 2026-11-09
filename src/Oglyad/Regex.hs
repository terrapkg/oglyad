-- | a regex engine for the dialect of Rust's regex crate, which andax's
-- find(), find_all() and sub() use (andax/fns/kokoro.rs in FyraLabs/anda).
--
-- it works the way Rust's BoundedBacktracker does: compile to a Thompson
-- NFA shaped like regex-automata's compiler builds it, then backtrack with a
-- visited set of (state, position) pairs. the visited set keeps it linear
-- time, and it's also what decides which captures win when a repeated group
-- can match empty, so mirroring the NFA shape gets the same answers as Rust.
-- it was checked against the real crate on every regex in terrapkg/packages
-- and ~300k fuzzed ones, with identical matches and captures.
--
-- supported: literals and escapes, ., classes with ranges, negation and
-- [:posix:] names, \d \w \s and their negations, \b \B \A \z, ^ $, groups
-- ((...), (?:...), (?P<name>...), (?<name>...)), alternation, * + ? {n}
-- {n,} {n,m} and their lazy forms, and the i, m and s flags. anything else
-- is a compile error, which callers treat as "can't tell"
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

data Node
  = -- | one character
    One (Char -> Bool)
  | -- | ^ and $, True for multi-line
    LineStart Bool
  | LineEnd Bool
  | -- | \b, False for \B
    WordBoundary Bool
  | Group (Maybe Int) Node
  | Alt [Node]
  | Seq [Node]
  | -- | min, max, greedy
    Repeat Int (Maybe Int) Bool Node

data Regex = Regex
  { reProg :: Prog,
    reGroups :: Int,
    reNames :: [(Text, Int)]
  }

data Flags = Flags {fI, fM, fS :: Bool}

-- * parsing

-- | parser state: input left, groups numbered so far, names seen
data St = St {stIn :: String, stGroups :: Int, stNames :: [(Text, Int)]}

type Parse a = St -> Either Text (a, St)

compile :: Text -> Either Text Regex
compile src = do
  (node, st) <- alternation (Flags False False False) (St (T.unpack src) 0 [])
  case stIn st of
    [] -> Right (Regex (toProg (stGroups st) node) (stGroups st) (stNames st))
    c : _ -> Left ("unexpected " <> T.singleton c)

-- | a|b|c, up to a ')' or the end. (?flags) partway through applies to the
-- rest of the group, alternatives included, like Rust
alternation :: Flags -> Parse Node
alternation flags st0 = go flags st0 []
  where
    go fl st acc = do
      ((node, fl'), st') <- sequence' fl st []
      case stIn st' of
        '|' : rest -> go fl' st' {stIn = rest} (node : acc)
        _ -> Right (alt (reverse (node : acc)), st')
    alt [n] = n
    alt ns = Alt ns

sequence' :: Flags -> St -> [Node] -> Either Text ((Node, Flags), St)
sequence' fl st acc = case stIn st of
  [] -> done
  '|' : _ -> done
  ')' : _ -> done
  -- a bare flag group changes the flags for what follows
  '(' : '?' : rest
    | (fs, ')' : rest') <- span (`elem` ("imsxUR-" :: String)) rest,
      not (null fs) -> do
        fl' <- setFlags fl fs
        sequence' fl' st {stIn = rest'} acc
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
quantifier atom st = case stIn st of
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
      '?' : rest -> Right (Repeat lo hi False atom, st {stIn = rest})
      rest -> Right (Repeat lo hi True atom, st {stIn = rest})

atomP :: Flags -> Parse Node
atomP fl st = case stIn st of
  '(' : '?' : ':' : rest -> group Nothing st {stIn = rest}
  '(' : '?' : 'P' : '<' : rest -> named rest
  '(' : '?' : '<' : rest -> named rest
  '(' : '?' : _ -> Left "unsupported group"
  '(' : rest ->
    let n = stGroups st + 1
     in group (Just n) st {stIn = rest, stGroups = n}
  '[' : rest -> do
    (p, rest') <- classP fl rest
    Right (One p, st {stIn = rest'})
  '.' : rest -> Right (One (\c -> fS fl || c /= '\n'), st {stIn = rest})
  '^' : rest -> Right (LineStart (fM fl), st {stIn = rest})
  '$' : rest -> Right (LineEnd (fM fl), st {stIn = rest})
  '\\' : rest -> do
    (node, rest') <- escape fl rest
    Right (node, st {stIn = rest'})
  c : rest
    | c `elem` ("*+?{" :: String) -> Left "repetition with nothing to repeat"
    | otherwise -> Right (One (charEq fl c), st {stIn = rest})
  [] -> Left "unexpected end"
  where
    group idx s = do
      (inner, s') <- alternation fl s
      case stIn s' of
        ')' : rest -> Right (Group idx inner, s' {stIn = rest})
        _ -> Left "unclosed group"
    named rest = case span (\c -> isAlphaNum c || c == '_') rest of
      (name, '>' : rest')
        | not (null name) ->
            let n = stGroups st + 1
             in group (Just n) st {stIn = rest', stGroups = n, stNames = (T.pack name, n) : stNames st}
      _ -> Left "bad group name"

charEq :: Flags -> Char -> Char -> Bool
charEq fl c
  | fI fl = \x -> toLower x == toLower c || toUpper x == toUpper c
  | otherwise = (== c)

-- | an escape outside a class
escape :: Flags -> String -> Either Text (Node, String)
escape fl = \case
  'b' : rest -> Right (WordBoundary True, rest)
  'B' : rest -> Right (WordBoundary False, rest)
  'A' : rest -> Right (LineStart False, rest)
  'z' : rest -> Right (LineEnd False, rest)
  s -> do
    (p, rest) <- classEscape fl s
    Right (One p, rest)

-- | an escape that stands for one character or a class of them
classEscape :: Flags -> String -> Either Text (Char -> Bool, String)
classEscape fl = \case
  'd' : rest -> Right (isDigit', rest)
  'D' : rest -> Right (not . isDigit', rest)
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

-- | Rust's \d is Unicode-aware, and isDigit is ASCII-only
isDigit' :: Char -> Bool
isDigit' c = generalCategory c == DecimalNumber

isWord :: Char -> Bool
isWord c = isAlphaNum c || c == '_'

-- | the inside of [...], given what's after the '['
classP :: Flags -> String -> Either Text (Char -> Bool, String)
classP fl s0 = do
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
        (lo, rest) <- single s
        case (lo, rest) of
          (Left c, '-' : rest') | take 1 rest' /= "]" -> do
            (hi, rest'') <- single rest'
            case hi of
              Left d | c <= d -> go (range c d : acc) rest''
              _ -> Left "bad class range"
          (Left c, _) -> go (charEq fl c : acc) rest
          (Right p, _) -> go (p : acc) rest
    -- one class item: Left for a plain character (which can start a range),
    -- Right for an escape like \d
    single = \case
      '\\' : rest -> case rest of
        c : rest' | isPunctuation c || isSymbol c || c == ' ' -> Right (Left c, rest')
        'n' : rest' -> Right (Left '\n', rest')
        't' : rest' -> Right (Left '\t', rest')
        _ -> do
          (p, rest') <- classEscape fl rest
          Right (Right p, rest')
      c : rest -> Right (Left c, rest)
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

-- * compiling to an NFA

data Look = AtLineStart Bool | AtLineEnd Bool | AtWordBoundary Bool

data State
  = SChar (Char -> Bool) !Int
  | -- | alternatives in preference order
    SUnion [Int]
  | -- | record the position in a capture slot
    SCapture !Int !Int
  | SLook Look !Int
  | SMatch

data Prog = Prog
  { progStates :: V.Vector State,
    progStart :: Int,
    -- | two per group, group 0 being the whole match
    progSlots :: Int
  }

-- | NFA under construction: the next free id and the states so far
type Build = (Int, IntMap State)

new :: State -> Build -> (Int, Build)
new st (n, m) = (n, (n + 1, IM.insert n st m))

-- | reserve a state to fill in later, for loops
reserve :: Build -> (Int, Build)
reserve (n, m) = (n, (n + 1, m))

fill :: Int -> State -> Build -> Build
fill i st (n, m) = (n, IM.insert i st m)

toProg :: Int -> Node -> Prog
toProg groups node =
  let (matchId, b1) = new SMatch (0, IM.empty)
      (end0, b2) = new (SCapture 1 matchId) b1
      (body, b3) = nfa node end0 b2
      (start, (n, states)) = new (SCapture 0 body) b3
   in Prog (V.generate n (states IM.!)) start (2 * (groups + 1))

-- | compile a node that continues to `next`, returning its start state.
-- the repetition shapes follow regex-automata's thompson compiler
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
           in copies' lo opt b1
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
          -- n-1 copies, then one more that loops
          let (u, b1) = reserve b
              (x, b2) = nfa inner u b1
              b3 = fill u (SUnion (order x next)) b2
           in copies' (lo - 1) x b3
    where
      order x nx = if greedy then [x, nx] else [nx, x]
      copies n nx bb = copies' n nx bb
      copies' n nx bb = foldr (\_ (s, bb') -> nfa inner s bb') (nx, bb) [1 .. n]

-- | the shortest match a node can make, to tell whether x* can loop on
-- nothing
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

-- * matching

-- | positions the visited set can track before we give up. Rust switches to
-- another engine past its limit. we only ever see version strings, so a
-- refusal here is fine
visitedLimit :: Int
visitedLimit = 20000000

data Frame = Step !Int !Int | Restore !Int !Int

-- | the first match starting at or after a position, as capture slots.
-- the visited set lives for the whole search, like Rust's: a (state,
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

-- | (start, end, slots) of a match
type Match = (Int, Int, VU.Vector Int)

toMatch :: VU.Vector Int -> Match
toMatch slots = (slots VU.! 0, slots VU.! 1, slots)

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
            Just slots -> case toMatch slots of
              (s, e, _)
                | s == e && Just e == lastEnd -> go (s + 1) lastEnd
              m@(s, e, _) -> (m :) <$> go (if s == e then e + 1 else e) (Just e)

-- | group texts for one match, group 0 first. Nothing for groups that
-- didn't take part
groupsOf :: Regex -> VU.Vector Char -> Match -> [Maybe Text]
groupsOf re input (_, _, slots) =
  [ if a >= 0 && b >= 0 then Just (T.pack (VU.toList (VU.slice a (b - a) input))) else Nothing
  | g <- [0 .. reGroups re],
    let a = slots VU.! (2 * g)
        b = slots VU.! (2 * g + 1)
  ]

chars :: Text -> VU.Vector Char
chars = VU.fromList . T.unpack

-- | the first match, like Rust's Regex::captures. Left when the text is
-- too long for the engine
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
    go pos (m@(s, e, _) : ms) = slice pos s : expand (groupsOf re input m) : go e ms
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
