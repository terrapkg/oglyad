-- | runs the version-setting part of an update.rhai against an upstream value
-- we already fetched, so guards like `if v.starts_with("cli-v")` and edits like
-- `v.crop(5)` apply before drift compares it with the spec.
--
-- it understands a small slice of Rhai: let, assignment, if/else, try/catch,
-- terminate(), rpm.version / rpm.global("ver", ...), the string methods update
-- scripts use, andax's regex helpers (find, find_all, sub), template strings,
-- + and indexing. anything else that touches a value we need gives up with
-- 'CantTell', and drift falls back to the raw upstream value
module Oglyad.Source.RhaiEval
  ( Simulated (..),
    simulateVersion,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Oglyad.Regex qualified as Re
import Oglyad.Source.Rhai (Token (..), balanced, sourceCall, tokenize)
import Oglyad.Types (UpstreamSource)
import Text.Read (readMaybe)

data Simulated
  = -- | the script would set Version to this
    Sets Text
  | -- | the script finishes (or calls terminate()) without setting Version,
    -- so `anda update` wouldn't touch the spec for this upstream value
    NoUpdate
  | -- | the script does something we don't model
    CantTell
  deriving stock (Eq, Show)

-- | what Version the script would set, given the value its source helper
-- returns. the source is the one 'Oglyad.Source.Rhai.parseUpdateRhai' found.
-- any other helper call evaluates to an unknown value
simulateVersion :: UpstreamSource -> Text -> Text -> Simulated
simulateVersion src upstream script = case tokenize script of
  Just toks
    | balanced toks,
      Just prog <- stmts toks ->
        case exec (src, upstream) emptyEnv prog of
          Right env -> outcome env
          Left (Terminated env) -> outcome env
          -- an error nothing caught fails the whole `anda update` run. we
          -- can't say much about what the spec would look like then
          Left (Thrown _) -> CantTell
          Left GiveUp -> CantTell
  _ -> CantTell
  where
    -- rpm.version wins over rpm.global("ver", ...), since some scripts set the
    -- global to the raw tag and Version to a cleaned-up spelling of it
    outcome env = maybe NoUpdate Sets (envVersion env <|> envGlobal env)

-- * syntax

data Value = VStr Text | VNum Int | VBool Bool | VArr [Value] | VUnit
  deriving stock (Eq, Show)

data Expr
  = Lit Value
  | -- | a backtick string with ${...} to fill in
    Tmpl Text
  | Var Text
  | Src UpstreamSource
  | Call Text [Expr]
  | Method Expr Text [Expr]
  | Index Expr Expr
  | Not Expr
  | Bin Text Expr Expr
  | -- | rpm.changed(). we're asking "what if upstream moved", so it's true
    Changed
  | -- | an expression we couldn't parse. only fatal if something needs it
    Unparsed
  deriving stock (Show)

data Stmt
  = Let Text Expr
  | -- | v.crop(1); and friends, which change v in place
    Mutate Text Text [Expr]
  | -- | True for rpm.version, False for rpm.global("ver", ...)
    SetVersion Bool Expr
  | If Expr [Stmt] [Stmt]
  | Try [Stmt] [Stmt]
  | Terminate
  | -- | rpm.release(), print(...), and other calls that can't change Version
    Ignored
  | -- | an expression on its own, like find(...) used as a guard inside try
    ExprStmt Expr [Token]
  | -- | a statement we don't understand, kept so we can see what it touches
    Opaque [Token]
  deriving stock (Show)

-- * parsing

stmts :: [Token] -> Maybe [Stmt]
stmts = \case
  [] -> Just []
  TSym ';' : rest -> stmts rest
  toks -> do
    (s, rest) <- stmt toks
    (s :) <$> stmts rest

stmt :: [Token] -> Maybe (Stmt, [Token])
stmt = \case
  TIdent "if" : rest -> ifStmt rest
  TIdent "try" : TSym '{' : rest -> do
    (body, rest1) <- block rest
    case rest1 of
      TIdent "catch" : rest2 -> do
        let rest3 = case rest2 of
              TSym '(' : TIdent _ : TSym ')' : r -> r
              r -> r
        case rest3 of
          TSym '{' : r -> do
            (handler, rest4) <- block r
            Just (Try body handler, rest4)
          _ -> Nothing
      _ -> Nothing
  toks ->
    let (s, rest) = untilSemi toks
     in Just (simple s, rest)

ifStmt :: [Token] -> Maybe (Stmt, [Token])
ifStmt toks = do
  let (c, rest) = break (== TSym '{') toks
  rest1 <- case rest of
    TSym '{' : r -> Just r
    _ -> Nothing
  (yes, rest2) <- block rest1
  case rest2 of
    TIdent "else" : TIdent "if" : rest3 -> do
      (s, rest4) <- ifStmt rest3
      Just (If (expr c) yes [s], rest4)
    TIdent "else" : TSym '{' : rest3 -> do
      (no, rest4) <- block rest3
      Just (If (expr c) yes no, rest4)
    _ -> Just (If (expr c) yes [], rest2)

-- | the statements inside braces, given the tokens just after the '{'
block :: [Token] -> Maybe ([Stmt], [Token])
block toks = do
  (inner, rest) <- closing toks
  body <- stmts inner
  Just (body, rest)

simple :: [Token] -> Stmt
simple = \case
  [TIdent "terminate", TSym '(', TSym ')'] -> Terminate
  TIdent "let" : TIdent v : TSym '=' : e -> Let v (expr e)
  TIdent "rpm" : TSym '.' : TIdent "version" : TSym '(' : r
    | Just [a] <- argsOnly r -> SetVersion True a
  TIdent "rpm" : TSym '.' : TIdent f : TSym '(' : TStr key : TSym ',' : r
    | f `elem` ["global", "define"],
      key `elem` ["ver", "version"],
      Just [a] <- argsOnly r ->
        SetVersion False a
  TIdent "rpm" : _ -> Ignored
  TIdent "print" : _ -> Ignored
  TIdent v : TSym '.' : TIdent m : TSym '(' : r
    | m `elem` mutators,
      Just as <- argsOnly r ->
        Mutate v m as
  TIdent v : TSym '=' : e@(t : _) | t /= TSym '=' -> Let v (expr e)
  toks -> case expr toks of
    Unparsed -> Opaque toks
    e -> ExprStmt e toks
  where
    -- the arguments of a call that ends the statement
    argsOnly r = case args r of
      Just (as, []) -> Just as
      _ -> Nothing

-- | string methods that change the string they're called on
mutators :: [Text]
mutators = ["crop", "truncate", "pop", "replace", "trim", "make_lower", "make_upper"]

-- | tokens up to a ';' outside any brackets, and what's after it
untilSemi :: [Token] -> ([Token], [Token])
untilSemi = go (0 :: Int)
  where
    go _ [] = ([], [])
    go d (t : ts) = case t of
      TSym ';' | d == 0 -> ([], ts)
      TSym c
        | c `elem` ("([{" :: String) -> cons t (go (d + 1) ts)
        | c `elem` (")]}" :: String) -> cons t (go (d - 1) ts)
      _ -> cons t (go d ts)
    cons t (a, b) = (t : a, b)

-- | given the tokens after an opening bracket, the tokens inside it and the
-- ones after its closing bracket
closing :: [Token] -> Maybe ([Token], [Token])
closing = go (0 :: Int)
  where
    go _ [] = Nothing
    go d (t : ts) = case t of
      TSym c
        | c `elem` (")]}" :: String) -> if d == 0 then Just ([], ts) else cons t (go (d - 1) ts)
        | c `elem` ("([{" :: String) -> cons t (go (d + 1) ts)
      _ -> cons t (go d ts)
    cons t = fmap (\(a, b) -> (t : a, b))

-- | call arguments, given the tokens after the '('
args :: [Token] -> Maybe ([Expr], [Token])
args toks = do
  (inner, rest) <- closing toks
  Just (if null inner then [] else map expr (splitCommas inner), rest)
  where
    splitCommas ts = case go (0 :: Int) ts of
      (a, []) -> [a]
      (a, rest) -> a : splitCommas rest
    go _ [] = ([], [])
    go d (t : ts) = case t of
      TSym ',' | d == 0 -> ([], ts)
      TSym c
        | c `elem` ("([{" :: String) -> cons t (go (d + 1) ts)
        | c `elem` (")]}" :: String) -> cons t (go (d - 1) ts)
      _ -> cons t (go d ts)
    cons t (a, b) = (t : a, b)

-- | a whole expression. leftover tokens mean we misread it
expr :: [Token] -> Expr
expr toks = case orE toks of
  Just (e, []) -> e
  _ -> Unparsed

type P = [Token] -> Maybe (Expr, [Token])

orE, andE, cmpE, addE, unaryE, primary :: P
orE = binLoop andE [("||", [TSym '|', TSym '|'])]
andE = binLoop cmpE [("&&", [TSym '&', TSym '&'])]
cmpE ts = do
  (l, r) <- addE ts
  case [(op, rest) | (op, syms) <- cmpOps, Just rest <- [stripPrefixList syms r]] of
    (op, rest) : _ -> do
      (x, rest') <- addE rest
      Just (Bin op l x, rest')
    [] -> Just (l, r)
  where
    -- two-character operators first, so "<=" isn't read as "<"
    cmpOps =
      [ ("==", [TSym '=', TSym '=']),
        ("!=", [TSym '!', TSym '=']),
        ("<=", [TSym '<', TSym '=']),
        (">=", [TSym '>', TSym '=']),
        ("<", [TSym '<']),
        (">", [TSym '>'])
      ]
addE = binLoop unaryE [("+", [TSym '+'])]
unaryE = \case
  TSym '!' : r -> do
    (e, r') <- unaryE r
    Just (Not e, r')
  ts -> do
    (p, r) <- primary ts
    postfix p r
primary = \case
  TStr s : r -> Just (Lit (VStr s), r)
  TTmpl s : r -> Just (Tmpl s, r)
  TNum n : r -> (\i -> (Lit (VNum i), r)) <$> readMaybe (T.unpack n)
  TIdent "true" : r -> Just (Lit (VBool True), r)
  TIdent "false" : r -> Just (Lit (VBool False), r)
  TIdent "rpm" : TSym '.' : TIdent "changed" : TSym '(' : TSym ')' : r -> Just (Changed, r)
  TSym '(' : r -> do
    (e, r') <- orE r
    case r' of
      TSym ')' : r'' -> Just (e, r'')
      _ -> Nothing
  ts | Just (s, r) <- sourceCall ts -> Just (Src s, r)
  -- the lexer reads digits as identifiers too
  TIdent n : r | T.all isDigit n -> (\i -> (Lit (VNum i), r)) <$> readMaybe (T.unpack n)
  TSym '-' : TIdent n : r | T.all isDigit n -> (\i -> (Lit (VNum (negate i)), r)) <$> readMaybe (T.unpack n)
  TIdent f : TSym '(' : r -> do
    (as, r') <- args r
    Just (Call f as, r')
  TIdent v : r@(t : _) | t /= TSym ':' -> Just (Var v, r)
  [TIdent v] -> Just (Var v, [])
  _ -> Nothing

-- | method calls, properties and indexing: v.crop(1), v.len, caps[1]
postfix :: Expr -> P
postfix e = \case
  TSym '.' : TIdent m : TSym '(' : r -> do
    (as, r') <- args r
    postfix (Method e m as) r'
  TSym '.' : TIdent m : r -> postfix (Method e m []) r
  TSym '[' : r -> do
    (inner, r') <- closing r
    postfix (Index e (expr inner)) r'
  r -> Just (e, r)

binLoop :: P -> [(Text, [Token])] -> P
binLoop next ops ts = next ts >>= uncurry loop
  where
    loop l r = case [(op, rest) | (op, syms) <- ops, Just rest <- [stripPrefixList syms r]] of
      (op, rest) : _ -> do
        (x, rest') <- next rest
        loop (Bin op l x) rest'
      [] -> Just (l, r)

stripPrefixList :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefixList [] ys = Just ys
stripPrefixList (x : xs) (y : ys) | x == y = stripPrefixList xs ys
stripPrefixList _ _ = Nothing

-- * evaluation

data Env = Env
  { -- | Nothing is a value we can't know, like another helper's result
    envVars :: Map Text (Maybe Value),
    envVersion :: Maybe Text,
    envGlobal :: Maybe Text
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Nothing Nothing

data Stop
  = Terminated Env
  | -- | a Rhai error, like find() not matching. try/catch can stop it
    Thrown Env
  | GiveUp

-- | why an expression has no value
data NoValue
  = -- | it depends on something we can't see or don't model
    Unknown
  | -- | evaluating it raises a Rhai error
    Throw

type Eval = Either NoValue Value

type Ctx = (UpstreamSource, Text)

exec :: Ctx -> Env -> [Stmt] -> Either Stop Env
exec _ env [] = Right env
exec ctx env (s : ss) = step s >>= \env' -> exec ctx env' ss
  where
    step = \case
      Let v e -> case eval ctx env e of
        Right val -> Right (bind v (Just val))
        Left Unknown -> Right (bind v Nothing)
        Left Throw -> Left (Thrown env)
      Mutate v m as -> case Map.lookup v (envVars env) of
        Just (Just val) -> case traverse (eval ctx env) as >>= mutate val m of
          Right val' -> Right (bind v (Just val'))
          Left Throw -> Left (Thrown env)
          Left Unknown -> Right (bind v Nothing)
        -- mutating something we can't see keeps it unknown
        _ -> Right env
      SetVersion isMain e -> case eval ctx env e of
        Right (VStr t)
          | isMain -> Right env {envVersion = Just t}
          | otherwise -> Right env {envGlobal = Just t}
        Left Throw -> Left (Thrown env)
        _ -> Left GiveUp
      If c yes no -> case eval ctx env c of
        Right (VBool b) -> exec ctx env (if b then yes else no)
        Left Throw -> Left (Thrown env)
        -- a branch we can't decide only matters if it could change Version
        _ | all inert (yes <> no) -> Right env
        _ -> Left GiveUp
      Try body handler -> case exec ctx env body of
        Left (Thrown env') -> exec ctx env' handler
        r -> r
      Terminate -> Left (Terminated env)
      Ignored -> Right env
      ExprStmt e toks -> case eval ctx env e of
        Right _ -> Right env
        Left Throw -> Left (Thrown env)
        -- it might throw. that only matters if it's looking at our values
        Left Unknown
          | touches toks -> Left GiveUp
          | otherwise -> Right env
      Opaque toks
        | touches toks -> Left GiveUp
        | otherwise -> Right env
    bind v val = env {envVars = Map.insert v val (envVars env)}
    touches toks = any known [v | TIdent v <- toks]
    known v = maybe False isJust (Map.lookup v (envVars env))

-- | can this statement affect what Version ends up as?
inert :: Stmt -> Bool
inert = \case
  Ignored -> True
  If _ yes no -> all inert (yes <> no)
  Try body handler -> all inert (body <> handler)
  _ -> False

eval :: Ctx -> Env -> Expr -> Eval
eval ctx@(src, upstream) env = \case
  Lit v -> Right v
  Tmpl t -> VStr <$> template t
  Var v -> maybe (Left Unknown) Right (Map.lookup v (envVars env) >>= id)
  Src s
    | s == src -> Right (VStr upstream)
    | otherwise -> Left Unknown
  Call "print" _ -> Right VUnit
  Call f as -> traverse (eval ctx env) as >>= call f
  -- in Rhai, x.f(a) is f(x, a) for any function
  Method e m as -> do
    val <- eval ctx env e
    vals <- traverse (eval ctx env) as
    method val m vals <> call m (val : vals)
  Index e i -> do
    val <- eval ctx env e
    idx <- eval ctx env i
    index val idx
  Not e ->
    eval ctx env e >>= \case
      VBool b -> Right (VBool (not b))
      _ -> Left Unknown
  Bin op l r -> do
    a <- eval ctx env l
    b <- eval ctx env r
    binop op a b
  Changed -> Right (VBool True)
  Unparsed -> Left Unknown
  where
    -- only ${name} is filled in. anything fancier inside the braces is unknown
    template t = case T.breakOn "${" t of
      (pre, "") -> Right pre
      (pre, rest) -> case T.breakOn "}" (T.drop 2 rest) of
        (name, after)
          | not (T.null after),
            T.all (\c -> isAlphaNum c || c == '_') name -> do
              val <- eval ctx env (Var name)
              shown <- case val of
                VStr s -> Right s
                VNum n -> Right (T.pack (show n))
                _ -> Left Unknown
              ((pre <> shown) <>) <$> template (T.drop 1 after)
        _ -> Left Unknown

-- | andax functions. regexes the engine can't compile are unknown rather
-- than errors, since that's usually our engine's limit and not the script's
call :: Text -> [Value] -> Eval
call f as = case (f, as) of
  ("find", [VStr r, VStr t, VNum g]) -> do
    re <- regex r
    caps <- slow (Re.captures re t) >>= maybe (Left Throw) Right
    if g < 0 || g >= length caps
      then Left Throw
      else maybe (Left Throw) (Right . VStr) (caps !! g)
  ("find_all", [VStr r, VStr t]) -> do
    re <- regex r
    all' <- slow (Re.capturesAll re t)
    Right (VArr [VArr (map (maybe VUnit VStr) caps) | caps <- all'])
  ("sub", [VStr r, VStr rep, VStr t]) -> do
    re <- regex r
    VStr <$> slow (Re.replaceAll re rep t)
  _ -> Left Unknown
  where
    regex = either (const (Left Unknown)) Right . Re.compile
    -- Rust would have answered, we just ran out of patience
    slow = either (const (Left Unknown)) Right

-- | built-in methods that don't change the value they're called on. the
-- in-place ones return the changed string here, which is how scripts like
-- `gh(...).crop(1)` read
method :: Value -> Text -> [Value] -> Eval
method val m as = case (val, m, as) of
  (VStr s, "pop", []) -> Right (VStr (T.takeEnd 1 s))
  (VStr s, "pop", [VNum n]) -> Right (VStr (T.takeEnd n s))
  (VStr s, "starts_with", [VStr p]) -> Right (VBool (p `T.isPrefixOf` s))
  (VStr s, "ends_with", [VStr p]) -> Right (VBool (p `T.isSuffixOf` s))
  (VStr s, "contains", [VStr p]) -> Right (VBool (p `T.isInfixOf` s))
  (VStr s, "len", []) -> Right (VNum (T.length s))
  (VStr s, "is_empty", []) -> Right (VBool (T.null s))
  (VStr s, "to_lower", []) -> Right (VStr (T.toLower s))
  (VStr s, "to_upper", []) -> Right (VStr (T.toUpper s))
  (VArr xs, "len", []) -> Right (VNum (length xs))
  (VArr xs, "is_empty", []) -> Right (VBool (null xs))
  _ -> mutate val m as

-- | what an in-place string method leaves behind, following Rhai's string
-- api (crop keeps from start, truncate keeps the first n, pop drops the end)
mutate :: Value -> Text -> [Value] -> Eval
mutate (VStr s) m as =
  VStr <$> case (m, as) of
    ("crop", [VNum start]) | start >= 0 -> Right (T.drop start s)
    ("crop", [VNum start, VNum len]) | start >= 0 -> Right (T.take len (T.drop start s))
    ("truncate", [VNum n]) -> Right (T.take n s)
    ("pop", []) -> Right (T.dropEnd 1 s)
    ("pop", [VNum n]) -> Right (T.dropEnd n s)
    ("replace", [VStr a, VStr b]) | not (T.null a) -> Right (T.replace a b s)
    ("trim", []) -> Right (T.strip s)
    ("make_lower", []) -> Right (T.toLower s)
    ("make_upper", []) -> Right (T.toUpper s)
    _ -> Left Unknown
mutate _ _ _ = Left Unknown

-- | arrays and strings, with negative indices counting from the end like
-- Rhai. out of range is a Rhai error
index :: Value -> Value -> Eval
index val (VNum i) = case val of
  VArr xs -> pick xs
  VStr s -> VStr . T.singleton <$> pick (T.unpack s)
  _ -> Left Unknown
  where
    pick :: [a] -> Either NoValue a
    pick xs =
      let n = length xs
          j = if i < 0 then n + i else i
       in if j >= 0 && j < n then Right (xs !! j) else Left Throw
index _ _ = Left Unknown

binop :: Text -> Value -> Value -> Eval
binop op a b = case (op, a, b) of
  ("&&", VBool x, VBool y) -> Right (VBool (x && y))
  ("||", VBool x, VBool y) -> Right (VBool (x || y))
  ("+", VStr x, VStr y) -> Right (VStr (x <> y))
  ("+", VStr x, VNum y) -> Right (VStr (x <> T.pack (show y)))
  ("+", VNum x, VStr y) -> Right (VStr (T.pack (show x) <> y))
  ("+", VNum x, VNum y) -> Right (VNum (x + y))
  ("==", _, _) -> Right (VBool (a == b))
  ("!=", _, _) -> Right (VBool (a /= b))
  (_, VStr x, VStr y) -> maybe (Left Unknown) (Right . VBool) (ordOp (compare x y))
  (_, VNum x, VNum y) -> maybe (Left Unknown) (Right . VBool) (ordOp (compare x y))
  _ -> Left Unknown
  where
    ordOp o = case op of
      "<" -> Just (o == LT)
      "<=" -> Just (o /= GT)
      ">" -> Just (o == GT)
      ">=" -> Just (o /= LT)
      _ -> Nothing
