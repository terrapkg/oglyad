-- | reports update.rhai shapes our parser doesn't understand yet. this is your
-- to-do list for Oglyad.Source.Rhai
module Oglyad.Check.UnparsedRhai (check) where

import Data.Maybe (isJust)
import Data.Text qualified as T
import Oglyad.Source.Rhai (balanced, tokenize)
import Oglyad.Types

check :: Check
check _ p@Package {..} =
  broken <> case pkgUpstream of
    -- anda ran it for us, so there's an answer (or an upstream-error)
    Just (Unknown _) | isJust pkgUpstreamVer || isJust pkgUpstreamErr -> []
    Just (Unknown raw) ->
      [finding "unparsed-rhai" p Info ("can't parse update.rhai: " <> T.take 80 (head' (T.lines raw)))]
    _ -> []
  where
    -- not our parser's limits but a script Rhai itself rejects, so
    -- `anda update` fails on it every run
    broken = case tokenize <$> pkgRhai of
      Just (Just toks)
        | not (balanced toks) -> [finding "unparsed-rhai" p Warn "update.rhai has unbalanced brackets, so anda can't run it"]
      _ -> []
    head' (l : _) = l
    head' [] = ""
