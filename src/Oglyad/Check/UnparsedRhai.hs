module Oglyad.Check.UnparsedRhai (check) where

import Data.Maybe (isJust)
import Data.Text qualified as T
import Oglyad.Source.Rhai (balanced, tokenize)
import Oglyad.Types

check :: Check
check Snapshot {snapConfig} p@Package {..} =
  broken <> case pkgUpstream of
    -- anda ran it, so there's already an answer or an upstream-error
    Just (Unknown _) | isJust pkgUpstreamVer || isJust pkgUpstreamErr -> []
    Just (Unknown _) | not (cfgRolling snapConfig || pkgUpdBranch) -> []
    Just (Unknown raw) ->
      [finding "unparsed-rhai" p Info ("can't parse update.rhai: " <> T.take 80 (T.takeWhile (/= '\n') raw))]
    _ -> []
  where
    -- Rhai itself rejects these, so `anda update` fails on them every run
    broken = case tokenize <$> pkgRhai of
      Just (Just toks)
        | not (balanced toks) -> [finding "unparsed-rhai" p Warn "update.rhai has unbalanced brackets, so anda can't run it"]
      _ -> []
