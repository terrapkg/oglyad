-- | reports update.rhai shapes our parser doesn't understand yet. this is your
-- to-do list for Oglyad.Source.Rhai
module Oglyad.Check.UnparsedRhai (check) where

import Data.Text qualified as T
import Oglyad.Types

check :: Check
check _ p@Package {..} = case pkgUpstream of
  Just (Unknown raw) ->
    [finding "unparsed-rhai" p Info ("can't parse update.rhai: " <> T.take 80 (head' (T.lines raw)))]
  _ -> []
  where
    head' (l : _) = l
    head' [] = ""
