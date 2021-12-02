module Advent
    ( interactLines
    , Parseable(..)
    ) where

import Data.Maybe (mapMaybe)

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as C

class Parseable a where
  parse :: C.ByteString -> Maybe a

interactLines :: (Parseable a, Show b) => ([a] -> b) -> IO ()
interactLines f =
  C.interact (toLazyByteString . (<> char8 '\n') . string8 . show . f . mapMaybe parse . C.lines)
