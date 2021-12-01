module Main where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Debug.Trace

main :: IO ()
main = C.interact partTwo

traceValue :: Show a => String -> a -> a
traceValue tag value = trace (tag ++ ": " ++ show value) value

partOne :: C.ByteString -> C.ByteString
partOne = toLazyByteString . (<> char8 '\n') . intDec . countIncreases . parse

partTwo :: C.ByteString -> C.ByteString
partTwo = toLazyByteString . (<> char8 '\n') . intDec . countIncreases . map sumTriple . toTriples . parse

parse :: C.ByteString -> [Int]
parse = map (fst . fromJust . C.readInt) . C.lines
-- TODO: Oh. `fromJust` is bad.

toTriples :: [Int] -> [(Int, Int, Int)]
toTriples (a : rest@(b : c : _)) = (a, b, c) : toTriples rest
toTriples [_, _] = []
toTriples [_] = []
toTriples [] = []

sumTriple :: (Int, Int, Int) -> Int
sumTriple (a, b, c) = a + b + c

countIncreases :: [Int] -> Int
countIncreases = snd . foldl increases (Nothing, 0)

increases :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
increases (Nothing, count) current = (Just current, count)
increases (Just prev, count) current
    | prev < current = (Just current, count + 1)
    | otherwise      = (Just current, count)
