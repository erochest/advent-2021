{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day5
  ( partOne
  , partTwo
  , mapDimension
  , Point
  , Segment
  , Map
  , createMap
  , mapSegments
  , expandSegment
  , pointIndex
  , countDoubled
  ) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

import Debug.Trace

import Advent

partOne :: [Segment] -> Int
partOne = countDoubled . mapSegments createMap

partTwo :: [Segment] -> Int
partTwo = undefined

mapDimension :: Int
mapDimension = 1000

type Point = (Int, Int)
type Segment = (Point, Point)
type Map = V.Vector Int

instance Parseable Segment where
  parse = either (const Nothing) Just . parseOnly segment . C.toStrict

segment :: Parser Segment
segment = (,) <$> point <*> (" -> " *> point)

point :: Parser Point
point = (,) <$> decimal <*> (char8 ',' *> decimal)

createMap :: Map
createMap = V.replicate (mapDimension * mapDimension) 0

mapSegments :: Map -> [Segment] -> Map
mapSegments m segments = V.accum (+) m $ map ((, 1) . pointIndex) (concatMap expandSegment segments)

expandSegment :: Segment -> [Point]
expandSegment (start@(x0, y0), end@(x1, y1))
  | x0 == x1 || y0 == y1 = expand (offset x0 x1, offset y0 y1) start end
  | otherwise            = []
  where
    offset a b = case compare a b of
                  LT -> 1
                  EQ -> 0
                  GT -> (-1)
    expand offset@(offsetX, offsetY) start@(x0, y0) end
      | start == end = [end]
      | otherwise    = let next = (x0 + offsetX, y0 + offsetY)
                       in  start:expand offset next end

pointIndex :: Point -> Int
pointIndex (x, y) = x + y * mapDimension

countDoubled :: Map -> Int
countDoubled = V.length . V.filter (>1)

instance Parseable () where
  parse _ = Nothing
