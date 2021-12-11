{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Advent.Day6
  ( partOne
  , partTwo
  , LanternFish(..)
  , spawnLanternFish
  , lanternFishDay
  , lanternFishStep
  ) where

import Data.Foldable
import Debug.Trace

import Advent
import qualified Data.ByteString.Lazy.Char8 as C

partOne :: [LanternFish] -> Int
partOne school = length $ foldl' lanternFishDay school [(1 :: Int)..80]

partTwo :: [LanternFish] -> Int
partTwo = undefined

spawnStart :: Int 
spawnStart = 6

newFishBonus :: Int 
newFishBonus = 2

newtype LanternFish = LanternFish { getDays :: Int }
  deriving (Eq, Show, Ord)

spawnLanternFish :: (LanternFish, LanternFish)
spawnLanternFish = (LanternFish spawnStart, LanternFish (spawnStart + newFishBonus))

lanternFishStep :: LanternFish -> [LanternFish]
lanternFishStep (LanternFish 0) = let (lf0, lf1) = spawnLanternFish in [lf0, lf1]
lanternFishStep (LanternFish p) = [LanternFish (p - 1)]

lanternFishDay :: [LanternFish] -> Int -> [LanternFish]
lanternFishDay school _ = concatMap lanternFishStep school

instance Parseable [LanternFish] where
  parse = traverse (fmap (LanternFish . fst) . C.readInt) . C.split ','
