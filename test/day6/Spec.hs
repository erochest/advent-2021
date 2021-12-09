{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day6

import Debug.Trace

main :: IO ()
main = defaultMain suite

fixture :: ()
fixture = ()

suite :: TestTree
suite = testGroup "day 5"
  [
  ]

