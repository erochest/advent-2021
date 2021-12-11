{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day6

import qualified Data.List as L
import Debug.Trace
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)
import Advent.Day6 (spawnLanternFish, LanternFish (LanternFish), lanternFishStep)

main :: IO ()
main = defaultMain suite

fixture :: [LanternFish]
fixture = map LanternFish [3,4,3,1,2]

suite :: TestTree
suite = testGroup "day 6"
  [ testGroup "part 1"
    [ testGroup "partOne"
      [ testCase "calculates the checksum for the fixture" $
        partOne fixture @?= 5934
      ]
    , testGroup "spawnLanternFish"
      [ testCase "Resets the parent's spawn period." $ do
        fst spawnLanternFish @?= LanternFish 6
      , testCase "Creates a child with a longer spawn period." $
        snd spawnLanternFish @?= LanternFish 8
      ]
    , testGroup "lanternFishStep"
      [ testCase "decrements the period every day" $
        lanternFishStep (LanternFish 3) @?= [LanternFish 2]
      , testGroup "when the period is over"
        [ testCase "resets the period" $
          head (lanternFishStep (LanternFish 0)) @?= LanternFish 6
        , testCase "creates a new child with a higher period" $
          (lanternFishStep (LanternFish 0) !! 1) @?= LanternFish 8
        ]
      ]
    , testGroup "lanternFishDay"
      [ testCase "increments the school" $ do
        let input = fixture
            expected = map LanternFish [0,1,2,2,3]
        expected @=? L.sort (lanternFishDay input 0)
      ]
    , testGroup "Parseable.parse"
      [ testCase "parses a list of lantern fish" $ do
        parse "3,4,3,1,2" @?= Just fixture
      ]
    ]
  , testGroup "part 2"
     [ testGroup "partTwo" []
     ]
  ]