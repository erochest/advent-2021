{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe (isNothing)

import qualified Data.Sequence as S
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day5

main :: IO ()
main = defaultMain suite

fixture :: ()
fixture = ()

suite :: TestTree
suite = testGroup "day 5"
    [ testGroup "part 1"
      [ testGroup "partOne"
        [ testCase "finds the answer" $
          partOne fixture @?= 0
        ]
      ]
    , testGroup "part 2"
      [ testGroup "partTwo"
        [ testCase "finds the answer" $
          partTwo fixture @?= 0
        ]
      ]
    ]

