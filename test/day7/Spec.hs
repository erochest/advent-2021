{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit

import Advent
import Advent.Day7


main :: IO ()
main = defaultMain suite

fixture :: ()
fixture = undefined

suite :: TestTree
suite = testGroup "day 7"
  [ testGroup "part 1"
    [ testGroup "partOne"
      [ testCase "calculates the checksum for the fixture" $
        partOne fixture @?= undefined
      ]
    ]
  , testGroup "part 2"
     [ testGroup "partTwo"
      [ testCase "calculates the checksum for the fixture" $
        partTwo fixture @?= undefined
      ]
     ]
  ]