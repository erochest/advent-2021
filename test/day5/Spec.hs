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

import Debug.Trace

main :: IO ()
main = defaultMain suite

fixture :: [Segment]
fixture = [ ((0,9), (5,9))
          , ((8,0), (0,8))
          , ((9,4), (3,4))
          , ((2,2), (2,1))
          , ((7,0), (7,4))
          , ((6,4), (2,0))
          , ((0,9), (2,9))
          , ((3,4), (1,4))
          , ((0,0), (8,8))
          , ((5,5), (8,2))
          ]

suite :: TestTree
suite = testGroup "day 5"
    [ testGroup "part 1"
      [ testGroup "partOne"
        [ testCase "finds the answer" $
          partOne fixture @?= 5
        ]
      , testGroup "parse"
        [ testCase "parses an input line" $ do
          let expected :: Segment = ((0, 9), (5, 9))
          parse "0,9 -> 5,9" @?= Just expected
        ]
      , testGroup "countDoubled"
        [ testCase "counts the number of items greater than 1" $ do
          let input = [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
                      , 0, 0, 1, 0, 0, 0, 0, 0, 1, 0
                      , 0, 0, 1, 0, 0, 0, 0, 0, 1, 0
                      , 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
                      , 0, 1, 1, 2, 1, 1, 1, 2, 1, 1
                      , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      , 2, 2, 2, 1, 1, 1, 0, 0, 0, 0
                      ]
          countDoubled input @?= 5
        ]
      , testGroup "createMap"
        [ testCase "returns an empty map vector" $ do
            V.length createMap @?= mapDimension * mapDimension 
            V.all (==0) createMap @? "initialized to zeros"
        ]
      , testGroup "mapSegments"
        [ testCase "marks starting position on the map" $ do
          let segment  = ((2, 1), (2, 4))
              m        = mapSegments createMap [segment]
          m V.! 1002 @?= 1
        , testCase "increments values for overlapping segments" $ do
          let m0 = createMap
              m1 = mapSegments m0 [((2, 1), (2, 4)), ((2, 4), (7, 4))]
          m1 V.! 4002 @?= 2
        ]
      , testGroup "expandSegment"
        [ testCase "includes starting position" $ do
          let segment  = ((2, 1), (2, 4))
          (2, 1) `elem` expandSegment segment @? "includes starting point"
        , testCase "includes ending position" $ do
          let segment = ((2, 1), (2, 4))
          (2, 4) `elem` expandSegment segment @? "includes ending point"
        , testCase "when start and end are the same, output one item" $ do
          let segment = ((2, 1), (2, 1))
          length (expandSegment segment) @?= 1
        , testCase "includes middle positions going top-to-bottom" $ do
          let segment = ((2, 1), (2, 4))
          (2, 2) `elem` expandSegment segment @? "includes middle point"
          (2, 3) `elem` expandSegment segment @? "includes middle point"
        , testCase "includes middle positions going bottom-to-top" $ do
          let segment = ((2, 4), (2, 1))
          (2, 2) `elem` expandSegment segment @? "includes middle point"
          (2, 3) `elem` expandSegment segment @? "includes middle point"
        , testCase "includes middle positions going left-to-right" $ do
          let segment = ((1, 2), (4, 2))
          (2, 2) `elem` expandSegment segment @? "includes middle point"
          (3, 2) `elem` expandSegment segment @? "includes middle point"
        , testCase "includes middle positions going right-to-left" $ do
          let segment = ((4, 2), (1, 2))
          (2, 2) `elem` expandSegment segment @? "includes middle point"
          (3, 2) `elem` expandSegment segment @? "includes middle point"
        , testCase "returns an empty list if the points are on a diagonal" $ do
          let segment = ((1, 7), (4, 20))
          length (expandSegment segment) @?= 0
        ]
      , testGroup "pointIndex"
        [ testCase "maps first row to the column" $ do
          pointIndex (4, 0) @?= 4
        , testCase "maps the first column to a multiple of the dimensions" $ do
          pointIndex (0, 7) @?= 7 * mapDimension
        , testCase "adds the row to the multiple of the dimensions" $ do
          pointIndex (3, 4) @?= 4 * mapDimension + 3
        ]
      ]
    , testGroup "part 2"
      [ testGroup "partTwo"
        [ testCase "finds the answer" $
          partTwo fixture @?= 0
        ]
      ]
    ]

