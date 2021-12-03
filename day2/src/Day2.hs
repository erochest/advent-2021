{-# LANGUAGE OverloadedStrings #-}

module Day2
    ( Direction(..)
    , Movement(..)
    , partOne
    , movement
    , move
    ) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (parseOnly, Parser, char, decimal, space)

import Advent

partOne :: [Movement] -> Int
partOne = uncurry (*) . foldl' move (0, 0)

data Direction = Forward | Down | Up
  deriving (Show, Eq, Enum)

data Movement = Movement !Direction !Int
  deriving (Show, Eq)

instance Parseable Movement where
  parse = either (const Nothing) Just . parseOnly movement . C.toStrict

movement :: Parser Movement
movement = Movement <$> direction <*> (space *> decimal)

direction :: Parser Direction
direction =   (Forward <$ "forward")
          <|> (Up      <$ "up")
          <|> (Down    <$ "down")

move :: (Int, Int) -> Movement -> (Int, Int)
move (horizontal, depth) (Movement Forward amount) = inspect ("Forward " ++ show amount) (horizontal + amount, depth)
move (horizontal, depth) (Movement Up amount)      = inspect ("Up " ++ show amount) (horizontal, depth - amount)
move (horizontal, depth) (Movement Down amount)    = inspect ("Down " ++ show amount) (horizontal, depth + amount)

