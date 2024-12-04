#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci

--------------------------------
--------------------------------
----  Day 3:  Mull It Over  ----
--------------------------------
--------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc -- '.\day3.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day3part1
-- 

-- *Main> day3part2
-- 


-------------
-- Imports --
-------------
import Data.Char (isDigit)
import Data.List (tails, isPrefixOf, transpose, sort)
import Data.Map as M hiding (map, filter)
import Data.Maybe (fromJust)


-------------
-- Program --
-------------
main = day3part1

data ParserState = SeachingForM | MFound | UFound | LFound | OpenBracFound | ArgStr1DigitFound | CommaFound | ArgStr2DigitFound
data Expr = Mul Expr Expr | Plus Expr Expr | Val Int deriving (Show)

parse :: String -> Expr
parse s = go SeachingForM s "" ""
  where
    go :: ParserState -> String -> String -> String -> Expr
    go _ [] _ _
        = Val 0
    
    go SeachingForM (x:xs) argStr1 argStr2
        = case x of
            'm' -> go MFound xs argStr1 argStr2
            _   -> go SeachingForM xs "" ""
    
    go MFound (x:xs) argStr1 argStr2
        = case x of
            'u' -> go UFound xs argStr1 argStr2
            _   -> go SeachingForM xs "" ""
    
    go UFound (x:xs) argStr1 argStr2
        = case x of
            'l' -> go LFound xs argStr1 argStr2
            _   -> go SeachingForM xs "" ""
    
    go LFound (x:xs) argStr1 argStr2
        = case x of
            '(' -> go OpenBracFound xs argStr1 argStr2
            _   -> go SeachingForM xs "" ""
    
    go OpenBracFound (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFound xs (argStr1++[x]) argStr2
            else go SeachingForM xs "" ""
    
    go ArgStr1DigitFound (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFound xs (argStr1++[x]) argStr2
            else if x == ','
                    then go CommaFound xs argStr1 argStr2
                    else go SeachingForM xs "" ""
    
    go CommaFound (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFound xs argStr1 (argStr2++[x])
            else go SeachingForM xs "" ""
    
    go ArgStr2DigitFound (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFound xs argStr1 (argStr2++[x])
            else if x == ')'
                    then Plus (Mul (Val $ read argStr1) (Val $ read argStr2)) (go SeachingForM xs "" "")
                    else go SeachingForM xs "" ""

evaluate :: Expr -> Int
evaluate (Mul x y) = evaluate x * evaluate y
evaluate (Plus x y) = evaluate x + evaluate y
evaluate (Val x) = x

day3part1 = do
  contents <- readFile "day3 (data).csv"
  let result = evaluate $ parse contents
  print result
