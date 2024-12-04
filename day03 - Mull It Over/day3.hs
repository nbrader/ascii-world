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
main = day3part2

data ParserState = SeachingForMulOrDont | MulDisabledSearchingForDo | DFoundOfDo | OFoundOfDo | OpenBracFoundOfDo | DFoundOfDont | OFoundOfDont | NFoundOfDont | AposFoundOfDont | TFoundOfDont | OpenBracFoundOfDont | MFoundOfMul | UFoundOfMul | LFoundOfMul | OpenBracFoundOfMul | ArgStr1DigitFoundOfMul | CommaFoundOfMul | ArgStr2DigitFoundOfMul
data Expr = Mul Expr Expr | Plus Expr Expr | Val Int deriving (Show)

parse :: String -> Expr
parse s = go SeachingForMulOrDont s "" ""
  where
    go :: ParserState -> String -> String -> String -> Expr
    go _ [] _ _
        = Val 0
    
    go SeachingForMulOrDont (x:xs) argStr1 argStr2
        = case x of
            'm' -> go MFoundOfMul xs argStr1 argStr2
            'd' -> go DFoundOfDont xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go DFoundOfDont (x:xs) argStr1 argStr2
        = case x of
            'o' -> go OFoundOfDont xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go OFoundOfDont (x:xs) argStr1 argStr2
        = case x of
            'n' -> go NFoundOfDont xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go NFoundOfDont (x:xs) argStr1 argStr2
        = case x of
            '\'' -> go AposFoundOfDont xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go AposFoundOfDont (x:xs) argStr1 argStr2
        = case x of
            't' -> go TFoundOfDont xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go TFoundOfDont (x:xs) argStr1 argStr2
        = case x of
            '(' -> go OpenBracFoundOfDont xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go OpenBracFoundOfDont (x:xs) argStr1 argStr2
        = case x of
            ')' -> go MulDisabledSearchingForDo xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go MulDisabledSearchingForDo (x:xs) argStr1 argStr2
        = case x of
            'd' -> go DFoundOfDo xs argStr1 argStr2
            _   -> go MulDisabledSearchingForDo xs "" ""
    
    go DFoundOfDo (x:xs) argStr1 argStr2
        = case x of
            'o' -> go OFoundOfDo xs argStr1 argStr2
            _   -> go MulDisabledSearchingForDo xs "" ""
    
    go OFoundOfDo (x:xs) argStr1 argStr2
        = case x of
            '(' -> go OpenBracFoundOfDo xs argStr1 argStr2
            _   -> go MulDisabledSearchingForDo xs "" ""
    
    go OpenBracFoundOfDo (x:xs) argStr1 argStr2
        = case x of
            ')' -> go OpenBracFoundOfDo xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go MFoundOfMul (x:xs) argStr1 argStr2
        = case x of
            'u' -> go UFoundOfMul xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go UFoundOfMul (x:xs) argStr1 argStr2
        = case x of
            'l' -> go LFoundOfMul xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go LFoundOfMul (x:xs) argStr1 argStr2
        = case x of
            '(' -> go OpenBracFoundOfMul xs argStr1 argStr2
            _   -> go SeachingForMulOrDont xs "" ""
    
    go OpenBracFoundOfMul (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xs (argStr1++[x]) argStr2
            else go SeachingForMulOrDont xs "" ""
    
    go ArgStr1DigitFoundOfMul (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xs (argStr1++[x]) argStr2
            else if x == ','
                    then go CommaFoundOfMul xs argStr1 argStr2
                    else go SeachingForMulOrDont xs "" ""
    
    go CommaFoundOfMul (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xs argStr1 (argStr2++[x])
            else go SeachingForMulOrDont xs "" ""
    
    go ArgStr2DigitFoundOfMul (x:xs) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xs argStr1 (argStr2++[x])
            else if x == ')'
                    then Plus (Mul (Val $ read argStr1) (Val $ read argStr2)) (go SeachingForMulOrDont xs "" "")
                    else go SeachingForMulOrDont xs "" ""

evaluate :: Expr -> Int
evaluate (Mul x y) = evaluate x * evaluate y
evaluate (Plus x y) = evaluate x + evaluate y
evaluate (Val x) = x

day3part2 = do
  contents <- readFile "day3 (example).csv"
  let result = evaluate $ parse contents
  print result
