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
import Data.Map as M hiding (map, filter, drop)
import Data.Maybe (fromJust)


-------------
-- Program --
-------------
main = day3part2

data ParserState = SeachingForMulOrDont | MulDisabledSearchingForDo | UpToOpenBracFoundOfMul | ArgStr1DigitFoundOfMul | CommaFoundOfMul | ArgStr2DigitFoundOfMul
data Expr = Mul Expr Expr | Plus Expr Expr | Val Integer deriving (Show)

parse :: String -> Expr
parse s = go SeachingForMulOrDont s "" ""
  where
    go :: ParserState -> String -> String -> String -> Expr
    go _ [] _ _
        = Val 0
    go SeachingForMulOrDont xs@(x:xsTail) argStr1 argStr2
        = if and (zipWith (==) "mul(" xs)
            then go UpToOpenBracFoundOfMul (drop (length "mul(") xs) argStr1 argStr2
            else if and (zipWith (==) "don't()" xs)
                    then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go MulDisabledSearchingForDo xs@(x:xsTail) argStr1 argStr2
        = if and (zipWith (==) "do()" xs)
            then go SeachingForMulOrDont (drop (length "do()") xs) argStr1 argStr2
            else go MulDisabledSearchingForDo xsTail "" ""
    go UpToOpenBracFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xsTail (argStr1++[x]) argStr2
            else if and (zipWith (==) "don't()" xs)
                    then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go ArgStr1DigitFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xsTail (argStr1++[x]) argStr2
            else if x == ','
                    then go CommaFoundOfMul xsTail argStr1 argStr2
                    else if and (zipWith (==) "don't()" xs)
                            then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                            else go SeachingForMulOrDont xsTail "" ""
    go CommaFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xsTail argStr1 (argStr2++[x])
            else if and (zipWith (==) "don't()" xs)
                    then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go ArgStr2DigitFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xsTail argStr1 (argStr2++[x])
            else if x == ')'
                    then Plus (Mul (Val $ read argStr1) (Val $ read argStr2)) (go SeachingForMulOrDont xsTail "" "")
                    else if and (zipWith (==) "don't()" xs)
                            then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                            else go SeachingForMulOrDont xsTail "" ""

evaluate :: Expr -> Integer
evaluate (Mul x y) = evaluate x * evaluate y
evaluate (Plus x y) = evaluate x + evaluate y
evaluate (Val x) = x

day3part2 = do
  contents <- readFile "day3 (data).csv"
  let result = parse contents
  print $ evaluate result
