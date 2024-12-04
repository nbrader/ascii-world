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
-- 167090022

-- *Main> day3part2
-- 89823704


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

parse1 :: String -> Expr
parse1 s = go SeachingForMulOrDont s "" ""
  where
    go :: ParserState -> String -> String -> String -> Expr
    go _ [] _ _
        = Val 0
    go SeachingForMulOrDont xs@(x:xsTail) argStr1 argStr2
        = if "mul(" `isPrefixOf` xs
            then go UpToOpenBracFoundOfMul (drop (length "mul(") xs) argStr1 argStr2
            else go SeachingForMulOrDont xsTail "" ""
    go UpToOpenBracFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xsTail (argStr1++[x]) argStr2
            else go SeachingForMulOrDont xsTail "" ""
    go ArgStr1DigitFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xsTail (argStr1++[x]) argStr2
            else if x == ','
                    then go CommaFoundOfMul xsTail argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go CommaFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xsTail argStr1 (argStr2++[x])
            else go SeachingForMulOrDont xsTail "" ""
    go ArgStr2DigitFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xsTail argStr1 (argStr2++[x])
            else if x == ')'
                    then Plus (Mul (Val $ read argStr1) (Val $ read argStr2)) (go SeachingForMulOrDont xsTail "" "")
                    else go SeachingForMulOrDont xsTail "" ""

parse2 :: String -> Expr
parse2 s = go SeachingForMulOrDont s "" ""
  where
    go :: ParserState -> String -> String -> String -> Expr
    go _ [] _ _
        = Val 0
    go SeachingForMulOrDont xs@(x:xsTail) argStr1 argStr2
        = if "mul(" `isPrefixOf` xs
            then go UpToOpenBracFoundOfMul (drop (length "mul(") xs) argStr1 argStr2
            else if "don't()" `isPrefixOf` xs
                    then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go MulDisabledSearchingForDo xs@(x:xsTail) argStr1 argStr2
        = if "do()" `isPrefixOf` xs
            then go SeachingForMulOrDont (drop (length "do()") xs) argStr1 argStr2
            else go MulDisabledSearchingForDo xsTail "" ""
    go UpToOpenBracFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xsTail (argStr1++[x]) argStr2
            else if "don't()" `isPrefixOf` xs
                    then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go ArgStr1DigitFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr1DigitFoundOfMul xsTail (argStr1++[x]) argStr2
            else if x == ','
                    then go CommaFoundOfMul xsTail argStr1 argStr2
                    else if "don't()" `isPrefixOf` xs
                            then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                            else go SeachingForMulOrDont xsTail "" ""
    go CommaFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xsTail argStr1 (argStr2++[x])
            else if "don't()" `isPrefixOf` xs
                    then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                    else go SeachingForMulOrDont xsTail "" ""
    go ArgStr2DigitFoundOfMul xs@(x:xsTail) argStr1 argStr2
        = if isDigit x
            then go ArgStr2DigitFoundOfMul xsTail argStr1 (argStr2++[x])
            else if x == ')'
                    then Plus (Mul (Val $ read argStr1) (Val $ read argStr2)) (go SeachingForMulOrDont xsTail "" "")
                    else if "don't()" `isPrefixOf` xs
                            then go MulDisabledSearchingForDo (drop (length "don't()") xs) argStr1 argStr2
                            else go SeachingForMulOrDont xsTail "" ""

evaluate :: Expr -> Integer
evaluate (Mul x y) = evaluate x * evaluate y
evaluate (Plus x y) = evaluate x + evaluate y
evaluate (Val x) = x

day3part1 = do
  contents <- readFile "day3 (data).csv"
  let result = parse1 contents
  print $ evaluate result

day3part2 = do
  contents <- readFile "day3 (data).csv"
  let result = parse2 contents
  print $ evaluate result
