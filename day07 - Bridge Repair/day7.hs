#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1

---------------------------------
---------------------------------
----  Day 7:  Bridge Repair  ----
---------------------------------
---------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 -- '.\day7.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day7part1
-- 303766880536

-- *Main> day7part2
-- 337041851384440


-------------
-- Imports --
-------------
import Data.List
import Data.List.Split
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import Data.HashSet as H hiding (map, foldl', filter)
import Data.Either (lefts, rights)


-------------
-- Program --
-------------
main = day7part1

readResultAndOperands :: String -> (String,[String])
readResultAndOperands inStr = (result, operands)
  where result = takeWhile (/= ':') inStr
        operands = splitOn " " $ drop 2 $ dropWhile (/= ':') inStr

data Expr = Mul Expr Expr | Plus Expr Expr | Concat Expr Expr | Val String deriving (Show, Eq)

evalExpr :: Expr -> String
evalExpr (Mul x y) = show (readInt (evalExpr x) * readInt (evalExpr y))
evalExpr (Plus x y) = show (readInt (evalExpr x) + readInt (evalExpr y))
evalExpr (Concat x y) = evalExpr x ++ evalExpr y
evalExpr (Val x) = x

exprsFromOperands1 :: [String] -> [Expr]
exprsFromOperands1 [] = error "Invalid! exprsFromOperands1 took arg []"
exprsFromOperands1 (x:[]) = [Val x]
exprsFromOperands1 (x:xs) = do
    op <- [Mul,Plus]
    map (`op` Val x) (exprsFromOperands1 xs)

exprsFromOperands2 :: [String] -> [Expr]
exprsFromOperands2 [] = error "Invalid! exprsFromOperands2 took arg []"
exprsFromOperands2 (x:[]) = [Val x]
exprsFromOperands2 (x:xs) = do
    op <- [Mul,Plus,Concat]
    map (`op` Val x) (exprsFromOperands2 xs)
    
readInt :: String -> Int
readInt = read

day7part1 = do
    contents <- readFile "day7 (data).csv"
    let fileRows  = lines contents
    let listOfResultAndOperandsPerRow = map readResultAndOperands fileRows
    
    print . sum . map readInt . map fst . filter (\(result,evals) -> any (==result) evals) . map (fmap (map evalExpr . exprsFromOperands1 . reverse)) $ listOfResultAndOperandsPerRow

day7part2 = do
    contents <- readFile "day7 (data).csv"
    let fileRows  = lines contents
    let listOfResultAndOperandsPerRow = map readResultAndOperands fileRows
    
    print . sum . map readInt . map fst . filter (\(result,evals) -> any (==result) evals) . map (fmap (map evalExpr . exprsFromOperands2 . reverse)) $ listOfResultAndOperandsPerRow
