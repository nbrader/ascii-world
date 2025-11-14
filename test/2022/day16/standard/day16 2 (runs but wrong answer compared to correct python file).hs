#!/usr/bin/env stack
-- stack --resolver lts-21.9 ghci --package regex-tdfa --package containers --package split --package mtl --package strict --package search-algorithms

import Data.List
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Data.Map.Strict (Map)
import Text.Regex.TDFA
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Graph = Map.Map String (S.Set String)
type Flow = Map.Map String Int
type EncodedValves = [(String, Int)]
type Visited = [Int]
type Path = [String]
type Outcomes = Map.Map [Int] (Int, Path)

readFlow :: String -> Int
readFlow x =
  let matches :: AllTextMatches [] String
      matches = x =~ "\\d+" :: AllTextMatches [] String
  in case getAllTextMatches matches of
       (n:_) -> read n :: Int
       []    -> 0

dp :: String -> Int -> Visited -> Path -> Int -> Outcomes -> Graph -> Flow -> EncodedValves -> Outcomes
dp current remainingTime visited path currentFlow outcomes graph flow encodedValves =
    let visitedTuple = map (\x -> if x > 0 then 1 else 0) visited
        newOutcomes = case Map.lookup visitedTuple outcomes of
            Just (flow', _) -> if currentFlow > flow' then Map.insert visitedTuple (currentFlow, path) outcomes else outcomes
            Nothing -> Map.insert visitedTuple (currentFlow, path) outcomes
    in foldl' (\acc (dest, index) ->
        let timeAfterDest = remainingTime - if S.member dest (fromJust (Map.lookup current graph)) then 1 else maxBound
        in if visited !! index > 0 || timeAfterDest <= 0
            then acc
            else let newVisited = take index visited ++ [length path + 1] ++ drop (index + 1) visited
                     newPath = path ++ [dest]
                     newFlow = currentFlow + timeAfterDest * (Map.findWithDefault 0 dest flow)
                 in dp dest timeAfterDest newVisited newPath newFlow acc graph flow encodedValves
    ) newOutcomes encodedValves

getGraphFromFile :: FilePath -> IO Graph
getGraphFromFile inputFilePath = do
    content <- readFile inputFilePath
    let linesInfos = map (getAllTextMatches . (=~ "[A-Z][A-Z]|\\d+")) (lines content)
    return $ Map.fromList [(head x, S.fromList (drop 2 x)) | x <- linesInfos]

main :: IO ()
main = do
    graph <- getGraphFromFile "day16 (data).csv"
    let flow = Map.fromList [(x, readFlow x) | x <- Map.keys graph]
    let encodedValves = zip (Map.keys flow) [0..]
    let initialVisited = replicate (length encodedValves) 0
    let initialPath = []

    let outcomes1 = dp "AA" 30 initialVisited initialPath 0 Map.empty graph flow encodedValves
    let (maxFlowed1, bestPath1) = maximumBy (comparing fst) (Map.elems outcomes1)

    putStrLn $ "Max flow for maxFlowedProblemPart1: " ++ show maxFlowed1
    putStrLn $ "Best path: " ++ show bestPath1

    let outcomes2 = dp "AA" 26 initialVisited initialPath 0 Map.empty graph flow encodedValves
    let (maxFlowed2, bestPaths2) = maximumBy (comparing fst) (Map.elems outcomes2)

    putStrLn $ "Max flow for maxFlowedProblemPart2: " ++ show maxFlowed2
    putStrLn $ "Best paths: " ++ show bestPaths2
