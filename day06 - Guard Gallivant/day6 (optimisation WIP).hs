#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package split-0.2.3.5 --package strict-0.4.0.1 --package unordered-containers-0.2.19.1

-----------------------------------
-----------------------------------
----  Day 6:  Guard Gallivant  ----
-----------------------------------
-----------------------------------
{-
    To build, run the following shell command in this directory:
        stack --resolver lts-20.5 ghc --package strict-0.4.0.1 --package unordered-containers-0.2.19.1 -- '.\day6.hs' -O2
-}

------------
-- Output --
------------
-- *Main> day6part1
-- 4973

-- *Main> day6part2
-- 1482


-------------
-- Imports --
-------------
import Data.List
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import Data.HashSet as H hiding (map, foldl', filter)
import Data.Either (lefts, rights)
import Debug.Trace (trace)


-------------
-- Program --
-------------
main = day6part2

day6part1 = do
    contents <- readFile "day6 (data).csv"
    let fileRows  = lines contents
    let world :: World
        world = readWorld fileRows
    
    let walkedWorld = walkPath world
    
    let uniquePoints = nub $ map fst (guardHistory walkedWorld)
    
    print $ length uniquePoints
    -- putStrLn ""
    -- putStrLn $ showWorld (dims world) world pathPoints

day6part2 = do
    contents <- readFile "day6 (example).csv"
    let fileRows  = lines contents
    let world :: World
        world = readWorld fileRows
    
    let walkedWorldWithoutExtraWall = walkPath world
        
    let blockedWorldsWithCycles = everyBlockedWorldWithCycle (getPrevWorld walkedWorldWithoutExtraWall)
    
    print walkedWorldWithoutExtraWall
    print $ length blockedWorldsWithCycles
    -- putStrLn ""
    
    -- print $ length pathPoints
    -- mapM_ print $ zipWith const [0..] $ map (\w -> let newPathPoints = walkPathUntilExitOrReachesCycle w
                                                    -- in showWorld (dims w) w newPathPoints) cycleWorlds
    
    -- mapM_ (\w -> let newPathPoints = walkPathUntilExitOrReachesCycle w
                 -- in do
                    -- putStrLn ""
                    -- putStrLn $ showWorld (dims w) w newPathPoints) cycleWorlds

-- everyPathAndWorldWithCycle:
--      A faster way of calculating the looping worlds would be to first generate the original walkedPath and feed this
--          into a function which, if it's the empty path returns the empty list and otherwise concat the following:
--              1. if the head position of the path doesn't exist earlier in that path and if the list the continuation
--                         of the walk of the tail loops in a world where a wall is where the head was then a singleton
--                         list of that continued path and the world with the new world.
--                         Otherwise return the empty list.
--              2. itself applied to the tail
-- 
--        This should result in a list of all worlds that obstructed the original path with a single new wall
everyBlockedWorldWithCycle :: World -> [World]
everyBlockedWorldWithCycle world
    | guardHistory prevWorld == [] = []
    | otherwise = first ++ second
  where path = guardHistory world
        pathHead = head path
        pathTail = tail path
        pathTailHead = head pathTail
        
        pathHeadPos = fst pathHead
        pathTailPositions = map fst pathTail
        
        blockedWorld = (floorToExtraWall pathHeadPos world) {guardHistory = tail path}
        prevWorld = getPrevWorld world
        
        (walkedWorldBlockedWorld, blockedWorldReachesCycle) = walkedWorldAndWhetherReachesCycle blockedWorld
        
        first | not (pathHeadPos `elem` pathTailPositions) && blockedWorldReachesCycle = trace (showWorld walkedWorldBlockedWorld) $ [walkedWorldBlockedWorld]
              | otherwise = trace ("\nDoesn't Reach Cycle:" ++ showWorld walkedWorldBlockedWorld) $ []
        
        second = everyBlockedWorldWithCycle prevWorld

getPrevWorld :: World -> World
getPrevWorld w = w {guardHistory = tail $ guardHistory w}

addV2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)

type Point = (Integer, Integer)
data World = World {
    guardHistory  :: [(Point, Dir)],
    floors :: H.HashSet Point,
    initWalls  :: H.HashSet Point,
    extraWalls :: H.HashSet Point,
    dims   :: (Integer, Integer)} deriving (Show)

walls :: World -> H.HashSet Point
walls w = initWalls w `H.union` extraWalls w

type Floor = Point
type Wall  = Point
type Start  = Point
readWorld :: [String] -> World
readWorld rows = foldl' insertFloorOrWall emptyWorldWithStart floorAndWalls
  where floorAndWallsAndStart :: [Either (Either Floor Wall) Start]
        floorAndWallsAndStart = toFloorsAndWallsAndStart rows
        
        floorAndWalls = lefts floorAndWallsAndStart
        
        emptyWorldWithStart :: World
        emptyWorldWithStart = World {
            guardHistory  = [(head $ rights floorAndWallsAndStart, U)],
            floors = H.fromList [],
            initWalls  = H.fromList [],
            extraWalls = H.fromList [],
            dims   = (genericLength (head rows), genericLength rows)
            }
        
        toFloorsAndWallsAndStart :: [String] -> [Either (Either Floor Wall) Start]
        toFloorsAndWallsAndStart rows = concat [readChar c (colNum, rowNum) | (rowNum, row) <- zip [0..] rows, (colNum,c) <- zip [0..] row, case c of {' ' -> False; _ -> True}]
          where readChar '.' pos = [Left (Left  pos)]
                readChar '#' pos = [Left (Right pos)]
                readChar '^' pos = [Left (Left pos), Right pos]
        
        insertFloorOrWall :: World -> Either Floor Wall -> World
        insertFloorOrWall world (Left  f) = world {floors    = H.insert f $ floors    world}
        insertFloorOrWall world (Right w) = world {initWalls = H.insert w $ initWalls world}

data Dir = R | U | L | D deriving (Show, Eq)
rotR90 :: Dir -> Dir

rotR90 R = D
rotR90 U = R
rotR90 L = U
rotR90 D = L

fromDir R = ( 1, 0)
fromDir U = ( 0,-1)
fromDir L = (-1, 0)
fromDir D = ( 0, 1)

inWorld :: Point -> World -> Bool
inWorld pos world = (   pos `H.member` floors    world
                     || pos `H.member` initWalls world )

floorToExtraWall :: Point -> World -> World
floorToExtraWall extraWall world
    = world {
        floors     = H.delete extraWall $ floors world,
        extraWalls = H.insert extraWall $ extraWalls world
        }

walkPath :: World -> World
walkPath world = until guardNotInWorld doMove world
  where doMove w =  let (pos,dir) = head $ guardHistory w
                        newPosIgnoringWalls = pos `addV2` fromDir dir
                        newGuard = if newPosIgnoringWalls `H.member` walls world
                                    then (pos, rotR90 dir)
                                    else (newPosIgnoringWalls, dir)
                    in w {guardHistory = newGuard : guardHistory w}

walkedWorldAndWhetherReachesCycle :: World -> (World, Bool)
walkedWorldAndWhetherReachesCycle world = (worldAtExitOrLoop, not $ guardNotInWorld world)
  where doMove w =  let (pos,dir) = head $ guardHistory w
                        newPosIgnoringWalls = pos `addV2` fromDir dir
                        newGuard = if newPosIgnoringWalls `H.member` walls world
                                    then (pos, rotR90 dir)
                                    else (newPosIgnoringWalls, dir)
                    in w {guardHistory = newGuard : guardHistory w}
        
        worldAtExitOrLoop = until guardNotInWorldOrReachesCycle doMove world

guardNotInWorld :: World -> Bool
guardNotInWorld world = (\((pos,dir):tailPath) -> not (pos `inWorld` world)) . guardHistory $ world

guardNotInWorldOrReachesCycle :: World -> Bool
guardNotInWorldOrReachesCycle world = (\((pos,dir):tailPath) -> not (pos `inWorld` world) || (pos,dir) `elem` tailPath) . guardHistory $ world

showWorld world = ("\n" ++) $ intercalate "\n" . reverse $ [[case find ((==(x,y)) . fst) pathPoints of
                                                                            Just (_,R) -> '>'
                                                                            Just (_,U) -> '^'
                                                                            Just (_,L) -> '<'
                                                                            Just (_,D) -> 'V'
                                                                            Nothing -> (if (x,y) `H.member` floors world then '.' else (if (x,y) `H.member` initWalls world then '#' else (if (x,y) `H.member` extraWalls world then 'O' else ' '))) | x <- [0..(dimX-1)]] | y <- reverse [0..(dimY-1)]]
  where pathPoints = guardHistory world
        (dimX, dimY) = dims world
