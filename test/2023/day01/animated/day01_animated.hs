#!/usr/bin/env stack
{- stack --resolver lts-21.22 runghc
   --package containers-0.6.7
   --package ansi-terminal-0.11.5
-}

-- |
-- Animated visualization for Advent of Code 2023 Day 01 ("Trebuchet!")
-- built using the ascii-world library.
--
-- Each frame renders the calibration document inside an AsciiWorld grid:
--
--   • Row 1: Part 1 markers (first/last digit discovery, cursor, Part 2 alerts)
--   • Row 2: The raw calibration text
--   • Row 3: Part 2 digits discovered so far
--
-- The animation walks character-by-character, surfaces digit-word matches,
-- and reports running totals for both puzzle parts.
module Main where

import AsciiWorld
    ( AsciiWorld
    , MaskOrPointsIndex (MaskIndex, PointsIndex)
    , alterMask
    , emptyAsciiWorld
    , setPoint
    , showAsciiWorld
    )
import Control.Concurrent (threadDelay)
import Control.Exception (bracket_)
import Control.Applicative ((<|>))
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import Mask (bitwiseOr, pointToMask)
import System.Console.ANSI (clearFromCursorToScreenEnd, clearScreen, hideCursor, setCursorPosition, showCursor)
import System.Directory (doesFileExist)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

-------------------------------------------------------------------------------
-- Data types -----------------------------------------------------------------
-------------------------------------------------------------------------------

data Stage
    = StageScanning
    | StageSummary
    deriving (Eq, Show)

data DigitMark = DigitMark
    { dmPos :: Int
    , dmValue :: Int
    , dmLabel :: String
    }
    deriving (Eq, Show)

data LineRender = LineRender
    { lrText :: String
    , lrPointer :: Maybe Int
    , lrFirst :: Maybe DigitMark
    , lrLast :: Maybe DigitMark
    , lrNewFirst :: Maybe DigitMark
    , lrNewLast :: Maybe DigitMark
    , lrPart2Digits :: [DigitMark]
    , lrNewPart2 :: [DigitMark]
    }
    deriving (Eq, Show)

data LineFinal = LineFinal
    { lfText :: String
    , lfFirst :: Maybe DigitMark
    , lfLast :: Maybe DigitMark
    , lfPart2 :: [DigitMark]
    }
    deriving (Eq, Show)

data LineScanState = LineScanState
    { lssFirst :: Maybe DigitMark
    , lssLast :: Maybe DigitMark
    , lssPart2 :: [DigitMark]
    }
    deriving (Eq, Show)

data Frame = Frame
    { frLines :: [LineRender]
    , frStage :: Stage
    , frLineIdx :: Int
    , frCharIdx :: Maybe Int
    , frRunningBefore :: (Int, Int)
    , frRunningAfter :: (Int, Int)
    , frLinePart1 :: Maybe Int
    , frLinePart2 :: Maybe Int
    }
    deriving (Eq, Show)

data LayerKey
    = LayerTextChar Char
    | LayerPart1NewBoth
    | LayerPart1Both
    | LayerPart1NewFirst
    | LayerPart1First
    | LayerPart1NewLast
    | LayerPart1Last
    | LayerPart2Digit Char
    | LayerPart2NewDigit Char
    | LayerPart2Indicator
    deriving (Eq, Ord, Show)

data PointKey
    = CursorPoint
    deriving (Eq, Ord, Show)

data AnimationDimensions = AnimationDimensions
    { adWidth :: Int
    , adHeight :: Int
    , adLineCount :: Int
    }

-------------------------------------------------------------------------------
-- Constants ------------------------------------------------------------------
-------------------------------------------------------------------------------

digitPatterns :: [(String, Int, String)]
digitPatterns = digitStrings ++ digitWords
  where
    digitStrings = [([c], digitToInt c, [c]) | c <- ['0' .. '9']]
    digitWords =
        [ ("zero", 0, "zero")
        , ("one", 1, "one")
        , ("two", 2, "two")
        , ("three", 3, "three")
        , ("four", 4, "four")
        , ("five", 5, "five")
        , ("six", 6, "six")
        , ("seven", 7, "seven")
        , ("eight", 8, "eight")
        , ("nine", 9, "nine")
        ]

frameDelayMicros :: Int
frameDelayMicros = 70 * 1000

blockHeight :: Int
blockHeight = 3

-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    raw <- loadInput
    let calibrationLines = sanitizeLines raw
        lineCount = length calibrationLines
        width = maximum (0 : map length calibrationLines)
        dims = AnimationDimensions {adWidth = width, adHeight = blockHeight * max 1 lineCount, adLineCount = lineCount}
        (finalTotals, frames) = buildAnimation calibrationLines
        totalFrames = length frames
    if null calibrationLines
        then do
            putStrLn "Trebuchet! animation"
            putStrLn "No calibration lines found."
        else
            bracket_ hideCursor showCursor $ do
                clearScreen
                mapM_ (renderFrame dims totalFrames) (zip [1 ..] frames)
    putStrLn ""
    putStrLn "Trebuchet! animation finished."
    putStrLn $ "Processed " ++ show lineCount ++ " lines."
    putStrLn $ "Final totals -> Part 1: " ++ show (fst finalTotals) ++ ", Part 2: " ++ show (snd finalTotals)

-------------------------------------------------------------------------------
-- Loading and preprocessing --------------------------------------------------
-------------------------------------------------------------------------------

loadInput :: IO String
loadInput = do
    let preferred = "test/2023/day01/standard/day01 (example).csv"
    exists <- doesFileExist preferred
    if exists
        then readFile preferred
        else pure (unlines fallback)
  where
    fallback =
        [ "two1nine"
        , "eightwothree"
        , "abcone2threexyz"
        , "xtwone3four"
        , "4nineeightseven2"
        , "zoneight234"
        , "7pqrstsixteen"
        ]

sanitizeLines :: String -> [String]
sanitizeLines raw = filter (not . null) $ map stripCarriage (lines raw)
  where
    stripCarriage = filter (/= '\r')

-------------------------------------------------------------------------------
-- Animation building ---------------------------------------------------------
-------------------------------------------------------------------------------

buildAnimation :: [String] -> ((Int, Int), [Frame])
buildAnimation lines0 =
    let process (accFrames, totalsBefore, processedLines) (lineIdx, lineText) =
            let (lineFrames, lineFinal, totalsAfter) = framesForLine lines0 processedLines totalsBefore lineIdx lineText
            in (accFrames ++ lineFrames, totalsAfter, processedLines ++ [lineFinal])
        (frames, finalTotals, _) = foldl' process ([], (0, 0), []) (zip [0 ..] lines0)
    in (finalTotals, frames)

framesForLine :: [String] -> [LineFinal] -> (Int, Int) -> Int -> String -> ([Frame], LineFinal, (Int, Int))
framesForLine allLines processedBefore totalsBefore lineIdx lineText =
    let initialState = LineScanState Nothing Nothing []
        indices = if null lineText then [] else [0 .. length lineText - 1]
        (scanFrames, finalState) = buildScanFrames indices initialState
        lineFinal = LineFinal
            { lfText = lineText
            , lfFirst = lssFirst finalState
            , lfLast = lssLast finalState
            , lfPart2 = lssPart2 finalState
            }
        part1Value = valueFromMarks (lfFirst lineFinal) (lfLast lineFinal)
        part2Value = valueFromDigits (lfPart2 lineFinal)
        totalsAfter = (fst totalsBefore + part1Value, snd totalsBefore + part2Value)
        summaryFrame =
            Frame
                { frLines = assembleLines (renderProcessed processedBefore ++ [renderFinal lineFinal] ++ renderPending lineIdx)
                , frStage = StageSummary
                , frLineIdx = lineIdx
                , frCharIdx = Nothing
                , frRunningBefore = totalsBefore
                , frRunningAfter = totalsAfter
                , frLinePart1 = Just part1Value
                , frLinePart2 = Just part2Value
                }
    in (scanFrames ++ [summaryFrame], lineFinal, totalsAfter)
  where
    totalLines = length allLines

    renderProcessed :: [LineFinal] -> [LineRender]
    renderProcessed finals = map renderFinal finals

    renderPending :: Int -> [LineRender]
    renderPending idx = map renderPendingLine (drop (idx + 1) allLines)

    assembleLines :: [LineRender] -> [LineRender]
    assembleLines rendered =
        let completedCount = length rendered
            missing = totalLines - completedCount
        in if missing <= 0 then rendered else rendered ++ replicate missing emptyLine

    renderFinal :: LineFinal -> LineRender
    renderFinal lf =
        LineRender
            { lrText = lfText lf
            , lrPointer = Nothing
            , lrFirst = lfFirst lf
            , lrLast = lfLast lf
            , lrNewFirst = Nothing
            , lrNewLast = Nothing
            , lrPart2Digits = lfPart2 lf
            , lrNewPart2 = []
            }

    renderPendingLine :: String -> LineRender
    renderPendingLine txt =
        LineRender
            { lrText = txt
            , lrPointer = Nothing
            , lrFirst = Nothing
            , lrLast = Nothing
            , lrNewFirst = Nothing
            , lrNewLast = Nothing
            , lrPart2Digits = []
            , lrNewPart2 = []
            }

    emptyLine :: LineRender
    emptyLine = renderPendingLine ""

    buildScanFrames :: [Int] -> LineScanState -> ([Frame], LineScanState)
    buildScanFrames [] st = ([], st)
    buildScanFrames (idx : rest) st =
        let charAtIdx = if idx < length lineText then Just (lineText !! idx) else Nothing
            maybeDigit = do
                ch <- charAtIdx
                if isDigit ch
                    then Just DigitMark {dmPos = idx, dmValue = digitToInt ch, dmLabel = [ch]}
                    else Nothing
            newFirst = case (lssFirst st, maybeDigit) of
                (Nothing, Just d) -> Just d
                _ -> Nothing
            newLast = maybeDigit
            newPart2 = matchesAt idx lineText
            part2Digits = lssPart2 st ++ newPart2
            first' = lssFirst st <|> newFirst
            last' = case newLast of
                Just d -> Just d
                Nothing -> lssLast st
            state' = LineScanState {lssFirst = first', lssLast = last', lssPart2 = part2Digits}
            currentLineRender =
                LineRender
                    { lrText = lineText
                    , lrPointer = Just idx
                    , lrFirst = first'
                    , lrLast = last'
                    , lrNewFirst = newFirst
                    , lrNewLast = newLast
                    , lrPart2Digits = part2Digits
                    , lrNewPart2 = newPart2
                    }
            frame =
                Frame
                    { frLines = renderProcessed processedBefore ++ [currentLineRender] ++ renderPending lineIdx
                    , frStage = StageScanning
                    , frLineIdx = lineIdx
                    , frCharIdx = Just idx
                    , frRunningBefore = totalsBefore
                    , frRunningAfter = totalsBefore
                    , frLinePart1 = Nothing
                    , frLinePart2 = Nothing
                    }
            (restFrames, finalState) = buildScanFrames rest state'
        in (frame : restFrames, finalState)

-------------------------------------------------------------------------------
-- Rendering ------------------------------------------------------------------
-------------------------------------------------------------------------------

renderFrame :: AnimationDimensions -> Int -> (Int, Frame) -> IO ()
renderFrame dims totalFrames (frameIdx, frame) = do
    setCursorPosition 0 0
    putStrLn "AoC 2023 Day 01 - Trebuchet!"
    putStrLn $ "Frame " ++ show frameIdx ++ " / " ++ show totalFrames
    putStrLn $ describeStage dims frame
    putStrLn $ describeTotals frame
    putStrLn ""
    let world = frameToWorld dims frame
        ascii = showAsciiWorld (adHeight dims) ' ' maskToChar pointsToChar indexZOrder world
    putStr ascii
    putStrLn ""
    putStr (unlines (detailLines dims frame))
    clearFromCursorToScreenEnd
    threadDelay frameDelayMicros

maskToChar :: LayerKey -> Char
maskToChar (LayerTextChar c) = c
maskToChar LayerPart1NewBoth = 'B'
maskToChar LayerPart1Both = 'b'
maskToChar LayerPart1NewFirst = 'F'
maskToChar LayerPart1First = 'f'
maskToChar LayerPart1NewLast = 'L'
maskToChar LayerPart1Last = 'l'
maskToChar (LayerPart2Digit c) = c
maskToChar (LayerPart2NewDigit c) = c
maskToChar LayerPart2Indicator = '+'

pointsToChar :: PointKey -> Char
pointsToChar CursorPoint = 'v'

indexZOrder :: MaskOrPointsIndex LayerKey PointKey -> MaskOrPointsIndex LayerKey PointKey -> Ordering
indexZOrder a b = compare (priority a) (priority b)
  where
    priority (MaskIndex layer) = layerPriority layer
    priority (PointsIndex pointKey) = pointPriority pointKey

layerPriority :: LayerKey -> Int
layerPriority LayerPart1NewBoth = 80
layerPriority LayerPart1NewFirst = 70
layerPriority LayerPart1NewLast = 70
layerPriority LayerPart1Both = 60
layerPriority LayerPart1First = 50
layerPriority LayerPart1Last = 50
layerPriority LayerPart2Indicator = 40
layerPriority (LayerPart2NewDigit _) = 30
layerPriority (LayerPart2Digit _) = 20
layerPriority (LayerTextChar _) = 10

frameToWorld :: AnimationDimensions -> Frame -> AsciiWorld LayerKey PointKey
frameToWorld dims frame =
    foldl' addLine baseWorld (zip [0 ..] (frLines frame))
  where
    baseWorld = emptyAsciiWorld (adWidth dims)

    width = adWidth dims
    lineCount = adLineCount dims

    addLine world (idx, lr) =
        let yPart1 = rowY lineCount idx RowPart1
            yText = rowY lineCount idx RowText
            yPart2 = rowY lineCount idx RowPart2
            withText = foldl' (\w (x, c) -> insertLayer width (LayerTextChar c) (x, yText) w) world (zip [0 ..] (lrText lr))
            withCursor = maybe withText (\col -> setPoint CursorPoint (col, yPart1) withText) (lrPointer lr)
            withPart1 = addPart1Markers width withCursor lr yPart1
            withPart2 = addPart2Digits width withPart1 lr yPart2
        in foldl' (\w mark -> insertLayer width LayerPart2Indicator (dmPos mark, yPart1) w) withPart2 (lrNewPart2 lr)

insertLayer :: Int -> LayerKey -> (Int, Int) -> AsciiWorld LayerKey PointKey -> AsciiWorld LayerKey PointKey
insertLayer width key point = alterMask (\maybeMask -> Just $ bitwiseOr (fromMaybe 0 maybeMask) (pointToMask width point)) key

pointPriority :: PointKey -> Int
pointPriority CursorPoint = 90

addPart1Markers :: Int -> AsciiWorld LayerKey PointKey -> LineRender -> Int -> AsciiWorld LayerKey PointKey
addPart1Markers width acc lr yRow =
    case newBoth of
        Just mark -> insert LayerPart1NewBoth mark acc
        Nothing ->
            case existingBoth of
                Just mark -> insert LayerPart1Both mark acc
                Nothing -> foldl' applySingle acc updates
  where
    insert key mark world = insertLayer width key (dmPos mark, yRow) world

    newBoth = case (lrNewFirst lr, lrNewLast lr) of
        (Just f, Just l) | dmPos f == dmPos l -> Just f
        _ -> Nothing

    existingBoth =
        if maybe False (const True) newBoth
            then Nothing
            else case (lrFirst lr, lrLast lr) of
                (Just f, Just l) | dmPos f == dmPos l -> Just f
                _ -> Nothing

    updates =
        [ (LayerPart1NewFirst, lrNewFirst lr)
        , (LayerPart1First, if isJust (lrNewFirst lr) then Nothing else lrFirst lr)
        , (LayerPart1NewLast, lrNewLast lr)
        , (LayerPart1Last, if isJust (lrNewLast lr) then Nothing else lrLast lr)
        ]

    applySingle world (key, maybeMark) = maybe world (\mark -> insertLayer width key (dmPos mark, yRow) world) maybeMark

addPart2Digits :: Int -> AsciiWorld LayerKey PointKey -> LineRender -> Int -> AsciiWorld LayerKey PointKey
addPart2Digits width acc lr yRow =
    let baseDigits = foldl' (\w mark -> insertLayer width (LayerPart2Digit (markChar mark)) (dmPos mark, yRow) w) acc (lrPart2Digits lr)
    in foldl' (\w mark -> insertLayer width (LayerPart2NewDigit (markChar mark)) (dmPos mark, yRow) w) baseDigits (lrNewPart2 lr)
  where
    markChar mark = intToDigit (dmValue mark)

rowY :: Int -> Int -> RowType -> Int
rowY lineCount lineIdx rowType =
    let totalRows = lineCount * blockHeight
        offset = case rowType of
            RowPart1 -> 0
            RowText -> 1
            RowPart2 -> 2
    in (totalRows - 1) - (lineIdx * blockHeight + offset)

data RowType = RowPart1 | RowText | RowPart2

describeStage :: AnimationDimensions -> Frame -> String
describeStage dims frame =
    case frStage frame of
        StageScanning ->
            let lineNo = frLineIdx frame + 1
                focusInfo = case frCharIdx frame of
                    Just c -> "scanning line " ++ show lineNo ++ " / " ++ show (adLineCount dims) ++ ", column " ++ show (c + 1)
                    Nothing -> "scanning line " ++ show lineNo
            in "Stage: scanning - " ++ focusInfo
        StageSummary ->
            let lineNo = frLineIdx frame + 1
            in "Stage: summary - line " ++ show lineNo ++ " totals"

describeTotals :: Frame -> String
describeTotals frame =
    let before = frRunningBefore frame
        after = frRunningAfter frame
    in "Totals [before → after] Part1: " ++ show (fst before) ++ " → " ++ show (fst after)
        ++ ", Part2: " ++ show (snd before) ++ " → " ++ show (snd after)

detailLines :: AnimationDimensions -> Frame -> [String]
detailLines dims frame =
    case frStage frame of
        StageScanning -> scanningDetails
        StageSummary -> summaryDetails
  where
    scanningDetails =
        let lineNo = frLineIdx frame + 1
            renderers = frLines frame
            currentLine = renderers !! frLineIdx frame
            charInfo = case (frCharIdx frame, lrPointer currentLine) of
                (Just idx, _) ->
                    let ch = if idx < length (lrText currentLine) then [lrText currentLine !! idx] else " "
                    in "Character: column " ++ show (idx + 1) ++ " -> '" ++ ch ++ "'"
                _ -> "Character: (end)"
            newFirstInfo = case lrNewFirst currentLine of
                Just mark ->
                    "New Part 1 first digit at column " ++ show (dmPos mark + 1) ++ " -> " ++ show (dmValue mark)
                Nothing -> ""
            newLastInfo = case lrNewLast currentLine of
                Just mark ->
                    "Latest Part 1 digit at column " ++ show (dmPos mark + 1) ++ " -> " ++ show (dmValue mark)
                Nothing -> ""
            part2Info = case lrNewPart2 currentLine of
                [] -> ""
                marks ->
                    let describe mark =
                            "(" ++ show (dmPos mark + 1) ++ ": " ++ show (dmValue mark) ++ " from " ++ dmLabel mark ++ ")"
                    in "New Part 2 matches: " ++ unwords (map describe marks)
            summaryLine =
                "Line " ++ show lineNo ++ " digits so far -> Part1 first: " ++ describeDigit (lrFirst currentLine)
                    ++ ", last: " ++ describeDigit (lrLast currentLine)
                    ++ "; Part2 count: " ++ show (length (lrPart2Digits currentLine))
            extras = filter (not . null) [newFirstInfo, newLastInfo, part2Info]
        in ("Focus line: " ++ show lineNo ++ " / " ++ show (adLineCount dims)) : charInfo : extras ++ [summaryLine]

    summaryDetails =
        let lineNo = frLineIdx frame + 1
            part1Value = fromMaybe 0 (frLinePart1 frame)
            part2Value = fromMaybe 0 (frLinePart2 frame)
        in [ "Line " ++ show lineNo ++ " summary -> Part1 value: " ++ show part1Value ++ ", Part2 value: " ++ show part2Value ]

    describeDigit :: Maybe DigitMark -> String
    describeDigit Nothing = "(none)"
    describeDigit (Just mark) = show (dmValue mark) ++ " @ col " ++ show (dmPos mark + 1)

-------------------------------------------------------------------------------
-- Digit detection ------------------------------------------------------------
-------------------------------------------------------------------------------

matchesAt :: Int -> String -> [DigitMark]
matchesAt idx lineText =
    [ DigitMark {dmPos = idx, dmValue = val, dmLabel = label}
    | (pattern, val, label) <- digitPatterns
    , idx + length pattern <= length lineText
    , take (length pattern) (drop idx lineText) == pattern
    ]

valueFromMarks :: Maybe DigitMark -> Maybe DigitMark -> Int
valueFromMarks (Just firstMark) (Just lastMark) = dmValue firstMark * 10 + dmValue lastMark
valueFromMarks _ _ = 0

valueFromDigits :: [DigitMark] -> Int
valueFromDigits [] = 0
valueFromDigits marks =
    let firstVal = dmValue (head marks)
        lastVal = dmValue (last marks)
    in firstVal * 10 + lastVal

-------------------------------------------------------------------------------
-- Utilities ------------------------------------------------------------------
-------------------------------------------------------------------------------

