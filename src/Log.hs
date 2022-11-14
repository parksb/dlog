module Log (writeToFile, readFromFile, removeFromFile, exists) where

import Data.Functor
import Data.List
import System.Directory
import System.Mem (performGC)

type Log = (Int, String)

writeToFile :: Log -> IO ()
writeToFile = manipulateFile . insertLog

removeFromFile :: Int -> IO ()
removeFromFile = manipulateFile . removeLog

manipulateFile :: ([Log] -> [Log]) -> IO ()
manipulateFile f =
  readFromFile >>= \logs ->
    writeFile tmpPath (showAll (f logs))
      >> readFile tmpPath
      >>= writeFile path
      >> removeFile tmpPath

readFromFile :: IO [Log]
readFromFile =
  readRaws <&> map ((\(x, _ : y) -> (read x :: Int, y)) . break (== ';'))

exists :: Int -> IO Bool
exists ymd = readRaws <&> any (\raw -> show ymd `isPrefixOf` raw)

readRaws :: IO [String]
readRaws = readFile path <&> lines

insertLog :: Log -> [Log] -> [Log]
insertLog log [] = [log]
insertLog (ymd, txt) ((hYmd, hTxt) : t)
  | ymd < hYmd = (ymd, txt) : (hYmd, hTxt) : t
  | otherwise = (hYmd, hTxt) : insertLog (ymd, txt) t

removeLog :: Int -> [Log] -> [Log]

remoevLog _ [] = []

removeLog ymd ((hYmd, hTxt) : t)
  | ymd == hYmd = t
  | otherwise = (hYmd, hTxt) : removeLog ymd t

showAll :: [Log] -> String
showAll = intercalate "\n" . map format
  where
    format :: Log -> String
    format (ymd, txt) = show ymd ++ ";" ++ txt

path = "logs.dlog"

tmpPath = "logs.tmp.dlog"
