module Log (fcreate, fread, fupdate, fdelete, fexists) where

import Data.Functor
import Data.List
import System.Directory

type Log = (Int, String)

fcreate :: Log -> IO ()
fcreate = manipulateFile . f
  where
    f :: Log -> [Log] -> [Log]
    f log [] = [log]
    f (ymd, txt) ((hYmd, hTxt) : t)
      | ymd < hYmd = (ymd, txt) : (hYmd, hTxt) : t
      | otherwise = (hYmd, hTxt) : f (ymd, txt) t

fdelete :: Int -> IO ()
fdelete = manipulateFile . f
  where
    f :: Int -> [Log] -> [Log]
    f _ [] = []
    f ymd ((hYmd, hTxt) : t)
      | ymd == hYmd = t
      | otherwise = (hYmd, hTxt) : f ymd t

fupdate :: Log -> IO ()
fupdate = manipulateFile . f
  where
    f :: Log -> [Log] -> [Log]
    f _ [] = []
    f (ymd, txt) ((hYmd, hTxt) : t)
      | ymd == hYmd = (ymd, txt) : t
      | otherwise = (hYmd, hTxt) : f (ymd, txt) t

fread :: IO [Log]
fread =
  freadRaws <&> map ((\(x, _ : y) -> (read x :: Int, y)) . break (== ';'))

fexists :: Int -> IO Bool
fexists ymd = freadRaws <&> any (\raw -> show ymd `isPrefixOf` raw)

manipulateFile :: ([Log] -> [Log]) -> IO ()
manipulateFile f =
  fread >>= \logs ->
    writeFile tmpPath (showAll (f logs))
      >> readFile tmpPath
      >>= writeFile path
      >> removeFile tmpPath

freadRaws :: IO [String]
freadRaws = readFile path <&> lines

showAll :: [Log] -> String
showAll = intercalate "\n" . map (\(ymd, txt) -> show ymd ++ ";" ++ txt)

path = "logs.dlog"

tmpPath = "logs.tmp.dlog"
