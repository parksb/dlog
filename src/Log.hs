module Log (writeToFile, readFromFile, exists) where

import Data.List
import Data.Functor
import System.Directory

type Log = (Int, String)

writeToFile :: Log -> IO ()
writeToFile log = do
    readFromFile >>= \logs ->
        writeFile tempPath $ intercalate "\n" $ map format (insertLog log logs)
    >> readFile tempPath >>= \raws -> writeFile path raws
    >> removeFile tempPath
    where format (ymd, txt) = show ymd ++ ";" ++ txt

readFromFile :: IO [Log]
readFromFile =
    readRaws <&> map ((\(x, _ : y) -> (read x :: Int, y)) . break (== ';'))

exists :: Int -> IO Bool
exists ymd = readRaws <&> any (\raw -> show ymd `isPrefixOf` raw)

readRaws :: IO [String]
readRaws = readFile path <&> lines

insertLog :: Log -> [Log] -> [Log]
insertLog log [] = [log]
insertLog (ymd, txt) ((hYmd, hTxt):t)
    | ymd < hYmd = (ymd, txt) : (hYmd, hTxt) : t
    | otherwise = (hYmd, hTxt) : insertLog (ymd, txt) t

path = "logs.dlog"
tempPath = "logs.tmp.dlog"

