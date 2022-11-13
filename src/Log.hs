module Log (writeLog, readLogs, exists) where

import Data.List
import Data.Functor

type Log = (Int, String)

writeLog :: Log -> IO ()
writeLog (ymd, txt) =
    appendFile path (show ymd ++ ";" ++ txt ++ "\n")

readLogs :: IO [Log]
readLogs =
    readRaws <&> map ((\(x, y) -> (read x :: Int, y)) . break (== ';'))

exists :: Int -> IO Bool
exists ymd = readRaws <&> any (\raw -> show ymd `isPrefixOf` raw)

readRaws :: IO [String]
readRaws = readFile path <&> lines

path :: String
path = "logs.txt"
