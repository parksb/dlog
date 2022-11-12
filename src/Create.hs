module Create (act) where

import System.Environment
import System.IO
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay (DayOfYear)
import Data.Time.LocalTime
import Data.List

act :: Maybe String -> String -> IO ()
act Nothing path = do
    ymd <- currentYmd
    act' ymd path
act (Just ymd) path = act' ymd path

act' :: String -> String -> IO ()
act' ymd path = do
    logs <- fmap lines (readFile path)
    if exists logs ymd then putStrLn "Log already exists."
    else do
        log <- putStrLn "What happened?" >> getLine
        appendFile path (ymd ++ ";" ++ log ++ "\n")

currentYmd :: IO String
currentYmd = do
    t <- getCurrentTime
    tz <- getCurrentTimeZone
    return (ymd (date (utcToLocalTime tz t)))

exists :: [String] -> String -> Bool
exists s t = any (\x -> t `isPrefixOf` x) s

date :: LocalTime -> (Year, MonthOfYear, DayOfYear)
date n = toGregorian (localDay n)

ymd :: (Year, MonthOfYear, DayOfYear) -> String
ymd (y, m, d) = show y ++ show m ++ show d

