module Create (act) where

import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay (DayOfYear)
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Log (exists, writeToFile)

act :: Maybe String -> IO ()
act Nothing = currentYmd >>= \ymd -> act' (read ymd)
act (Just ymd) = act' (read ymd)

act' :: Int -> IO ()
act' ymd =
  Log.exists ymd >>= \isExists ->
    if isExists
      then putStrLn "Log already exists."
      else
        putStrLn "What happened?" >> getLine >>= \txt ->
          Log.writeToFile (ymd, txt)

currentYmd :: IO String
currentYmd =
  getCurrentTime >>= \time ->
    getCurrentTimeZone >>= \tz ->
      return $ ymd (date (utcToLocalTime tz time))

date :: LocalTime -> (Year, MonthOfYear, DayOfYear)
date n = toGregorian (localDay n)

ymd :: (Year, MonthOfYear, DayOfYear) -> String
ymd (y, m, d) = show y ++ show m ++ show d
