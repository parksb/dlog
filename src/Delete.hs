module Delete (act) where

import qualified Log (fdelete, fexists)

act :: String -> IO ()
act ymd = act' (read ymd)

act' :: Int -> IO ()
act' ymd =
  Log.fexists ymd >>= \isExists ->
    if not isExists
      then putStrLn "Log not found."
      else Log.fdelete ymd
