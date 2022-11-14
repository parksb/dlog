module Delete (act) where

import qualified Log (exists, removeFromFile)

act :: String -> IO ()
act ymd = act' (read ymd)

act' :: Int -> IO ()
act' ymd =
  Log.exists ymd >>= \isExists ->
    if not isExists
      then putStrLn "Log not found."
      else Log.removeFromFile ymd
