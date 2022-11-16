module Update (act) where

import qualified Log (fexists, fupdate)

act :: (String, String) -> IO ()
act (ymd, txt) = act' (read ymd, txt)

act' :: (Int, String) -> IO ()
act' (ymd, txt) =
  Log.fexists ymd >>= \isExists ->
    if not isExists
      then putStrLn "Log not found."
      else Log.fupdate (ymd, txt)
