module Update (act) where

import qualified Log (exists, updateToFile)

act :: (String, String) -> IO ()
act (ymd, txt) = act' (read ymd, txt)

act' :: (Int, String) -> IO ()
act' (ymd, txt) =
  Log.exists ymd >>= \isExists ->
    if not isExists
      then putStrLn "Log not found."
      else Log.updateToFile (ymd, txt)
