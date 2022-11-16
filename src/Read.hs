module Read (act) where

import Data.Functor
import Data.List
import qualified Log (fread)

act :: IO ()
act = readAll >>= \logs -> putStrLn logs

readAll :: IO String
readAll =
  Log.fread
    <&> intercalate "\n" . map (\(x, y) -> show x ++ "\t" ++ y)
