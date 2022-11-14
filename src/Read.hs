module Read (act) where

import Data.Functor
import Data.List
import qualified Log (exists, readFromFile)

act :: IO ()
act = readAll >>= \logs -> putStrLn logs

readAll :: IO String
readAll =
  Log.readFromFile
    <&> intercalate "\n" . map (\(x, _ : y) -> show x ++ "\t" ++ y)
