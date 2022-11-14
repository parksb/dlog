module Read (act) where

import Data.List
import Data.Functor

import qualified Log (readFromFile, exists)

act :: IO ()
act = readAll >>= \logs -> putStrLn logs

readAll :: IO String
readAll =
    Log.readFromFile <&>
    intercalate "\n" . map (\(x, _ : y) -> show x ++ "\t" ++ y)

