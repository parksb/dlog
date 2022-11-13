module Read (act) where

import Data.List

act :: String -> IO ()
act path = do
    logs <- readAll path
    putStrLn logs

readAll :: String -> IO String
readAll path = do
    logs <- fmap lines (readFile path)
    let splitted = map ((\(x, y) -> (read x :: Int, y)) . break (==';')) logs
    let sorted = sortBy gt splitted
    return $ (intercalate "\n" . map (\(x, _ : y) -> show x ++ "\t" ++ y)) sorted

gt :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
gt (a1, b1) (a2, b2) =
    case compare a1 a2 of
        EQ -> compare b1 b2
        LT -> LT
        GT -> GT

