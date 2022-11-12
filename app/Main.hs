module Main where

import System.Environment as Env

import qualified Create (act)
import qualified Read (act)
import qualified Update (act)
import qualified Delete (act)

main :: IO ()
main = do
    let path = "logs.txt"
    args <- Env.getArgs
    act args path

act :: [String] -> String -> IO ()
act ["-c"] path = Create.act Nothing path
act ["-c", ymd] path = Create.act (Just ymd) path
act ["-r"] path = Read.act
act ["-u"] path = Update.act
act ["-d"] path = Delete.act
act _ _ = help

help :: IO ()
help = putStrLn "Unknown argument"

