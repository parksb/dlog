module Main where

import System.Environment as Env

import qualified Create (act)
import qualified Read (act)
import qualified Update (act)
import qualified Delete (act)

main :: IO ()
main = do
    args <- Env.getArgs
    act args

act :: [String] -> IO ()
act ["-c"] = Create.act
act ["-r"] = Read.act
act ["-u"] = Update.act
act ["-d"] = Delete.act
act _ = help

help :: IO ()
help = putStrLn "Unkown argument"

