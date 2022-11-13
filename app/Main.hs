module Main where

import System.Environment as Env

import qualified Create (act)
import qualified Read (act)
import qualified Update (act)
import qualified Delete (act)

main :: IO ()
main = Env.getArgs >>= \args -> command args

command :: [String] -> IO ()
command ["-c"] = Create.act Nothing
command ["-c", ymd] = Create.act (Just ymd)
command ["-r"] = Read.act
command ["-u"] = Update.act
command ["-d"] = Delete.act
command _ = help

help :: IO ()
help = putStrLn "Unknown argument"

