module Main where

import qualified Create (act)
import qualified Delete (act)
import qualified Read (act)
import System.Environment as Env
import qualified Update (act)

main :: IO ()
main = Env.getArgs >>= \args -> command args

command :: [String] -> IO ()
command ["-c"] = Create.act Nothing
command ["-c", ymd] = Create.act (Just ymd)
command ["-r"] = Read.act
command ["-u"] = Update.act
command ["-d", ymd] = Delete.act ymd
command _ = help

help :: IO ()
help = putStrLn "Unknown argument"
