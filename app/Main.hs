module Main where

import System.Environment
import System.Exit
import Lib

main :: IO ()
main = getArgs >>= parseArgs >>= putStrLn

parseArgs :: [String] -> IO String
parseArgs []     = usage   >> exit
parseArgs ["-h"] = usage   >> exit
parseArgs ["-v"] = version >> exit
parseArgs (f:_)  = (show . loadFile) <$> readFile f

usage   = putStrLn "Usage: north [-vh] [file]"
version = putStrLn "North v0.0.1"
exit    = exitWith ExitSuccess
die     = exitWith $ ExitFailure 1
