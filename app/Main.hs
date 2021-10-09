module Main where

{-# LANGUAGE OverloadedStrings #-}

import System.Process.Typed
import System.FilePath.Posix

import Data.Functor
import System.Environment
import System.Exit
import System.IO
import Control.Monad

import Lib

main :: IO ()
main = getArgs >>= processArgs

-- TODO: Proper arg parsing
processArgs :: [String] -> IO ()
processArgs []         = usage       >> exit
processArgs ["-h"]     = usage       >> exit
processArgs ["--help"] = usage       >> exit
processArgs (file:[])  = genExe file >> exit
processArgs _          = usage       >> exit

genExe srcFile = do
    -- Define files
    let asmFile = srcFile -<.> ".asm"
    let objFile = srcFile -<.> ".o"
    let binFile = srcFile -<.> ""
    -- Generate files
    genAsm srcFile asmFile
    runProcess_ $ genObj asmFile objFile
    runProcess_ $ genBin objFile binFile
    runProcess_ $ delTmp asmFile
    runProcess_ $ delTmp objFile

-- TODO: Better error handling
genAsm i o = assembly >>= writeFile o
  where
    assembly = readFile i <&> loadProgram <&> unwrapMaybe
    unwrapMaybe = \maybeAsm -> case maybeAsm of
        Just asm -> asm
        Nothing  -> error "[ERROR] Oops..."

genObj i o = proc "nasm" [i, "-o", o, "-felf64"]

genBin i o = proc "ld"   [i, "-o", o]

delTmp f   = proc "rm" ["-f", f]

usage   = putStrLn "Usage: north [-h --help] [file]"
version = putStrLn "North v0.0.1"
exit    = exitWith ExitSuccess
die     = exitWith $ ExitFailure 1
