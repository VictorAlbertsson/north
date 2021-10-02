module Lib where

import Data.Function ((&))

import qualified Program.Lex
import qualified Program.Expand
import qualified Program.Check
import qualified Program.Compile

--loadProgram :: String -> String
loadProgram f = f
    & Program.Lex.step
    & Program.Expand.step
    & Program.Check.step
    & Program.Compile.step
