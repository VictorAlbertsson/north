module Lib where

import Data.Function ((&))

import qualified Program.Lex
import qualified Program.Expand
import qualified Program.Type
import qualified Program.Compile

--loadProgram :: String -> String
loadProgram f = f
    & Program.Lex.step
    & Program.Expand.step
    & Program.Type.step
    & Program.Compile.step
