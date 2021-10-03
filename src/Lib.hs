module Lib where

import Control.Arrow ((>>>))

import qualified Program.Lex
import qualified Program.Expand
import qualified Program.Type
import qualified Program.Final

--loadProgram :: String -> String
loadProgram
    =   Program.Lex.step
    >>> Program.Expand.step
    >>> Program.Type.step
    >>> Program.Final.step
