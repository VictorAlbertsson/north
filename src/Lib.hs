module Lib where

import Control.Arrow ((>>>))

import qualified Program.Lex
import qualified Program.Expand
import qualified Program.Type
-- import qualified Program.Final
import qualified Program.Compile

--loadProgram :: String -> String
loadProgram
    =   Program.Lex.step
    >>> Program.Expand.step
    >>> Program.Type.step
    >>> Program.Compile.step
    -- >>> Program.Assemble.step
    -- >>> Program.Link.step
