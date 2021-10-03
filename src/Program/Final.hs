module Program.Final where

import Program.Common.Data

import qualified Program.Compile
import qualified Program.Interpret

-- TODO: Set `mode` according to command flag `-m<c|i>`
mode = ModeInterpret

step
    | mode == ModeCompile   = Program.Compile.step
    | mode == ModeInterpret = Program.Interpret.step
