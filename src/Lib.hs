module Lib
    ( loadFile
    , lexFile
    , lexLine
    , lexWord
    , parseFile
    , compileFile
    ) where

import Data.Function ((&))

loadFile :: String -> [[String]]
loadFile f = lexFile f
--           & parseFile
--           & compileFile

lexFile :: String -> [[String]]
lexFile f = lines f & map lexLine

lexLine :: String -> [String]
lexLine l = words l & map lexWord

-- TODO: Enum of instances `symbol` and `unsigned integer`
lexWord :: String -> String
lexWord w = w

parseFile = undefined

compileFile = undefined
