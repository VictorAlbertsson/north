import Program.Common.Data

import Control.Exception
import Data.List

tests = zip [1..]
    [ ]

testStatus True  = greenText "PASSED"
  where
    greenText t = "\^[[32m" ++ t ++ "\^[[0m"

testStatus False = redText "FAILED"
  where
    redText t = "\^[[31m" ++ t ++ "\^[[0m"

testMessage (n, t) = yellowText $ "[TEST #" ++ show n ++ "] " ++ testStatus t
  where
    yellowText t = "\^[[33m" ++ t ++ "\^[[0m"

main :: IO ()
main = putStrLn $ concat $ intersperse "\n" $ map testMessage tests
