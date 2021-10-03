import Program.Common.Data

import Control.Exception
import Data.List

typeIndex = 5 -- Number of `IntrinsicType`s (in `Program.Common.Data`)
funcIndex = 4 -- Number of `IntrinsicFunc`s (in `Program.Common.Data`)

tests = zip [1..]
    [ -- #ObfuscatedHaskell
      -- Compares `funcIndex` to the length of `lookupTable`
      (flip assert
          <*> ((==) $ fromIntegral $ length lookupTable)
          $   funcIndex
      ) == funcIndex
    ]

testStatus True  = "PASSED"
testStatus False = "FAILED"

testMessage (n, t) = "[TEST #" ++ show n ++ "] " ++ testStatus t

main :: IO ()
main = putStrLn $ concat $ intersperse "\n" $ map testMessage tests
