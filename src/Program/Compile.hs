module Program.Compile where

-- TODO: Generate assembly
step p = p

intrinsics :: [(String, String)]
intrinsics =
    [ ("dup", "some assembly")
    , ("swp", "some more assembly")
    ]
