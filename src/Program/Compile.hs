module Program.Compile where

-- TODO: Generate assembly
step = id

intrinsics :: [(String, String)]
intrinsics =
    [ ("dup", "some assembly")
    , ("swp", "some more assembly")
    ]
