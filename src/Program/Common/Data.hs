module Program.Common.Data where

data Mode
    = ModeCompile
    | ModeInterpret
    deriving (Show, Read, Eq)

-- TODO: Implement comment handling, either as a `TokenComment` field or as a `Maybe`
-- TODO: Better and more extensive documentation
-- TODO: Add character literal token
-- TODO: Add float/double token
-- TODO: Add stack literal token
data Token
    -- Labels control type relevant behaviour, for more information see README
    = TokenLabel           String
    | TokenSymbol          String
    | TokenString          String
    | TokenUnsignedInteger Word
    | TokenSignedInteger   Int
    | TokenDouble          Double
    | TokenStack           [Token]
    deriving (Show, Read, Eq)

data IntrinsicType
    -- Numeric types
    -- TODO: Add irrational and transcendental numbers as numeric types
    -- NOTE: Above types might be better implemented as `properties` or `typeclasses`
    -- TODO: Implement subtyping of numeric types
    = TypeN64 -- 64 bit natural numbers
    | TypeZ64 -- 64 bit integer numbers
    | TypeQ64 -- 64 bit fractional/quotient numbers
    | TypeR64 -- 64 bit real numbers
    | TypeC64 -- 64 bit complex numbers
    -- Memory types
    | TypePtr -- Memory Pointer
    deriving (Show, Read, Eq)

-- TODO: Procedure table, <procedure name> <input types> <output types>
data IntrinsicFunc
    = FuncAdd
    | FuncSub
    | FuncMul
    | FuncDiv
    | FuncMod
    | FuncPow
    deriving (Show, Read, Eq)

lookupTable :: [(IntrinsicFunc, [IntrinsicType], [IntrinsicType])]
lookupTable =
    -- NOTE: Numeric functions
    [ (FuncAdd, [TypeN64, TypeN64], [TypeN64])
    , (FuncSub, [TypeN64, TypeN64], [TypeZ64])
    , (FuncMul, [TypeN64, TypeN64], [TypeN64])
    , (FuncDiv, [TypeN64, TypeN64], [TypeQ64])
    , (FuncMod, [TypeN64, TypeN64], [TypeN64])
    , (FuncPow, [TypeN64, TypeN64], [TypeN64])
    ]
