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
    = TypeN64 -- 64 bit naturals
    | TypeI64 -- 64 bit integers
    | TypeQ64 -- 64 bit quotients
    | TypeR64 -- 64 bit reals
    -- Memory types
    | TypePTR -- Memory Pointer
    deriving (Show, Read, Eq)

-- TODO: Procedure table, <procedure name> <input types> <output types>
data IntrinsicFunc
    = FuncAdd
    | FuncSub
    | FuncMul
    | FuncDiv
    deriving (Show, Read, Eq)

lookupTable :: [(IntrinsicFunc, [IntrinsicType], [IntrinsicType])]
lookupTable =
    [ (FuncAdd, [TypeN64, TypeN64], [TypeN64])
    , (FuncSub, [TypeN64, TypeN64], [TypeI64])
    , (FuncMul, [TypeN64, TypeN64], [TypeN64])
    , (FuncDiv, [TypeN64, TypeN64], [TypeR64])
    ]
