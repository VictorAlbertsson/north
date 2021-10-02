module Program.Common.Data where

-- TODO: Implement comment handling, either as a `TokenComment` field or as a `Maybe`
-- TODO: Better and more extensive documentation
-- TODO: Add character literal parsing
data Token
    -- Labels control type relevant behaviour, for more information see README
    = TokenLabel           String
    | TokenSymbol          String
    | TokenString          String
    | TokenUnsignedInteger Word
    | TokenSignedInteger   Int
    deriving (Show, Read, Eq)
