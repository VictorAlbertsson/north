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
    = TokenSymbol   String           -- DONE
    | TokenString   String           -- DONE
    -- Numbers {
    | TokenNatural  Word             -- DONE
    | TokenInteger  Int              -- TODO
    | TokenRational (Word, Word)     -- TODO
    | TokenReal     Double           -- TODO
    | TokenComplex  (Double, Double) -- TODO
    -- }
    | TokenStack    [Token]          -- TODO
    deriving (Show, Read, Eq)

-- NOTE: Some musings on subtyping data vs. properties (typeclasses)
-- data(N) <: data(Z) -> prop(N) :> prop(Z)
