{-# LANGUAGE StandaloneDeriving #-}
-- | Core dataType definitions for P
-- | Juan García Garland (Nov. 2016)

module Language where


type Variable = String

-- | The AST definitions for P
data Program = Program Variable Sent Variable
data Number  = D Digit | N Digit Number
data Digit   = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
data Sent    = A Assign | W While | S Sec
data Assign  = As Variable Expr
data Expr    = Zero | Succ Variable | Pred Variable
data While   = Wh Cond Sent
data Cond    = Nonzero Variable
data Sec     = Cons Sent Sent

deriving instance Show Cond
deriving instance Show Sec
deriving instance Show While
deriving instance Show Expr
deriving instance Show Assign
deriving instance Show Sent
deriving instance Show Digit
deriving instance Show Number
deriving instance Show Program

