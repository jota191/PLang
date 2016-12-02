{-# LANGUAGE StandaloneDeriving #-}
-- | Core dataType definitions for P
-- | Juan García Garland (Nov. 2016)

module Language where


type Variable = Int

-- | The AST definitions for P

data Program = Program Variable Sec Variable

data Sent    = Assign Variable Expr
             | While Cond Sec

data Expr    = Zero | Succ Variable | Pred Variable

data Cond    = Nonzero Variable

data Sec     = Sec [Sent]

deriving instance Show Cond
deriving instance Show Sec
deriving instance Show Expr
deriving instance Show Sent
deriving instance Show Program

