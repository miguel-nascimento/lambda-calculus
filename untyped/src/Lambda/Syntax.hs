module Lambda.Syntax where

type Name = String

data Expr
  = Var Name
  | Lamb Name Expr -- Lamb is the Abstraction
  | App Expr Expr
  | Lit Int        -- Lit is the way to have a Haskell type in the lc
  | Prim PrimOp Expr Expr
  deriving (Show)

data PrimOp = Add | Mult
  deriving (Show)