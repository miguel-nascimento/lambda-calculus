module Lambda.Interp where

import qualified Data.Map as Map
import Lambda.Syntax

data Value = VInt Int | VClosure Name Expr Env
  deriving (Show)

type Env = Map.Map Name Value

interp :: Expr -> Env -> Value
interp expr env = case expr of
  Var x -> env Map.! x
  Lamb n v -> VClosure n v env
  App expr1 expr2 ->
    let r = interp expr1 env
        r' = interp expr2 env
     in case r of
          VClosure v expr env' -> interp expr (Map.insert v r' env')
          VInt a -> VInt a
  Lit n -> VInt n
  Prim op expr1 expr2 -> interpOp op (interp expr1 env) (interp expr2 env)

interpOp :: PrimOp -> Value -> Value -> Value
interpOp op (VInt x) (VInt y) = case op of
  Add -> VInt $ x + y
  Mult -> VInt $ x * y
interpOp op _ _ = error "only VInt"

lambSum :: Int -> Int -> Expr
lambSum x y = App (App (Lamb "x" (Lamb "y" (Prim Add (Var "x") (Var "y")))) (Lit x)) (Lit y)