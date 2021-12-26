import qualified Data.Map as Map

data Type = TyInt | TyBool | TyArrow Type Type
  deriving (Show, Eq)

data Value = VClosure String (Type, Expr) Context | VInt Int | VBool Bool
  deriving (Show)

data Expr = Lamb String (Type, Expr) | App Expr Expr | Var String | ExpTrue | ExpFalse | ExpInt Int
  deriving (Show)

type TyContext = Map.Map String Type

type Context = Map.Map String Value

data Error = VariableNotFoundInCtx String | WrongTypeFunc Expr Type Expr Type | AppLHSNotLambda Expr Type
  deriving (Show)

infer :: TyContext -> Expr -> Either Error Type
infer ctx ExpTrue = Right TyBool
infer ctx ExpFalse = Right TyBool
infer ctx (ExpInt n) = Right TyInt
infer ctx (Var x) = case Map.lookup x ctx of
  Nothing -> Left $ VariableNotFoundInCtx x
  Just ty -> Right ty
infer ctx (Lamb x (ty, body)) =
  let ctx' = Map.insert x ty ctx
      ty' = infer ctx' body
   in TyArrow ty <$> ty'
infer ctx (App e1 e2) = do
  tyE1 <- infer ctx e1
  tyE2 <- infer ctx e2
  case tyE1 of
    TyArrow a b ->
      if tyE2 == a
        then Right b
        else Left $ WrongTypeFunc e1 tyE1 e2 tyE2
    _ -> Left $ AppLHSNotLambda e1 tyE1

eval :: Context -> Expr -> Either Error Value
eval ctx (Var x) = case Map.lookup x ctx of
  Nothing -> Left $ VariableNotFoundInCtx x
  Just v -> Right v
eval ctx ExpTrue = Right $ VBool True
eval ctx ExpFalse = Right $ VBool False
eval ctx (ExpInt n) = Right $ VInt n
eval ctx (Lamb x body) = Right $ VClosure x body ctx
eval ctx (App expr1 expr2) = do
  r <- eval ctx expr1
  r' <- eval ctx expr2
  case r of
    VClosure s (_, exp) ctx -> eval (Map.insert s r' ctx) exp
    VInt n -> Right $ VInt n
    VBool b -> Right $ VBool b

idInt :: Expr
idInt = Lamb "x" (TyInt, Var "x")

idBool :: Expr
idBool = Lamb "x" (TyBool, Var "x")

one :: Expr
one = App idInt (ExpInt 1)

-- Passing a integer to a function that takes a boolean
err :: Expr
err = App idBool (ExpInt 1)

run :: Expr -> Either Error Value
run e = case infer Map.empty e of
  Left err -> Left err
  Right ty -> eval Map.empty e

runIdOne :: Either Error Value
runIdOne = run one

runErr :: Either Error Value
runErr = run err