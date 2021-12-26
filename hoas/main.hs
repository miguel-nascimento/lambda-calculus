{-# LANGUAGE GADTs #-}

data Expr a where
  Lamb :: (Expr a -> Expr b)      -> Expr (a -> b)
  App  :: Expr (a -> b) -> Expr a -> Expr b
  Lift :: a                       -> Expr a

eval :: Expr a -> a
eval (App e1 e2) = eval e1 (eval e2)
eval (Lamb f) = eval . f . Lift
eval (Lift a) = a

s :: Expr ((a -> b -> c) -> (a -> b) -> (a -> c))
s = Lamb (\x -> Lamb (\y -> Lamb (\z -> App (x `App` z) (y `App` z))))

k :: Expr (a -> b -> a)
k = Lamb (\x -> Lamb (\y -> x))

skk :: Expr (a -> a)
skk = App (App s k) k