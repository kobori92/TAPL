data Term =
      Var Int
    | Abs String Term
    | App Term Term

pickFreshName ctx x
    | not (x `elem` ctx) = (x : ctx, x)
    | otherwise = pickFreshName ctx (x ++ "'")

showTerm ctx t = case t of
    Var i     -> if (length ctx <= i)
                    then "x" ++ show i
                    else ctx !! i
    Abs x t   -> "(l" ++ x' ++ ". " ++ showTerm ctx' t ++ ")"
                 where
                     (ctx', x') = pickFreshName ctx x
    App t1 t2 -> "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"

instance Show Term where
    show t = showTerm [] t
    {-
  show (Var i) = showTerm "x"
  show (Abs x t) = "\\" ++ "x. " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
-}
x = Var 0
abs = Abs "x" x
