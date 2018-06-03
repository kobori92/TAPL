
data Term =
    True
  | False
  | If Term Term Term
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term

isNumericValue t = case t of
  Zero   -> Prelude.True
  Succ n -> isNumericValue n
  _      -> Prelude.False

isValue t = case t of
  Main.True  -> Prelude.True
  Main.False -> Prelude.True
  _          -> isNumericValue t

evaluate1step t = case t of
  (If Main.True thn _)  -> thn
  (If Main.False _ els) -> els
  (If cond thm els)     -> (If cond' thm els)
    where cond' = evaluate1step cond
  (Succ n)              -> Succ n'
    where n' = evaluate1step n
  (Pred Zero)           -> Zero
  (Pred (Succ n))       -> n
  (Pred t)              -> Pred t'
    where t' = evaluate1step t
  (IsZero Zero)         -> Main.True
  (IsZero (Succ _))     -> Main.False
  (IsZero t)            -> IsZero t'
    where t' = evaluate1step t
  _ -> t

evaluate t
  | isValue t  = t
  | otherwise =  evaluate t'
      where t' = evaluate1step t


