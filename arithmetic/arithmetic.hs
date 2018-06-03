
data Term =
    Tru
  | Fals
  | If Term Term Term
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term

instance Show Term where
  show Tru               = "Tru"
  show Fals              = "Fals"
  show (If cond thn els) =
    "If (" ++ (show cond) ++ ") then (" ++ (show thn) ++ ") else (" ++ (show els) ++")"
  show Zero              = "Zero"
  show (Succ n)          = "Succ (" ++ (show n) ++ ")"
  show (Pred t)          = "Pred (" ++ (show t) ++ ")"
  show (IsZero n)        = "IsZero (" ++ (show n) ++ ")"

isNumericValue t = case t of
  Zero   -> True
  Succ n -> isNumericValue n
  _      -> False

isValue t = case t of
  Tru  -> True
  Fals -> True
  _    -> isNumericValue t

evaluate1step t = case t of
  (If Tru thn _)    -> thn
  (If Fals _ els)   -> els
  (If cond thm els) -> (If cond' thm els)
    where cond' = evaluate1step cond
  (Succ n)          -> Succ n'
    where n' = evaluate1step n
  (Pred Zero)       -> Zero
  (Pred (Succ n))   -> n
  (Pred t)          -> Pred t'
    where t' = evaluate1step t
  (IsZero Zero)     -> Tru
  (IsZero (Succ _)) -> Fals
  (IsZero t)        -> IsZero t'
    where t' = evaluate1step t
  _                 -> t

evaluate t
  | isValue t  = t
  | otherwise =  evaluate t'
      where t' = evaluate1step t


