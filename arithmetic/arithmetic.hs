
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
  (If Tru thn _)    -> Just thn
  (If Fals _ els)   -> Just els
  (If cond thm els) -> case maybeCond of
                         Just cond' -> Just (If cond' thm els)
                         _          -> maybeCond
                       where maybeCond = evaluate1step cond
  (Succ n)          -> case maybeN of
                         Just n' -> Just (Succ n')
                         _       -> maybeN
                       where maybeN = evaluate1step n
  (Pred Zero)       -> Just Zero
  (Pred (Succ n))   -> Just n
  (Pred t)          -> case maybeT of
                         Just t' -> Just (Pred t')
                         _       -> maybeT
                       where maybeT = evaluate1step t
  (IsZero Zero)     -> Just Tru
  (IsZero (Succ _)) -> Just Fals
  (IsZero t)        -> case maybeT of
                         Just t' -> Just (IsZero t')
                         _       -> maybeT
                       where maybeT = evaluate1step t
  _                 -> Nothing

evaluate t = case maybeT of
    Just t' -> evaluate t'
    _       -> t
  where maybeT = evaluate1step t

