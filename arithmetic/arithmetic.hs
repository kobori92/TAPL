data Term =
  | true
  | false
  | if of Term * Term * Term
  | zero
  | succ of Term
  | pred of Term
  | isZero of Term
