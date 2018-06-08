type BindingName = String

data Term =
      Var Int
    | Abs BindingName Term
    | App Term Term

type Context = [BindingName]

pickFreshName :: Context -> BindingName -> (Context, BindingName)
pickFreshName context name
    | not (name `elem` context) = (name : context, name)
    | otherwise = pickFreshName context (name ++ "'")

showTerm :: Context -> Term -> String
showTerm context term = case term of
    Var index     -> if (length context <= index)
                        then "x" ++ show index
                        else context !! index
    Abs name body -> "(l" ++ freshName ++ ". " ++ showTerm newContext body ++ ")"
                        where
                            (newContext, freshName) = pickFreshName context name
    App func arg  -> "(" ++ showTerm context func ++ " " ++ showTerm context arg ++ ")"

instance Show Term where
    show t = showTerm [] t

shiftTerm :: Term -> Int -> Term
shiftTerm term distance = shift term 0
    where
        shift term cut = case term of
            Var index     -> if index < cut
                                then Var index
                                else Var (index + distance)
            Abs name body -> Abs name shiftedBody
                                where shiftedBody = shift body (cut + 1)
            App func arg  -> App shiftedFunc shiftedArg
                                where shiftedFunc = shift func cut;
                                      shiftedArg = shift arg cut

substituteTerm :: Term -> Term -> Term
substituteTerm func arg = shiftTerm term (-1)
    where term = substitute 0 arg' func
            where
                arg' = shiftTerm arg 1;
                substitute targetIndex argTerm term = case term of
                    Var index     -> if index == targetIndex
                                        then argTerm
                                        else term
                    Abs name body -> Abs name body'
                                        where
                                            body' = substitute (targetIndex + 1) (shiftTerm argTerm 1) body
                    App func arg  -> App func' arg'
                                        where
                                            func' = substitute targetIndex argTerm func;
                                            arg' = substitute targetIndex argTerm arg

isValue :: Context -> Term -> Bool
isValue context term = case term of
        Abs _ _ -> True
        _       -> False


evaluate1step :: Context -> Maybe Term -> Maybe Term
evaluate1step context term = case term of
        Just (App (Abs x body) arg)
            | isValue context arg -> Just $ substituteTerm body arg
            | otherwise           -> Just (App (Abs x body) arg')
                                        where Just arg' = evaluate1step context $ Just arg
        Just (App func arg)       -> Just (App func' arg)
                                        where Just func' = evaluate1step context $ Just func
        _                         -> Nothing

evaluate :: Context -> Term -> Term
evaluate context term =
    case mTerm' of
        Just _  -> term'
                    where Just term' = fmap (evaluate context) mTerm'
        Nothing -> term
    where
        mTerm' = evaluate1step context (Just term)

func = Abs "x" $ Var 0
value = Abs "x" $ Abs "x" $ Var 1
app = App func value
