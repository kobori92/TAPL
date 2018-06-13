data Type =
      Arr Type Type
    | Boolean

data Term =
      Var Int
    | Abs String Type Term
    | App Term Term
    | Tru
    | Fals
    | If Term Term Term

type BindingName = String

data Binding =
      NameBind
    | VarBind Type

instance Eq Binding where
    NameBind  == NameBind  = True
    NameBind  == VarBind _ = False
    VarBind _ == VarBind _ = True


type Context = [(BindingName, Binding)]

instance Show Type where
    show typ = case typ of
        Arr typ1 typ2 -> "(" ++ show typ1 ++ " -> " ++ show typ2 ++ ")"
        Boolean       -> "Bool"

pickFreshName :: Context -> BindingName -> (Context, BindingName)
pickFreshName context name
    | ((name, NameBind) `elem` context) = pickFreshName context (name ++ "''")
    | otherwise                         = ((name, NameBind) : context, name)

showTerm :: Context -> Term -> String
showTerm context term = case term of
    Var index         -> if (length context <= index)
                            then "x" ++ show index
                            else fst $ context !! index
    Abs name typ body -> "(l" ++ freshName ++ ":" ++ show typ ++ ". " ++ showTerm newContext body ++ ")"
                            where
                                (newContext, freshName) = pickFreshName context name
    App func arg      -> "(" ++ showTerm context func ++ " " ++ showTerm context arg ++ ")"
    Tru               -> "True"
    Fals              -> "False"
    If cond thn els   -> "If (" ++ show cond ++ ") then {" ++ show thn ++ "} else {" ++ show els ++ "}"

instance Show Term where
    show t = showTerm [] t

shiftTerm :: Term -> Int -> Term
shiftTerm term distance = shift term 0
    where
        shift term cut = case term of
            Var index         -> if index < cut
                                    then Var index
                                    else Var (index + distance)
            Abs name typ body -> Abs name typ shiftedBody
                                    where shiftedBody = shift body (cut + 1)
            App func arg      -> App shiftedFunc shiftedArg
                                    where shiftedFunc = shift func cut;
                                          shiftedArg = shift arg cut
            Tru               -> Tru
            Fals              -> Fals
            If cond thn els   -> If cond' thn' els'
                                    where cond' = shift cond cut;
                                          thn' = shift thn cut;
                                          els' = shift els cut

substituteTerm :: Term -> Term -> Term
substituteTerm func arg = shiftTerm term (-1)
    where term = substitute 0 arg' func
            where
                arg' = shiftTerm arg 1;
                substitute targetIndex argTerm term = case term of
                    Var index         -> if index == targetIndex
                                            then argTerm
                                            else term
                    Abs name typ body -> Abs name typ body'
                                            where
                                                body' = substitute (targetIndex + 1) (shiftTerm argTerm 1) body
                    App func arg      -> App func' arg'
                                            where
                                                func' = substitute targetIndex argTerm func;
                                                arg' = substitute targetIndex argTerm arg
                    Tru               -> Tru
                    Fals              -> Fals
                    If cond thn els   -> If cond' thn' els'
                                            where
                                                cond' = substitute targetIndex argTerm cond;
                                                thn' = substitute targetIndex argTerm thn;
                                                els' = substitute targetIndex argTerm els


isValue :: Context -> Term -> Bool
isValue context term = case term of
        Abs _ _ _ -> True
        Tru       -> True
        Fals      -> True
        _         -> False


evaluate1step :: Context -> Maybe Term -> Maybe Term
evaluate1step context term = case term of
        Just (App (Abs x typ body) arg)
            | isValue context arg -> Just $ substituteTerm body arg
            | otherwise           -> Just (App (Abs x typ body) arg')
                                        where Just arg' = evaluate1step context $ Just arg
        Just (App func arg)       -> Just (App func' arg)
                                        where Just func' = evaluate1step context $ Just func
        Just (If Tru thn _)       -> Just thn
        Just (If Fals _ els)      -> Just els
        Just (If cond thn els)
            -> case cond' of
                    Just cond''   -> Just (If cond'' thn els)
                    Nothing       -> Nothing
                where
                    cond' = evaluate1step context $ Just cond
        _                         -> Nothing


evaluate :: Context -> Term -> Term
evaluate context term =
    case mTerm' of
        Just _  -> term'
                    where Just term' = fmap (evaluate context) mTerm'
        Nothing -> term
    where
        mTerm' = evaluate1step context (Just term)

x = If (Abs "x" Boolean (Var 0))
y = evaluate1step [] $ Just $ If (Abs "x" Boolean (Var 0)) Tru Fals
z = evaluate [] $ If (Abs "x" Boolean (Var 0)) Tru Fals
