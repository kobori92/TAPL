{--case と whereは段落を揃える--}
data Type =
      Arr Type Type
    | Boolean

instance Eq Type where
    Arr srcType tgtType  == Arr srcType' tgtType'
                       = srcType == srcType' && tgtType == tgtType'
    Boolean == Boolean = True
    Arr _ _ == Boolean = False

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
    NameBind    == NameBind     = True
    NameBind    == VarBind _    = False
    VarBind typ == VarBind typ' = typ == typ'


type Context = [(BindingName, Binding)]

instance Show Type where
    show typ = case typ of
        Arr typ1 typ2 -> "(" ++ show typ1 ++ " -> " ++ show typ2 ++ ")"
        Boolean       -> "Bool"

pickFreshName :: Context -> BindingName -> (Context, BindingName)
pickFreshName context name
    | ((name, NameBind) `elem` context)
        = pickFreshName context (name ++ "''")
    | otherwise
        = ((name, NameBind) : context, name)

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
    If cond thn els   -> "If (" ++ show cond
                            ++ ") then {" ++ show thn
                            ++ "} else {" ++ show els ++ "}"

instance Show Term where
    show t = showTerm [] t



shift :: Term -> Int -> Term
shift term distance = shiftTerm term 0
    where
        shiftTerm term cutoff = case term of
            Var index         -> if index < cutoff
                                    then Var index
                                    else Var (index + distance)
            Abs name typ body -> Abs name typ shiftedBody
                                    where shiftedBody = shiftTerm body (cutoff + 1)
            App func arg      -> App shiftedFunc shiftedArg
                                    where shiftedFunc = shiftTerm func cutoff;
                                          shiftedArg  = shiftTerm arg cutoff
            Tru               -> Tru
            Fals              -> Fals
            If cond thn els   -> If cond' thn' els'
                                    where cond' = shiftTerm cond cutoff;
                                          thn'  = shiftTerm thn cutoff;
                                          els'  = shiftTerm els cutoff

substituteTerm targetIndex argTerm term = case term of
    Var index         -> if index == targetIndex
                            then argTerm
                            else term
    Abs name typ body -> Abs name typ body'
                            where
                                body' = substituteTerm (targetIndex + 1) (shift argTerm 1) body
    App func arg      -> App func' arg'
                            where
                                func' = substituteTerm targetIndex argTerm func;
                                arg'  = substituteTerm targetIndex argTerm arg
    Tru               -> Tru
    Fals              -> Fals
    If cond thn els   -> If cond' thn' els'
                            where
                                cond' = substituteTerm targetIndex argTerm cond;
                                thn'  = substituteTerm targetIndex argTerm thn;
                                els'  = substituteTerm targetIndex argTerm els

substitute :: Term -> Term -> Term
substitute func arg
    = shift (substituteTerm 0 arg' func) (-1)
        where arg' = shift arg 1;



isValue :: Context -> Term -> Bool
isValue context term = case term of
    Abs _ _ _ -> True
    Tru       -> True
    Fals      -> True
    _         -> False


evaluate1step :: Context -> Maybe Term -> Maybe Term
evaluate1step context term = case term of
    Just (App (Abs x typ body) arg)
        | isValue context arg
                           -> Just $ substitute body arg
        | otherwise        -> Just (App (Abs x typ body) arg')
                                where Just arg' = evaluate1step context $ Just arg
    Just (App func arg)    -> Just (App func' arg)
                                where Just func' = evaluate1step context $ Just func
    Just (If Tru thn _)    -> Just thn
    Just (If Fals _ els)   -> Just els
    Just (If cond thn els) -> case cond' of
                                Just cond'' -> Just (If cond'' thn els)
                                Nothing     -> Nothing
                              where
                                cond' = evaluate1step context $ Just cond
    _       -> Nothing


evaluate :: Context -> Term -> Term
evaluate context term =
    case mTerm' of
        Just _  -> term'
                    where Just term' = fmap (evaluate context) mTerm'
        Nothing -> term
    where
        mTerm' = evaluate1step context (Just term)

getTypeFrom :: Context -> Int -> Maybe Type
getTypeFrom context index
    | index < (length context) = case context !! index of
        (_, VarBind typ) -> Just typ
        _                -> Nothing
    | otherwise                = Nothing

typeOf :: Context -> Term -> Maybe Type
typeOf context term = case term of
    Var index         -> getTypeFrom context index
    Abs name typ body ->
        case typeOf context' body of
            Just targetType     -> Just $ Arr typ targetType
            _                   -> Nothing
        where
            context' = (name, VarBind typ) : context
    App func arg      -> case typeOf context func of
        Just (Arr srcTyp tgtType)
                            -> if typeOf context arg == Just srcTyp
                                then Just tgtType
                                else Nothing
        _                   -> Nothing
    Tru               -> Just Boolean
    Fals              -> Just Boolean
    {--修正--}
    If cond thn els   ->
        case typ of
            Just Boolean
                | typeOf context els == thnTyp -> thnTyp
                | otherwise                    -> Nothing
                where
                    thnTyp = typeOf context thn
            otherwise -> Nothing
        where
            typ = typeOf context thn


x = If (Abs "x" Boolean (Var 0))
y = evaluate1step [] $ Just $ If (Abs "x" Boolean (Var 0)) Tru Fals
z = evaluate [] $ If (Abs "x" Boolean (Var 0)) Tru Fals
