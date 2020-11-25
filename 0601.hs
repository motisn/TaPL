import Control.Monad.State
--import Debug.Trace

-- 入力したラムダ式をパースしてASTに変換
data AST = Var String | Abs String AST | App AST AST
    deriving (Eq, Show)

(<?-) :: (Eq a) => a -> [a] -> [Int]
(<?-) a = filter (>=0) . zipWith (\i -> \x ->
    if x==a then i else -1) [0..]

reserved = ['(',')']

tokenize :: String -> [String]
tokenize = (\(ss, cs) -> 
    if cs == "" then ss else cs:ss) . foldr (\c -> \(ss, cs) ->
        (case c of
            ' ' -> if cs == "" then (ss, "") else (cs:ss, "")
            _ -> if (c <?- reserved) /= []
                then
                    if cs == "" then ((c:[]):ss, "") else ((c:[]):cs:ss, "")
                else
                    (ss, c:cs)
        )) ([], "")

parse :: [String] -> AST
parse = fst . (runState expr)

-- 逆戻りはないからZipperじゃなくてリストで十分
-- expr = term (term)*
expr :: State [String] AST
expr = do
    t0 <- term
    ($ t0) . fix $ \loop t -> do
        token <- (state $ \(xs) -> case xs of
            t:ts -> (t, ts)
            [] -> ("", []))   -- pop的な
        case token of
            "" -> return t
            _ -> do
                state $ \(ts) -> ((), token:ts)   -- push的な
                t1 <- term
                loop $ App t t1

-- term = (expr) | Abs x'.' expr | Ver v 
term :: State [String] AST
term = do
    token <- (state $ \(t:ts) -> (t, ts))   -- pop的な
    case token of
        "(" -> do
            -- 括弧の中身をbraketに集めて独立して処理、残りをresidualに確保
            residual <- (state $ \(ts) -> ($ (ts,[])) . ($ 1) . fix $ \loop i (xs,tekarb) -> case xs of
                [] -> let residual = concat $ reverse tekarb   -- 残りの文字列を回収
                    in error("Missing ')' of ...("++residual)
                t:ts ->
                    -- 多重括弧を除去
                    let i' = case t of
                                "(" -> i + 1
                                ")" -> i - 1
                                _ -> i
                    in if i' == 0
                            then (ts, reverse tekarb)
                            else loop i' (ts,t:tekarb))
            expression <- expr
            state $ \(ts) -> ((), residual)
            return expression
        ")" -> do
            residual <- (state $ \(ts) -> (concat ts, []))   -- 残りの文字列をpopで回収
            error("Unexpexed ')' at ...)"++residual)
        _ -> if last token == '.'
            then do
                expression <- expr
                return $ Abs (init token) expression
            else do
                return $ Var token

-- ASTをde Bruijn項に変換
data DeBruijn = DBint Int | DBabs DeBruijn | DBapp DeBruijn DeBruijn
    deriving (Eq, Show)
data Context = Context {freeVar :: [String], boundVar :: [String]} deriving (Eq, Show)

removename :: AST -> (DeBruijn, Context)
removename named = runState (_removename named) (Context [] [])
_removename :: AST -> State Context DeBruijn
_removename (Var str) = do
    context <- (state $ \c -> (c, c))
    case str <?- (boundVar context) of
        i:is -> do return $ DBint i
        [] -> case str <?- (freeVar context) of
            i:is -> do return $ DBint i
            [] -> do 
                let newcontext = context {freeVar = (freeVar context) ++ [str]} -- 自由変数は後ろに足す（インデックスが固定）
                state $ \c -> ((), newcontext)
                return $ DBint (length (freeVar newcontext) - 1 + length (boundVar newcontext)) -- DBintは0オリジン
_removename (App t1 t2) = do
    d1 <- _removename t1
    d2 <- _removename t2
    return $ DBapp d1 d2
_removename (Abs str t) = do
    context <- (state $ \c -> (c, c))
    let _context = context {boundVar = str:(boundVar context)} -- 束縛変数は頭から足す（直近のインデックスが最小）
    state $ \c -> ((), _context)
    d <- _removename t
    newcontext <- (state $ \c -> (c, c))
    state $ \c -> ((), newcontext {boundVar = (boundVar context)}) -- 束縛変数の後片付け
    return $ DBabs d

restorename :: (DeBruijn, Context) -> AST
restorename (debruign, context) = fst $ runState (_restorename debruign) context
_restorename :: DeBruijn -> State Context AST
_restorename (DBint int) = do
    context <- (state $ \c -> (c, c))
    let offset = length (boundVar context)
    if int < offset
        then return $ Var (boundVar context !! int)
        else return $ Var (freeVar context !! (int - offset))
_restorename (DBapp d1 d2) = do
    t1 <- _restorename d1
    t2 <- _restorename d2
    return $ App t1 t2
_restorename (DBabs d) = do
    context <- (state $ \c -> (c, c))
    let bvar = show $ length (boundVar context)
    state $ \c -> ((), context {boundVar = bvar:(boundVar context)}) -- 束縛変数は頭から足す
    t <- _restorename d
    state $ \c -> ((), context) -- 束縛変数の後片付け
    return $ Abs bvar t

    