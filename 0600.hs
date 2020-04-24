-- 0601.hsでは関数適用を空白で表したが、(*)のほうが実装が素直じゃない？という気持ち
import Control.Monad.State
--import Debug.Trace

(<?-) :: (Eq a) => a -> [a] -> [Int]
(<?-) a = filter (>=0) . zipWith (\i -> \x ->
    if x==a then i else -1) [0..]

reserved = ['(',')','*','.']

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
        )) ([""], "")   -- 終止符""

data AST = Var String | Abs String AST | App AST AST
    deriving (Eq, Show)

parse :: [String] -> AST
parse ts = fst $ runState expr ts

-- 逆戻りはないからZipperじゃなくてリストで十分
-- expr = Abs term | term ("*" term)*
expr :: State [String] AST
expr = do
    t0 <- term
    ($ t0) . fix $ \loop t -> do
        token <- (state $ \(t:ts) ->(t, ts))    -- pop的な
        case token of
            "" -> return t
            "." -> case t of
                Var v -> do
                    expression <- expr
                    return $ Abs v expression
                _ -> undefined
            "*" -> do
                t1 <- term
                loop $ App t t1
            _ -> do
                state $ \(ts) -> ((), token:ts)   -- push的な
                return t

-- term = (expr) | Ver v 
term :: State [String] AST
term = do
    token <- (state $ \(t:ts) -> (t, ts))   -- pop的な
    case token of
        "(" -> do
            expression <- expr
            ket <- (state $ \(t:ts) -> (t, ts))   -- pop的な
            if ket /= ")"
                then undefined
                else return expression
        ")" -> undefined
        _ -> return $ Var token