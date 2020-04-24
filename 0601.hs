import Control.Monad.State
import Debug.Trace

data T = Tint Int | Tabs T | Tapp T T | X deriving (Eq, Show)
data DeBruijn = DeBruijn {
    _context :: String,
    _named :: String
} deriving Show

type Context = [String]

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

data AST = Var String | Abs String AST | App AST AST
    deriving (Eq, Show)

parse :: [String] -> AST
parse ts = fst $ runState expr ts

-- 逆戻りはないからZipperじゃなくてリストで十分
-- expr = term ("*" term)*
expr :: State [String] AST
expr = do
    t0 <- term
    ($ t0) . fix $ \loop t -> do
        token <- (state $ \(xs) -> case xs of
            t:ts ->(t, ts)
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
    trace (show token) return ()
    case token of
        "(" -> do
            -- 括弧の中身を独立して処理
            tokens <- (state $ \(ts) -> (ts, ts))
            let braket = take (maximum (")" <?- tokens)) tokens
                residual = drop (maximum (")" <?- tokens)+1) tokens
            state $ \(ts) -> ((), braket)
            expression <- expr
            state $ \(ts) -> ((), residual)
            return expression
        ")" -> undefined
        _ -> if last token == '.'
            then do
                expression <- expr
                return $ Abs (init token) expression
            else do
                return $ Var token