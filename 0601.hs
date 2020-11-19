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
parse ts = fst $ runState expr ts

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
data T = Tint Int | Tabs T | Tapp T T | X deriving (Eq, Show)
data DeBruijn = DeBruijn {
    _context :: String,
    _named :: String
} deriving Show

type Context = [String]