import Data.List

data T = True' | False' | O | Succ T | X deriving (Eq, Show)
succ' :: T -> T
succ' x = Succ(x)
pred' :: T -> T
pred' (Succ x) = x
pred' x = x
iszero :: T -> T
iszero x = if x == O then True' else False'

s_next s = 
    let sub1 = [True', False', O]
        sub2 = [succ', pred', iszero] <*> s
        sub3 = (\t1 t2 t3 -> case t1 of
            True' -> t2
            False' -> t3
            _ -> X) <$> s <*> s <*> s
    in foldl union [] [sub1, sub2, sub3]

s0 = [] :: [T]
s1 = s_next s0
s2 = s_next s1
s3 = s_next s2

count :: [a] -> Int
count = foldr (\_ x -> x + 1) 0