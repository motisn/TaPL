import Data.List
-- 3.1
data T = Tru | Fls | O | Suc T | Pre T | Iszero T | T :? (T, T) | X deriving (Eq, Show)

-- 3.2
s_next s = 
    let sub1 = [Tru, Fls, O]                -- |sub1| = 3
        sub2 = [Suc, Pre, Iszero] <*> s     -- |sub2| = 3*|s|
        sub3 = (\t1 t2 t3 -> t1 :? (t2, t3)) <$> s <*> s <*> s
                                            -- |sub3| = |s|^3
    in foldl' union [] [sub1, sub2, sub3]

s0 = [] :: [T]
s1 = s_next s0
s2 = s_next s1
--numel s3をすると終わらない。
--s3 = s_next s2
numel :: [a] -> Int
numel = foldr (\_ n -> n + 1) 0

-- 3.3
consts :: T -> [T]
consts Tru = [Tru]
consts Fls = [Fls]
consts O = [O]
consts (Suc t) = consts t
consts (Pre t) = consts t
consts (Iszero t) = consts t
consts (t1 :? (t2, t3)) = foldl' union [] [consts t1, consts t2, consts t3]

size :: T -> Int
size Tru = 1
size Fls = 1
size O = 1
size (Suc t) = size t + 1
size (Pre t) = size t + 1
size (Iszero t) = size t + 1
size (t1 :? (t2, t3)) = sum $ map size [t1, t2, t3]

depth :: T -> Int
depth Tru = 1
depth Fls = 1
depth O = 1
depth (Suc t) = depth t + 1
depth (Pre t) = depth t + 1
depth (Iszero t) = depth t + 1
depth (t1 :? (t2, t3)) = foldl' max 0 (map depth [t1, t2, t3]) + 1

isnum :: T -> Bool
isnum O = True
isnum (Suc t) = isnum t
isnum _ = False

eval :: T -> T
eval t
    | or [t == Tru, t == Fls, isnum(t), t == X] = t
eval (t1 :? (t2, t3))
    | eval t1 == Tru = eval t2
    | eval t1 == Fls = eval t3
    | otherwise = X
eval (Suc t)
    | isnum(eval t) == True = Suc (eval t)
    | otherwise = X
eval (Pre t) = case (eval t) of
    O -> O
    Suc t' -> t'
    _ -> X
eval (Iszero t) = case (eval t) of
    O -> Tru
    Suc _ -> Fls
    _ -> X