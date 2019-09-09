data T = True' | False' | O | Succ T deriving (Eq, Show)
succ' :: T -> T
succ' x = Succ(x)
pred' :: T -> T
pred' (Succ x) = x
pred' x = x
iszero :: T -> T
iszero x = if x == O then True' else False'