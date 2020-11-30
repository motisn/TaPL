data DeBruijn = DBint Int | DBabs DeBruijn | DBapp DeBruijn DeBruijn
    deriving (Eq, Show)

-- 0602
shiftDB d term = _shiftD_OverC d 0 term 
_shiftD_OverC d c (DBint i) = DBint (if i < c then i else i + d)
_shiftD_OverC d c (DBapp t1 t2) = DBapp (_shiftD_OverC d c t1) (_shiftD_OverC d c t2)
_shiftD_OverC d c (DBabs t) = DBabs (_shiftD_OverC d (c+1) t)

substitute j s (DBint i) = if i == j then s else DBint i
substitute j s (DBapp t1 t2) = DBapp (substitute j s t1) (substitute j s t2)
substitute j s (DBabs t) = DBabs (substitute (j+1) s (shiftDB 1 t))

-- 0603
isval t = case t of
    DBapp _ _ -> False
    otherwise -> True
isabs t = case t of
    DBabs _ -> True
    otherwise -> False

eval1 (DBint i) = DBint i
eval1 (DBabs t) = DBabs t
eval1 (DBapp t1 t2) = case t1 of
    (DBapp _ _) -> DBapp (eval1 t1) t2   -- E-APP1
    (DBint _) -> DBapp t1 (eval1 t2)   -- E-APP2-a
    (DBabs _) -> case t2 of
        (DBapp _ _) -> DBapp t1 (eval1 t2)   -- E-APP2-b
        v -> shiftDB (-1) (substitute 0 (shiftDB 1 v) t1)   -- E-APPABS

eval t = if t == eval1 t then t else eval (eval1 t)

-- 0703
eval' (DBint i) = DBint i
eval' (DBabs t) = DBabs t
eval' (DBapp t1 t2) = let t1' = eval' t1
                          v2 = eval' t2
                      in substitute 0 v2 t1'
          