data DeBruijn = DBint Int | DBabs DeBruijn | DBapp DeBruijn DeBruijn
    deriving (Eq, Show)

shiftDB d term = _shiftD_OverC d 0 term 
_shiftD_OverC d c (DBint i) = DBint (if i < c then i else i + d)
_shiftD_OverC d c (DBapp t1 t2) = DBapp (_shiftD_OverC d c t1) (_shiftD_OverC d c t2)
_shiftD_OverC d c (DBabs t) = DBabs (_shiftD_OverC d (c+1) t)

substitute j s (DBint i) = DBint (if i == j then s else i)
substitute j s (DBapp t1 t2) = DBapp (substitute j s t1) (substitute j s t2)
substitute j s (DBabs t) = DBabs (substitute (j+1) s (shiftDB 1 t))