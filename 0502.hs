import Unsafe.Coerce
import Prelude hiding (and, or, not, fst, snd, head, tail, sum)
realbool b = b True False
realnat c = c (1+) 0
--list l = l (:) []
blist l = l (\h -> \t -> (realbool h):t) []
clist l = l (\h -> \t -> (realnat h):t) []

tru = \t -> \f -> t     -- p1 -> p2 -> p1
fls = \t -> \f -> f     -- p1 -> p2 -> p2
test = \l -> \m -> \n -> l m n     -- (t1 -> t2 -> t3) -> t1 -> t2 -> t3

and = \b -> \c -> b c fls   -- (t1 -> (p1 -> p2 -> p2) -> t2) -> t1 -> t2
-- b=truならt1=t2となり、b=flsならばt2=fls
-- :t and tru tru -- p1 -> p2 -> p1 (=tru)
-- :t and tru fls -- p1 -> p2 -> p2 (=fls)
or = \b -> \c -> b tru c    -- ((p1 -> p2 -> p1) -> t1 -> t2) -> t1 -> t2
not = \b -> b tru fls -- ((p4 -> p5 -> p4) -> (p6 -> p7 -> p7) -> t) -> t

pair = \f -> \s -> \b -> b f s      -- t1 -> t2 -> (t1 -> t2 -> t3) -> t3
fst = \p -> p tru       -- ((p1 -> p2 -> p1) -> t) -> t
snd = \p -> p fls       -- ((p1 -> p2 -> p2) -> t) -> t

c0 = \s -> \z -> z     -- p1 -> p2 -> p2
c1 = \s -> \z -> s z

-- (\s -> s (s c))) prdが無限型エラーを起こすので、(\s -> s (unsafeCoerce (s c))) prd
_c2 = \s -> \z -> s (s z)
_scc = \n -> \s -> \z -> s (n s z)   -- 現在のChurch数（sを適用させた数）にsを適用する
_scc' = \c -> \s -> \z -> (c s) (s z)    -- sを適用してからChurch数にする
_plus = \m -> \n -> \s -> \z -> m s (n s z)  -- zにsをn回適用したものにsをm回適用する

c2 = \s -> \z -> s (unsafeCoerce (s z))
scc = \n -> \s -> \z -> s (unsafeCoerce (n s z)) 
plus = \m -> \n -> \s -> \z -> m s (unsafeCoerce (n s z))

times = \m -> \n -> m (plus n) c0   -- c0に引数にnを足すをm回適用する
times' = \m -> \n -> \s -> \z -> m (n s) z  -- zにsをn回適用するをm回適用する
power = \m -> \n -> n (times m) c1
iszero = \m -> m (\x -> fls) tru    -- m回flsをtruに適用する。
zz = pair c0 c0
ss = \p -> pair (snd p) (plus c1 (snd p))
prd = \m -> fst (m ss zz)
minus = \m -> \n -> (n prd) m
lcr = \l -> \r -> iszero (minus l r)    -- l < r
rcl = \l -> \r -> iszero (minus r l)    -- r < l
--equal m n = and $ (lcr m n) (rcl m n)  -- どうしても無限型？で怒られる…

nil = \c -> \n -> n
-- [x, y, z] = \c -> \n -> c x (c y (c z n))
cons = \h -> \t -> \c -> \n -> c h (t c n)
isnil = \l -> l (\h -> \t -> fls) tru   -- cは2変数関数
head = \l -> l tru fls
tail = \l -> fst (
    l (\p -> \ps -> pair (snd ps) (cons p (snd ps))) (pair nil nil)
    )

-- cannot construct the infinite type in first x :: t0 ~ t0 -> t
--omega = (\x -> x x) (\x -> x x)
-- 定義できるが実行すると無限ループになる
--omega' = (\x -> unsafeCoerce x x) (\x -> unsafeCoerce x x) ::t

fix = \f -> (\x -> f ((unsafeCoerce x) x)) (\x -> f ((unsafeCoerce x) x))
--g' = \fct -> \n -> if n==0 then 1 else n*fct(n-1)
-- Haskellは必要呼びなのでiszeroがTrueになった時点でc1に評価されて停止できる？
g = \fct -> \c -> test (iszero c) c1 (unsafeCoerce (times c (unsafeCoerce fct(prd (unsafeCoerce c)))))
-- 値呼びの場合はtestの戻り値を関数に包んで、選ばれなかった分岐の評価をしないようにする
g' = \fct -> \c -> test (iszero c) (\_ -> c1) (\_ -> unsafeCoerce (times c (unsafeCoerce fct(prd (unsafeCoerce c))))) c0

chrchnat n = fix (
    \fct -> \n -> if n == 0 then c0 else (unsafeCoerce (scc (unsafeCoerce (fct (n - 1)))))
    ) n

sum cs = fix (
    \fct -> \cs -> test (isnil cs) c0 (unsafeCoerce (plus (unsafeCoerce (head (unsafeCoerce cs))) (unsafeCoerce fct (tail (unsafeCoerce cs)))))
    ) cs