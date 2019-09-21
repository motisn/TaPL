import Prelude hiding (and, or, not, fst, snd, head, tail)
bool b = b 1 0
church c = c (1+) 0
--list l = l (:) []
blist l = l (\h -> \t -> (bool h):t) []
clist l = l (\h -> \t -> (church h):t) []

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
c2 = \s -> \z -> s (s z)
scc = \n -> \s -> \z -> s (n s z)   -- 現在のChurch数（sを適用させた数）にsを適用する
scc' = \c -> \s -> \z -> (c s) (s z)    -- sを適用してかChurch数にする
plus = \m -> \n -> \s -> \z -> m s (n s z)  -- zにsをn回適用したものにsをm回適用する
times = \m -> \n -> m (plus n) c0   -- c0に引数にnを足すをm回適用する
times' = \m -> \n -> \s -> \z -> m (n s) z  -- zにsをn回適用するをm回適用する
power = \m -> \n -> n (times m) c1
iszero = \m -> m (\x -> fls) tru    -- m回flsをtruに適用する。
zz = pair c0 c0
ss = \p -> pair (snd p) (plus c1 (snd p))
prd = \m -> fst (m ss zz)
minus = \m -> \n -> n `seq` (n prd) m   -- n >= c2のとき無限型エラー
-- `seq`で即評価するらしいが無駄だった
lcr = \l -> \r -> iszero (minus l r)    -- l < r
rcl = \l -> \r -> iszero (minus r l)    -- r < l
--equal m n = and $! (lcr m n) (rcl m n)  -- どうしても無限型？で怒られる…
-- (c2 $ \m -> m c0) c0がだめでc2 $ c0 c0は大丈夫…謎

nil = \c -> \n -> n
-- [x, y, z] = \c -> \n -> c x (c y (c z n))
cons = \h -> \t -> \c -> \n -> c h (t c n)
isnil = \l -> l (\h -> \t -> fls) tru   -- cは2変数関数
head = \l -> l tru fls
tail = \l -> fst (
    l (\p -> \ps -> pair (snd ps) (cons p (snd ps))) (pair nil nil)
    )
