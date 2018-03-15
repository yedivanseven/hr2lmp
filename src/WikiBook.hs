module WikiBook
    (
    ) where

g::(Integer -> Integer) -> (Integer -> Integer)
g f n = if n == 0 then 1 else n * f(n-1)

x0 :: Integer -> Integer
x0 = undefined

(f0:f1:f2:f3:f4:fs) = iterate g x0

fix::(a -> a) -> a
fix g = let f = g f in f

f = fix g

h::(Integer -> Integer) -> (Integer -> Integer)
h f n
    | n == 0    = 1
    | n == 1    = 1
    | otherwise = f(n-1) + f(n-2)

fib = fix h

one::Integer -> Integer
one x = 1

cond b x y = if b then x else y
