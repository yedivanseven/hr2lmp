module Chapter01
    (
    ) where

-- Prelude
divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

ldf :: Integral a => a -> a -> a
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k + 1) n

ld :: Integral a => a -> a
ld n = ldf 2 n

prime0 :: Integral a => a -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

-- Example 1.8
mnmInt :: [Int] -> Int
mnmInt []     = error "empty list"
mnmInt [x]    = x
mnmInt (x:xs) = min x (mnmInt xs)

mnmInt' :: [Int] -> Int
mnmInt' []  = error "empty list"
mnmInt' [x] = x
mnmInt' l   = min (head l) (mnmInt' (tail l))

min' :: Int -> Int -> Int
min' x y | x <= y    = x
         | otherwise = y

-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt []     = error "empty list"
maxInt [x]    = x
maxInt (x:xs) = max x (maxInt xs)

-- Exercise 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst x []                 = []
removeFst x (y:ys) | x == y    = ys
                   | otherwise = y : removeFst x ys

-- Example 1.11
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : srtInts (removeFst m xs) where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let m = mnmInt xs
              in m : srtInts' (removeFst m xs)

-- Example 1.12
average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

average' :: [Int] -> Float
average' [] = error "empty list"
average' xs = fromIntegral (sum' xs) / fromIntegral (length' xs)

-- Exercise 1.13
count :: Char -> String -> Int
count c ""                 = 0
count c (x:xs) | c == x    = 1 + count c xs
               | otherwise = count c xs

-- Exercise 1.14: Uses built-ins not mentioned in the book up to this point.
mltStr :: Int -> String -> String
mltStr i s | i == 0    = []
           | otherwise = s ++ mltStr (i - 1) s

blowup :: String -> String
blowup [] = []
blowup s  = blowup (init s) ++ mltStr (length s) [last s]

-- Exercise 1.15
mnmStr :: String -> Char
mnmStr []     = error "empty string"
mnmStr [x]    = x
mnmStr (x:xs) = min x (mnmStr xs)

removeFstStr :: Char -> String -> String
removeFstStr x []                  = []
removeFstStr x (y:ys) | x == y     = ys
                      | otherwise  = y : removeFstStr x ys

srtStr :: String -> String
srtStr [] = []
srtStr xs = m : srtStr (removeFstStr m xs) where m = mnmStr xs

srtStr' :: String -> String
srtStr' [] = []
srtStr' xs = let m = mnmStr xs
             in m : srtStr (removeFstStr m xs)

-- Example 1.16
prefix :: String -> String -> Bool
prefix [] ys         = True
prefix xs []         = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

-- Exercise 1.17
substring :: String -> String -> Bool
substring [] ys     = True
substring xs []     = False
substring xs (y:ys) =  prefix xs (y:ys) || substring xs ys

-- Exercise 1.19 : Try out flip
squareFstAddSnd :: Float -> Float -> Float
squareFstAddSnd x y = x*x + y

-- Intermezzo
factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n

factors' :: Integer -> [Integer]
factors' n | n < 1     = error "argument not positive"
           | n == 1    = []
           | otherwise = let p = ld n
                         in p : factors (div n p)

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Exercise 1.20
lengths :: [[a]] -> [Int]
lengths [] = error "empty list of lists"
lengths xs = map' length' xs

-- Exercise 1.21
sumLengths :: [[a]] -> Int
sumLengths [] = 0
sumLengths xs = sum' (map length' xs)

-- Intermezzo
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []                 =  []
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise = filter' p xs

-- Example 1.22
primes0 :: [Integer]
primes0 = filter' prime0 [2..]

-- Example 1.23
ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

ldp :: Integer -> Integer
ldp = ldpf primes1

primes1 :: [Integer]
primes1 = 2 : filter' prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive number"
        | n == 1    = False
        | otherwise = ldp n == n

-- Exercise 1.24: Nothing. It's shorter and linter suggest to write it that way.

-- Epilogue
a = 3
b = 4
f :: Integer -> Integer -> Integer
f x y = x^2 + y^2

g :: Integer -> Integer -- returns 0
g 0 = 0
g x = 2 * g (x-1)

factorialOf :: Integer -> Integer
factorialOf n | n < 0     = error "argument must be positive"
              | n == 0    = 1
              | otherwise = n * factorialOf (n - 1)
