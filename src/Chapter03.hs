module Chapter03
    (
    ) where

-- Overture
evens = [ x | x <- [0..100], even x]

sumOf2EvensIsEven = all even [ m + n | m <- evens, n <- evens]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n
  where
    ldp = ldpf primes
    ldpf (p:ps) m | rem m p == 0 = p
                  | p^2 > m      = m
                  | otherwise    = ldpf ps m
    primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve (0:xs) = sieve xs
sieve (n:xs) = n : sieve (mark xs 1 n)
 where
   mark :: [Integer] -> Integer -> Integer -> [Integer]
   mark (y:ys) k m | k == m    = 0 : mark ys 1 m
                   | otherwise = y : mark ys (k+1) m

primes :: [Integer]
primes = sieve [2..]


-- Exercise 3.38
oddsFrom3 :: [Integer]
oddsFrom3 = 3 : map (+2) oddsFrom3


-- Exercise 3.39
examples = [take n primes | n <- [0..], not (prime (product (take n primes) + 1)) ]

-- Intermezzo
mersenne = [ (p, 2^p - 1) | p <- primes, prime (2^p - 1) ]

notMersenne = [ (p, 2^p - 1) | p <- primes, not (prime (2^p - 1)) ]

-- Exercise 3.41
pdivisors :: Integer -> [Integer]
pdivisors n = [ d | d <- [1..(n-1)], rem n d == 0]

perfectNumbers :: [Integer]
perfectNumbers = [ n | n <- [1..], sum (pdivisors n) == n]


-- Intermezzo
primePairs :: [(Integer, Integer)]
primePairs = pairs primes
  where
    pairs (x:y:xys) | x + 2 == y = (x, y): pairs (y:xys)
                    | otherwise  = pairs (y:xys)


-- Exercise 3.42
primeTriples :: [(Integer, Integer, Integer)]
primeTriples = triples primes
  where
    triples (x:y:z:xyzs)
      | x + 2 == y && y + 2 == z = (x, y, z) : triples (y:z:xyzs)
      | otherwise                = triples (y:z:xyzs)
