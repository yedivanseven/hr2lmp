module Chapter02
    (
    ) where

-- Prelude
not' :: Bool -> Bool
not' True  = False
not' False = True

infixr 3 @@ --right associative and precedence 3, as we learn later
(@@) :: Bool -> Bool -> Bool -- and
False @@ x = False
True  @@ x = x

-- Example 2.1: inclusive or
infixr 2 ##  --right associative and precedence 2, as we learn later
(##) :: Bool -> Bool -> Bool
False ## x = x
True  ## x = True

-- Exercise 2.2: exclusive or
infixr 2 ~~ --right associative and precedence 2, as we learn later
(~~) :: Bool -> Bool -> Bool
False ~~ x = x
True  ~~ x = not' x

-- Intermezzo
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infix 1 -->
(-->) :: Bool -> Bool -> Bool
False --> x = True
True  --> x = x

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

-- Exercise 2.4: The following is correct and equivalent to ~~ above
infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

-- Intermezzo
p = True
q = False
formula1 = not p && ((p ==> q) <=> not (q && not p))

-- Example 2.5
formula2 p q = not p && ((p ==> q) <=> not (q && not p))

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = bf True && bf False

excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =    bf True  True
            && bf True  False
            && bf False True
            && bf False False

form1 p q =  p ==> (q ==> p)
form2 p q = (p ==> q) ==> p

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True, False],
                             q <- [True, False],
                             r <- [True, False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s | p <- [True, False],
                               q <- [True, False],
                               r <- [True, False],
                               s <- [True, False]]

-- Exercise 2.9
logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q <=> bf2 p q | p <- [True, False],
                                               q <- [True, False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
               (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [bf1 p q r <=> bf2 p q r | p <- [True, False],
                                                   q <- [True, False],
                                                   r <- [True, False]]

formula3 p q = p
formula4 p q = (p <+> q) <+> q

formula5 p q = p <=> (p <+> q) <+> q

-- Excercise 2.11
test1  = logEquiv1 id (\ p -> not (not p))
test2a = logEquiv1 id (\ p -> p && p)
test2b = logEquiv1 id (\ p -> p || p)
test3a = logEquiv2 (\ p q -> p ==> q) (\ p q -> not p || q)
test3b = logEquiv2 (\ p q -> not (p ==> q)) (\ p q -> p && not q)
test4a = logEquiv2 (\ p q -> not p ==> not q) (\ p q -> q ==> p)
test4b = logEquiv2 (\ p q -> p ==> not q) (\p q -> q ==> not p)
test4c = logEquiv2 (\ p q -> not p ==> q) (\p q -> not q ==> p)
test5a = logEquiv2 (\ p q -> p <=> q) (\ p q -> (p ==> q) && (q ==> p))
test5b = logEquiv2 (\ p q -> p <=> q) (\p q -> (p && q) || (not p && not q))
test6a = logEquiv2 (\ p q -> p && q) (\p q -> q && p)
test6b = logEquiv2 (\ p q -> p || q) (\p q -> q || p)
test7a = logEquiv2 (\ p q -> not (p && q)) (\p q -> not p || not q)
test7b = logEquiv2 (\ p q -> not (p || q)) (\p q -> not p && not q)
test8a = logEquiv3 (\ p q r -> p && (q && r)) (\p q r -> (p && q) && r)
test8b = logEquiv3 (\ p q r -> p || (q || r)) (\p q r -> (p || q) || r)
test9a = logEquiv3 (\ p q r -> p && (q || r)) (\p q r -> (p && q) || (p && r))
test9b = logEquiv3 (\ p q r -> p || (q && r)) (\p q r -> (p || q) && (p || r))

-- Exercise 2.13
test10a = logEquiv1 (\ p -> not True) (\ p -> False)
test10b = logEquiv1 (\ p -> not False) (\ p -> True)
test11  = logEquiv1 (\p -> p ==> False) (\ p -> not p)
test12a = logEquiv1 (\ p -> p || True) (\ p -> True)
test12b = logEquiv1 (\ p -> p && False) (\ p -> False)
test13a = logEquiv1 (\ p -> p || False) (\ p -> p)
test13b = logEquiv1 (\ p -> p && True) (\ p -> p)
test14  = logEquiv1 (\p -> p || not p) (\p -> True)
test15  = logEquiv1 (\p -> p && not p) (\p -> False)

-- Exercise 2.15
testPropContra1 :: (Bool -> Bool) -> Bool
testPropContra1 bf = and [not (bf p) | p <- [True, False]]

testPropContra2 :: (Bool -> Bool -> Bool) -> Bool
testPropContra2 bf = and [not (bf p q) | p <- [True, False],
                                         q <- [True, False]]

testPropContra3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
testPropContra3 bf = and [not (bf p q r) | p <- [True, False],
                                           q <- [True, False],
                                           r <- [True, False]]

-- Exercise 2.16
{-
1. The equation x^2 + 1 = 0 does not have a solution.
2. A largest naturak number does exist.
3. The number 13 is not a prime number.
4. The number n is not a prime number.
5. There is only a finite number of primes.
-}

-- Ecercise 2.17
{-
x < y < z says that (x < y) and (y < z).
Since not (P and Q) is equivalent to (not P) or (not Q), we have:
x < y < z is equivalent to x >= y or y >= z
-}

-- Exercise 2.20
test2201 = logEquiv2 (\ p q -> not p ==> q) (\ p q -> p ==> not q) --False
test2202 = logEquiv2 (\ p q -> not p ==> q) (\ p q -> q ==> not p) --False
test2203 = logEquiv2 (\ p q -> not p ==> q) (\ p q -> not q ==> p) --True
test2204 = logEquiv3 (\ p q r -> p ==> (q ==> r))                  --True
                     (\ p q r -> q ==> (p ==> r))
test2205 = logEquiv3 (\ p q r -> p ==> (q ==> r))                  --False
                     (\ p q r -> (p ==> q) ==> r)
test2206 = logEquiv3 (\ p q r -> (p ==> q) ==> p)                  --True
                     (\ p q r -> p)
test2207 = logEquiv3 (\ p q r -> p || q ==> r)                     --True
                     (\ p q r -> (p ==> r) && (q ==> r))

-- Exercise 2.21
-- 1.
truthTable2 :: (Bool -> Bool -> Bool) -> [[Bool]]
truthTable2 bf = [[p, q, bf p q] | p <- [True, False],
                                   q <- [True, False]]
formula2211 :: Bool -> Bool -> Bool
formula2211 p q = p || not q

test2211 = truthTable2 formula2211

{-
2.
There are 2^n, where n is the number of rows with n = 2^v,
where v is the number of boolean variables.
So in our case, we have v = 2, n = 4 and, thus, 16 Tables
3. Probably.
4 - 5.
Write everything in disjunct normal form.
-}

-- Intermezzo
square1 :: Integer -> Integer
square1 x = x^2

square2 :: Integer -> Integer
square2 = \ x -> x^2

m1 :: Integer -> Integer -> Integer
m1 = \ x -> \ y -> x*y

m2 :: Integer -> Integer -> Integer
m2 = \ x y -> x*y

m2' :: (Integer, Integer) -> Integer
m2' = \ (x, y) -> x*y

solvQdr :: (Float, Float, Float) -> (Float, Float)
solvQdr = \ (a, b, c) -> if a == 0 then error "not quadratic"
                         else let d = b^2 - 4*a*c in
                           if d < 0 then error "no real solutions"
                         else ((-b + sqrt d)/ 2*a, (-b - sqrt d)/ 2*a)

solvQdr' :: (Float, Float, Float) -> (Float, Float)
solvQdr' (a, b, c)
                | a == 0    = error "non quadratic"
                | d < 0     = error "no real solutions"
                | otherwise = ((-b + sqrt d) / 2*a, (-b - sqrt d) / 2*a)
                where d = b^2 - 4*a*c

solvQdr2 :: (Float, Float, Float) -> (Float, Float)
solvQdr2 = \ (a, b, c) -> let d = b^2 - 4*a*c
                          in case () of
                                  _ | a == 0    -> error "not quadratic"
                                    | d < 0     -> error "no real solution"
                                    | otherwise -> ((-b + sqrt d) / 2*a,
                                                    (-b - sqrt d) / 2*a)

solvQdr3 :: (Float, Float, Float) -> (Float, Float)
solvQdr3 (a, b, c) = let d = b^2 - 4*a*c
                     in case () of
                       _ | a == 0    -> error "not quadratic"
                         | d < 0     -> error "no real solution"
                         | otherwise -> ((-b + sqrt d) / 2*a,
                                         (-b - sqrt d) / 2*a)

{- Exercise 2.37
1.a False
1.b False
1.c False
1.d False
1.e False
1.f False
2.a True
2.b False
2.c True
2.d False
2.e False
2.f False
3.a False
3.b False
3.c False
3.d False
3.e False
3.f False
4.a True
4.b False
4.c False
4.d False
4.e False
4.f False
5.a True
5.b False
5.c False
5.d False
5.e False
5.f False
-}

{-
a.1 for no x
a.2 x arbitray
a.3 for no x
a.4 x = 0
a.5 x arbitray
b.1 for no x
b.2 for all x > 0
b.3 for no x
b.4 for no x
b.5 for x > 0
c.1 for no x
c.2 x arbitrary
c.3 for no x
c.4 for no x
c.5 for no x
d.1 for no x
d.2 for all x >= 0
d.3 for no x
d.4 for no x
d.5 for all x > 0 and x != 1
e.1 for no x
e.2 for all fathers
e.3 for no x
e.4 for no x
e.5 for all fathers, who do not have children with their daughters
f is rather subjective ...
-}

-- Intermezzo
any', all' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p
all' p = and . map p

every, some :: [a] -> (a -> Bool) -> Bool
every xs p = all p xs
some  xs p = any p xs

unique :: (a -> Bool) -> [a] -> Bool
unique p []     = error "cannot operate on empy list"
unique p [x]    = p x
unique p (x:xs) | not (p x) && any p xs = unique p xs
                | otherwise             = p x && not (any p xs)

parity :: [Bool] -> Bool
parity []     = error "cannot operate on empty list"
parity [x]    = not x
parity (x:xs) = x == not (parity xs)

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p
