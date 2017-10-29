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
The matter is one of finding proper basis vectors and combining them.
-}
