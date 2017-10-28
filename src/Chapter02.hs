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
