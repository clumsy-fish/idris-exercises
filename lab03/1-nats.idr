-- Examples from lecture
n0 : Nat
n0 = Z

n2 : Nat
n2 = S (S Z)

n5 : Nat
n5 = S(S(S(S(S Z))))

odd : Nat -> Bool
odd Z     = False
odd (S k) = not (odd k)

plus' : Nat -> Nat -> Nat
plus' Z     y = y
plus' (S k) y = S (plus' k y)

-- Define function `even` for determining if the given natural number is even
even : Nat -> Bool
even Z = True
even (S k) = not (even k)


-- Should be True
test1 : Bool
test1 = even n2 && even (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))

{-
Define recursive function, which is evaluated according the following rules:
               |  0,     if a < b
     a .-. b = | 
               |  a - b, otherwise
-}

infixl 10 .-.

(.-.) : Nat -> Nat -> Nat
(.-.) a Z = a 
(.-.) Z b = Z 
(.-.) (S a) (S b)= (.-.) a b


-- Should be True
test2 : Bool
test2 = (n5 .-. n2 == 3) && (n2 .-. n5 == 0)

{-
   Define function `abs'` for calculating absolute value of the difference between a b using (.-.):
     |a - b| = (a .-. b) + (b .-. a)
-}

abs' : Nat -> Nat -> Nat
abs' a b = ((.-.) a b) +  ((.-.) b a) 

-- Should be True
test3 : Bool
test3 = abs' n2 n5 == (S (S (S Z)))

{-
  Give another definition of `plus`, which is based on the pattern matching
  over the second argument.
-}

plus'' : Nat -> Nat -> Nat
plus'' x Z = x
plus'' x (S y) = S (plus'' x y)


-- Should be True
test4 : Bool
test4 = let answer = plus n2 n5 in 
         (plus'' n2 n5 == answer) && (plus'' n5 n2 == answer)
