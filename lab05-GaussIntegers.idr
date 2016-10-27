module GaussIntegers
{-
  1) Define datatype GaussI for representing gaussian integers.
  2) Define instances of the type classes Eq and Show (use form "re+i*im").
  3) Define functions cplus and cmult for addition and multiplication of gaussian integers.
  4) Define wrappers that specify the semigroup and monoid instances that use cplus and
     cmult respectively together with mentioned instances. ???????????
  5) Define instance Cast String GaussI.
-}

data GaussInteger = G_Int Int Int

implementation Show GaussInteger where
    show (G_Int x y) = (show x) ++ "+i*" ++ (show y)
 
implementation Eq GaussInteger where
      (G_Int x y) ==  (G_Int a b) =  (x==a) && (y==b)

cplus : GaussInteger -> GaussInteger -> GaussInteger
cplus (G_Int x y) (G_Int a b) = G_Int (x+a) (y+b)

cmult : GaussInteger -> GaussInteger -> GaussInteger
cmult (G_Int x y) (G_Int a b) = G_Int (x*a-y*b) (y*a+x*b)

{-
   Gaussian sensor is a device which sends gaussian integers five times a day.
   Some of signals can be lost while transmitting. Day log is a string like this:
      
      "2+i*3 1+i*(-2) _ 3+i*3 2+i*0"
   
   where underscore corresponds to last data. Day log data should be represented 
   in the program as List (Maybe GaussI). 
   
   6) Define function which transforms day log string to day log data.
   
   7) Define function which computes sum or product of given day log data
      according to its argument. Named instance collectJust might be useful.
      You may find the following machinery useful either, try to understand 
      it in details:
-}
{-
data Op = Add | Mult

wrapper : Op -> Type
wrapper Add = Additive
wrapper Mult = Multiplicative

wrap : (op : Op) -> Nat -> wrapper op
wrap Add n = getAdditive n
wrap Mult n = getMultiplicative n

unwrap : (op : Op) -> wrapper op -> Nat
unwrap Add v = __pi_arg v
unwrap Mult v = __pi_arg v

-- Should you really keen to understand what is going on in this type signature, 
-- read this: http://goo.gl/HgDatg
combine : (op : Op) -> {default %instance m : Monoid (wrapper op)} -> List Nat -> Nat
combine op = unwrap op . concat . map (wrap op)

test : Bool
test = let xs = [1..5] in combine Add xs == 15 && combine Mult xs == 120

-- In case you are desperate to implement function 7) try easier variant with List GaussI 
-- instead of List (Maybe GaussI).
-}