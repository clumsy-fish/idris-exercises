module GaussIntegers
{-
  1) Define datatype GaussI for representing gaussian integers.
  2) Define instances of the type classes Eq and Show (use form "re+i*im").
  3) Define functions cplus and cmult for addition and multiplication of gaussian integers.
  4) Define wrappers that specify the semigroup and monoid instances that use cplus and
     cmult respectively together with mentioned instances.
  5) Define instance Cast String GaussI.
-}

data GaussInteger = G_Int Int Int

implementation Show GaussInteger where
    show (G_Int x y) = (show x) ++ "+i*" ++ (show y)

implementation Eq GaussInteger where
      (G_Int x y) ==  (G_Int a b) = (x==a) && (y==b)

cplus : GaussInteger -> GaussInteger -> GaussInteger
cplus (G_Int x y) (G_Int a b) = G_Int (x+a) (y+b)

cmult : GaussInteger -> GaussInteger -> GaussInteger
cmult (G_Int x y) (G_Int a b) = G_Int (x*a-y*b) (y*a+x*b)

record GaussInteger_Multiplicative where
  constructor GaussInteger_GetMultiplicative
  _ : GaussInteger

record GaussInteger_Additive where
  constructor GaussInteger_GetAdditive
  _ : GaussInteger

implementation Semigroup GaussInteger_Multiplicative where
  (<+>) (GaussInteger_GetMultiplicative left) (GaussInteger_GetMultiplicative right) =
            GaussInteger_GetMultiplicative $ cmult left right

implementation Semigroup GaussInteger_Additive where
  (<+>) (GaussInteger_GetAdditive left) (GaussInteger_GetAdditive right) =
            GaussInteger_GetAdditive $ cplus left right

implementation Monoid GaussInteger_Additive where
        neutral = GaussInteger_GetAdditive $ G_Int 0 0

implementation Monoid GaussInteger_Multiplicative where
        neutral = GaussInteger_GetMultiplicative $ G_Int 1 0

implementation Cast String GaussInteger where
    cast str = G_Int x y where
        predicat: Char -> Bool
        predicat k = elem k ['0'..'9'] || k =='-'

        x: Int
        x = prim__fromStrInt . pack . takeWhile predicat $ unpack str

        y: Int
        y = prim__fromStrInt . pack . filter predicat $ dropWhile predicat $ unpack str -- наверное можно сделать короче, но это тоже неплохо

{-
   Gaussian sensor is a device which sends gaussian integers five times a day.
   Some of signals can be lost while transmitting. Day log is a string like this:

      "2+i*3 1+i*(-2) _ 3+i*3 2+i*0"

   where underscore corresponds to last data. Day log data should be represented
   in the program as List (Maybe GaussI).

   6) Define function which transforms day log string to day log data.
-}

logStringTologData : String -> List (Maybe GaussInteger)
logStringTologData str = map translate (words str)
        where translate : String -> Maybe GaussInteger
              translate "_" = Nothing
              translate str = Just (cast {to=GaussInteger} str)


datalist : List (Maybe GaussInteger)
datalist = let str = "2+i*3 1+i*(-2) _ 3+i*3 2+i*0" in logStringTologData str

datalist1 : List (Maybe GaussInteger)
datalist1 = let str = "_ _ _ _ _" in logStringTologData str

test_logStringTologData : Bool
test_logStringTologData = datalist == [Just (G_Int 2 3),Just (G_Int 1 (-2)), Nothing, Just (G_Int 3 3), Just (G_Int 2 0)]
                            && datalist1 ==[Nothing, Nothing, Nothing, Nothing, Nothing]

{-
    7) Define function which computes sum or product of given day log data
     according to its argument. Named instance collectJust might be useful.
     You may find the following machinery useful either, try to understand
     it in details:
-}

data Op = Add | Mult

wrapper : Op -> Type
wrapper Add = Additive
wrapper Mult = Multiplicative

wrap : (op : Op) -> Nat -> wrapper op
wrap Add n = GetAdditive n
wrap Mult n = GetMultiplicative n

unwrap : (op : Op) -> wrapper op -> Nat
unwrap Add  v = __pi_arg v
unwrap Mult v = __pi_arg v

-- Should you really keen to understand what is going on in this type signature,
-- read this: http://goo.gl/HgDatg
combine : (op : Op) -> {default %instance m : Monoid (wrapper op)} -> List Nat -> Nat
combine op = unwrap op . concat . map (wrap op)

test : Bool
test = let xs = [1..5] in combine Add xs == 15 && combine Mult xs == 120


wrapper' : Op -> Type
wrapper' Add = GaussInteger_Additive
wrapper' Mult = GaussInteger_Multiplicative

wrap': (op : Op) -> GaussInteger -> wrapper' op
wrap' Add x = GaussInteger_GetAdditive x
wrap' Mult x = GaussInteger_GetMultiplicative x

unwrap' : (op : Op) -> wrapper' op -> GaussInteger
unwrap' Add (GaussInteger_GetAdditive x) = x
unwrap' Mult (GaussInteger_GetMultiplicative x) = x

combine' : (op : Op) -> {default %instance m : Monoid (wrapper' op)} -> List (Maybe GaussInteger) -> Maybe GaussInteger
combine' op [] = Nothing
combine' op (Nothing :: xs) = combine' op xs
combine' op str =  Just $ unwrap' op (concat  $ concat <$> ((map $ wrap' op) <$>  str))

test_combine' : Bool
test_combine' = combine' Add datalist == Just (G_Int 8 4)
                && combine' Mult datalist == Just (G_Int 54 42)
                && combine' Add datalist1 == Nothing

-- In case you are desperate to implement function 7) try easier variant with List GaussI
-- instead of List (Maybe GaussI).
