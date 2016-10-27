import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect n ( Vect m a)

repeat : (n : Nat) -> a ->Vect n a
repeat Z        a = Nil
repeat (S k) a = a :: repeat k a

vtake : (n : Nat) -> Vect (n+m) a -> Vect n a
vtake Z 		xs = Nil
vtake (S k) (x :: xs) = x :: vtake k xs

vdrop : (n : Nat) -> Vect (n+m) a -> Vect m a
vdrop Z		xs = xs
vdrop (S k) (x :: xs) = vdrop k xs

trans : Matrix n m a -> Matrix m n a
trans {n = Z}  {m = k} Nil = repeat k Nil
trans (x :: xs) 			= Vect.zipWith (::) x (trans xs)

mul : Num a => Matrix n m a -> Matrix m k a -> Matrix n k a
mul Nil 	  _ = Nil
mul (x :: xs) ys = map (f x ) (trans ys) :: (mul xs ys) where
		f :  Num a => Vect n a -> Vect n a -> a
		f as bs = sum ( Vect.zipWith (*) as bs )


	