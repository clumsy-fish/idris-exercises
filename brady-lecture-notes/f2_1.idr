import Data.Vect

data Cmp : Nat -> Nat -> Type where
	CmpLT' : (y : _) -> Cmp x (x + S y)
	CmpEQ' : Cmp x x
	CmpGT' : (x : _) -> Cmp (y + S x) y

cmp' : (n : Nat) -> (m : Nat) -> Cmp n m
cmp' Z 	 Z 	    = CmpEQ'
cmp' Z 	 (S k) 	= CmpLT' k
cmp' (S k)  Z 		= CmpGT' k
cmp' (S m) (S n) with (cmp' m n)
	cmp' _ _ | CmpLT' k = CmpLT' k
	cmp' _ _ | CmpGT' k = CmpGT' k
	cmp' _ _ | CmpEQ'   = CmpEQ'

vtake' : (m:Nat) -> Vect n a -> Maybe (Vect m a)
vtake' Z _ = Just Nil
vtake' (S l)  {n =(S k)} (x :: xs) with (cmp' (S k) (S l))
	vtake' (S k) (x :: xs) | CmpLT' = x :: vtake' k xs
	vtake' _ 	  xs 	   		 | CmpEQ' = Just xs
	vtake' _   	_          | CmpGT' = Nothing

{-vdrop' Z xs = Just xs
vdrop' _ Nil = Just xs
vdrop' k  {n = l} (x :: xs) with (cmp' k l)
	vdrop' (S k) (x :: xs) | CmpLT' = vdrop' k xs
	vdrop' _		_		       | CmpEQ' = Just Nil
	vdrop' _		_	         | CmpGT' = Nothing
-}
