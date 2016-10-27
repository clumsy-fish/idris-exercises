import Data.Vect
import Data.Fin

-- Use implicit arguments to implement following function
vlength : Vect n a -> Nat
vlength {n = l} _ = l

-- sum of vectors (define recursively)
vadd : Num a => Vect n a -> Vect n a -> Vect n a
vadd {n = Z } _ _ = Nil
vadd (x :: xs ) (y :: ys ) = (x+y) :: vadd xs ys

-- scalar product (use functions zipWith and sum)
vScalarProd : Num a => Vect n a -> Vect n a -> a
vScalarProd xs ys = sum (zipWith  (*) xs ys)

-- replace all coordinates that are close to zero with zero ????????????????????
toAxis : (eps : Double) -> Vect n Double -> Vect n Double
--toAxis _ Nil = Nil
--toAxis eps (x :: xs) = if (abs(x) < eps) then (0 :: (toAxis xs)) else (x :: (toAxis xs))

-- increase vector dimension, adding 0 as the first coordinate ?????????????????77
incDimFirst : Vect n Double -> Vect (n+1) Double 
-- incDimFirst xs =  reverse (incDimLast (reverse xs ) )
 
-- increase vector dimension, adding 0 as the last coordinate
incDimLast : Vect n Double -> Vect (n+1) Double
incDimLast  Nil =  0 :: Nil 
incDimLast  (x :: xs) = x :: incDimLast xs

-- project vector to the space given by vector of indices
-- use function map
project : Vect n a -> Vect k (Fin n) -> Vect k a
project xs ys = map ( flip index xs) ys

test1 : Bool
test1 = project [1,2,3,4] [FS FZ, FZ, FS (FS FZ)] == [2,1,3]
        && project [1,2,3,4] [FS FZ, FS FZ, FS (FS (FS FZ))] == [2,2,4] 
        && project [0] [FZ, FZ, FZ, FZ] == [0,0,0,0]
--      Next tests don't compile, why? Reasons differ!
--      && project [1,2,3,4] [FS FZ, FS FZ, (FS (FS (FS (FS FZ))))] == [2,2,0] 
--      && project [0] [FZ, FZ, FZ, FZ] == [0,0,0]


-- find an index of an element in the vector
pos : (v : Vect n a) -> Elem x v -> Fin n
pos _  Here = FZ
pos (x :: xs) (There  l ) = FS (pos  xs l)

test2 : Bool
test2 = pos [1,2,3,4] (There (There Here)) == FS (FS FZ)


-- Implement and test following function ????????????????????????
vswap : (v1 ** Elem x v1) -> (v2 ** Elem y v2) -> ((v1' ** Elem y v1'), (v2' ** Elem x v2'))


test_vswap : Bool

