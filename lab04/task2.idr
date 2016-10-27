import Data.Vect

-- Find information about function `zipWith` and reimplement
-- transpose_mat using `zipWith` instead `transpose_helper`.

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
-- Смотри файл f1.idr

-- Define matrix addition and multiplication

addMatrix : Num numType => Vect rows (Vect cols numType) 
                           -> Vect rows (Vect cols numType) 
                           -> Vect rows (Vect cols numType)
addMatrix a b = zipWith ( zipWith (+)) a b 

multMatrix : Num numType => Vect n (Vect m numType) 
                            -> Vect m (Vect p numType) 
                            -> Vect n (Vect p numType)
-- Смотри файл f1.idr