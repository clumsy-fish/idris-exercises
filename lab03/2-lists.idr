-- Define function, which calculates sum of the fist and the last elements 
-- of the given list or returns 0 if the list is empty. 
-- Use functions `head` and `last`.

sum_fl : List Integer -> Integer
sum_fl Nil = 0
sum_fl (x :: xs) = (head (x :: xs)) + (last (x :: xs))

-- Define function, which returns length of the given list if there exists
-- 0 in there or returns (-1) otherwise.
-- Use functions `filter` and `length`. ????????????????????????????????????????

len_withZero : List Integer -> Int
--len_withZero xs = if (length (filter (0 == ) xs) == 0) then (-1) else length xs   

{-
 Define datatype with four possible values corresponding to various 
 list calculations:
   * sum
   * product
   * maximum
   * minimum
   
 Define function, which processes nonempty list according to the specified 
 calculation.
-}

data Calculation : Type where

	
process_list : Calculation -> (l: List Integer) -> {auto ok : NonEmpty l} -> Integer


{-
 Use previously defined function and datatype in the case when input data 
 are given in a string (first word in the string can be either SUM, PROD, 
 MIN, or MAX). Define whatever auxiliary functions you need.
 Your should return Nothing if the given string is somehow incorrect.
 
 Hint: functions `map` and `cast` may be useful.
 
 Recommendation: don't try to overcomplicate this function checking what 
 exactly follows the first word, just cast it to Integer without doubt.
-}

process_string : String -> Maybe Integer


-- Should be True
test : Bool
test = (process_string "SUM" == Nothing)
       && (process_string "SUM 1 2 3" == Just 6)
       && (process_string "MAX 5 2 3" == Just 5)
       && (process_string "PROD 1 0 5" == Just 0)
       && (process_string "MIN 1 0 5" == Just 0)
