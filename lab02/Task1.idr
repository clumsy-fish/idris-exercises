module Task1

{-
  1) Determine types of the following values without referring to REPL
  and then check your ideas with ":t". Don't forget to mention your
  mistakes if any.

  1) ("A", "B", "C") : (String, String, String)
  2) ["A", "B", "C"] : List String
  3) ["A", "B", 'C'] : мне кажется эта штука не прокатит
  4) ("A", "B", 'C') : (String, String, Char)
  5) (("A", "B"), 'C') : ((String, String), Char)
  6) ("A", ("B", 'C')) :(String, (String, Char))
  7) [["A", "B"], ["C"]] : List (List String)
  8) [("A", "B"), "C"] : это плохо!!!!!!!!!1
-}

{-
  2) Implement case-insensitive function `palindrome` using
  library functions reverse and toLower. Make sure your
  implementation passes tests given.
-}

export
palindrome : String -> Bool
palindrome string =  let str = toLower string in str == reverse str

test_palindrome : IO ()
test_palindrome = if palindrome "pullup"
                     && palindrome "Kayak"
                     && palindrome "noON"
                     && palindrome "Glenelg"
                     && palindrome "tattArratTat"
                     && palindrome "kuulilennuteetunneliluuk"
                     && not (palindrome "Idris")
                   then putStrLn "Test passed"
                   else putStrLn "Test failed"

-- Do you know meanings of test words?

{-
  3) Write a function `counts`, which returns a pair of the number of words
  in the input and the number of characters in the input.  For example,
  the input "Hello, Idris world!" should give the output (3, 19) .
  Provide tests for this function following the idea from previous exercise.
-}

counts : String -> (Nat, Nat)
counts  str = (length (words str), length str)

{-
test_counts : IO ()
test_counts = if (counts "Nastya Kirichenko" == (2,17))
                     && (counts "Skoro 1 sentyabrya" == (3,18)
                     && (counts "Ploho" == (1, 5))
			then putStrLn "Test passed1"
                   else putStrLn "Test failed"
-}

{-
  4) Write a function `top_ten`, which returns the ten largest values in a list.
  Hint: You may find functions `take` and `sort` useful (use :t and :doc for details).
  Provide tests.
-}

top_ten: Ord a => List a -> List a
top_ten xs =  take 10 (reverse (sort xs))

test_top_ten : IO ()
test_top_ten = if (top_ten [1, 3 .. 100] == [99, 97, 95, 93, 91, 89, 87, 85, 83, 81])
                     && (top_ten [2, 4 .. 50]== [50, 48, 46, 44, 42,40, 38, 36, 34, 32])
                     &&  not (top_ten [1..100] == [1 .. 10])
                   then putStrLn "Test passed2"
                   else putStrLn "Test failed"

{-
  5) Write a function `over_length`, which returns the number of strings in the list
  longer than the given number of characters. For example, evaluating
     over_length 3 ["One", "Two", "Three", "Four"]
  should give the output 2.
  Provide tests.
-}

over_length : Nat -> List String -> Nat
over_length n Nil = Z
over_length n (x :: xs) = if length x > n then 1 + over_length n xs else  over_length n xs

test_over_length : IO ()
test_over_length = if (over_length 2 ( words "Skoro 1 sentyabrya") == 2)
                     && (over_length  6 ["hvkhbk", "lkmlkm","jnoij-pijpoj", "vikhb", "ybojn"]== 1 )
		   then putStrLn "Test passed3"
                   else putStrLn "Test failed"
