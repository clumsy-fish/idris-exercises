import Task1

{-
  Write a complete program which prompts for an input, calls 
  the function `palindrome` and prints its output.
-}

main : IO ()
main = do
	putStrLn "This is unimplemented palidrome checker"
	putStrLn "Input string"
	str <- getLine
	if (palindrome  str)
		 then putStrLn "it is palindrome" 
		else  putStrLn "it is not palindrome"
