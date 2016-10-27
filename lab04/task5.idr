{-
An integer arithmetic expression can take one of the following forms:
  1. A single integer
  2. Addition of an expression to an expression
  3. Subtraction of an expression from an expression
  4. Multiplication of an expression with an expression
Define a recursive data type Expr which can be used to represent such
expressions.

Look at datatype Picture (task 3) for ideas.
-}

data Expr = Single Int
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr


-- Implement function, which evaluates an integer arithmetic expression.
evaluate : Expr -> Int 
evaluate (Single x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mul x y) = (evaluate x) * (evaluate y)

test_ex : Expr
test_ex =  Sub (Mul (Single 5) (Add (Single 8) (Single 1))) (Single 12)  -- 5*(8+1)-12

test_evaluate : Bool 
test_evaluate = evaluate test_ex == 33  