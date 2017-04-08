import Data.Vect


    data Expr = Const Int            --константы
                 |Var String           -- переменные
                 |App Expr Expr         -- применение
                 |Lambda String Expr     -- абстракция
                 |Op Char (Vect n Expr) -- N-нарная операция

data Environment = Env (List (String, Expr))

data Control =  CS Expr Environment -- я объединила c и v из методички в один тип

data Context = Mt
               |Fun Control Context
               |Arg Control Context
               |Opd (Vect k Control) Char (Vect l Control) Context --k+l = n

data StateCEK = State Control Context
