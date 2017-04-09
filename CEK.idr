import Data.Vect

data Expr = Const Int            --константы
                 |Var String           -- переменные
                 |App Expr Expr         -- применение
                 |Lambda String Expr     -- абстракция
                 |Op Char (Vect n Expr) -- N-нарная операция
mutual
    data Environment = Env (List (String, Control))

    data Control =  CS Expr Environment -- я объединила c и v из методички в один тип

data Context = Mt
               |Fun Control Context
               |Arg Control Context
               |Opd (Vect k Control) Char (Vect l Control) Context --k+l = n-1

data StateCEK = State Control Context

find : String -> Environment -> Maybe Control
find var (Env e) = snd <$> (List.find pred e)
        where
        pred : (String, Control) -> Bool
        pred  (x, _) = if (var == x) then True else False

conv : Vect n Expr -> Environment -> Vect n Control
conv [] _ = []
conv (x::xs) e = (CS x e) :: conv xs e

calc : Char -> Vect n Control -> Control -> Maybe Expr
calc f ((CS (Const a) _ )::Nill) (CS (Const b) _ ) = case f of
                                                        '+' => pure (Const (a+b))
                                                        '-' => pure (Const (a-b))
                                                        '*' => pure (Const (a*b))
calc _ _ _ = Nothing

step : (Control, Context) -> Maybe (Control, Context)
step (CS (App m n) e , k) = pure (CS m e, Arg (CS n e) k)  --cek1
step (CS (Op f (m :: t)) e, k) = pure (CS m e, Opd [] f (conv t e) k) --cek2
step (CS (Var x) e, k) = case find x e of        --cek7
                          Nothing => Nothing
                          Just c => pure (c, k)
step (cs1, Arg cs2 k) = pure (cs2, Fun cs1 k)  --cek4
step (cs,Fun (CS (Lambda x m) (Env e)) k) = pure (CS m (Env ((x, cs)::e)),k) --cek3
step (cs, Opd arg f [] k) = case calc f arg cs of --cek6
                            Nothing => Nothing
                            Just val => pure (CS val (Env []), k)
step (cs1, Opd val f (cs2 :: arg) k) = pure (cs2, Opd (cs1::val) f arg k) --cek5
step _ = Nothing

-- это я просто проверяла руками работает ли step
{-term : Expr
term =  App (Lambda "x" (Var "x")) (Const 5)

state1 : (Control, Context)
state1 = (CS term (Env []), Mt)

state2 : (Control, Context)
state2 = (CS (Lambda "x" (Var "x")) (Env []), Arg (CS (Const 5) (Env [])) Mt)

state3 : (Control, Context)
state3 = (CS (Const 5) (Env []), Fun (CS (Lambda "x" (Var "x")) (Env [])) Mt)

state4 : (Control, Context)
state4 = (CS (Var "x") (Env [("x", CS (Const 5) (Env []))]),Mt)

state5 : (Control, Context)
state5 = (CS (Const 5) (Env []), Mt)
-}
