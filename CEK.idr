import Data.Vect

data Expr = Const Int            --константы
                 |Var String           -- переменные
                 |App Expr Expr         -- применение
                 |Lambda String Expr     -- абстракция
                 |Op Char (Vect n Expr) -- N-нарная операция

implementation Show Expr where
        show (Const a) = show a
        show (Var s) = show s
        show (App e1 e2) = "(" ++ (show e1) ++ (show e2) ++ ")"
        show (Lambda x e) = "(L" ++ (show x) ++ "." ++ (show e) ++ ")"
        show (Op c v) = (show c) ++ (show v)

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

cek : StateCEK -> StateCEK
cek s@(State c cs) = case step (c,cs) of
                Nothing => s
                Just (c1,cs1) => cek (State c1 cs1)

term1 : Expr
term1 =  App (Lambda "x" (Var "x")) (Const 5)

term2 : Expr
term2 =  App (App (Lambda "f" (Lambda "x" (App (Var "f")(Var "x"))))(Lambda "y" (Op '+' [Var "y", Var "y"]))) (Const 1)

start : Expr -> StateCEK
start term =  State (CS term (Env [])) Mt

get_term : StateCEK -> Expr
get_term (State (CS term _) _ )= term

main : IO ()
main = do
    putStrLn "Test for CEK machine"
    putStrLn ((show term1) ++ "  --->  " ++ (show $ get_term $ cek $ start term1))
    putStrLn ((show term2) ++ "  --->  " ++ (show $ get_term $ cek $ start term2))
