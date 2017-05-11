import Data.Vect

-- все типы данных определены как в стратье про СЕК без каких-либо изменений

--тип данных определяющий выражение, которое мы хотим вычислить
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
    --окружение, которое хранит соответствия между переменными выражениями
    data Environment = Env (List (String, Control))
    -- пара в которой храниться выражение и окружение
    data Control =  CS Expr Environment -- я объединила c и v из методички в один тип

--контекст помнит, внутри какой из частей выражения мы сейчас находимся
data Context = -- пустой контекст
                Mt
                 --внутри лямбды
               |Fun Control Context
                --внутри аргумнта
               |Arg Control Context
               -- двнный контекст означает, что мы в данный момент считаем один из аргументов
               --первый вектор это уже посчитанные аргументы, второй это те, которые предстоит вычислить
               |Opd (Vect k Control) Char (Vect l Control) Context --k+l = n-1

-- состояние машины описывется парой выражения с окружением и контекста
data StateCEK = State Control Context

--вспомогательная функция которая из окружения находит сопоставление именно с той переменной, которая нам нужна
--возвращает выражение которое ствилось в соотвецтвие этой переменной
find : String -> Environment -> Control
find var (Env e) = case snd <$> (List.find pred e) of
                    Nothing => CS (Const 0) (Env [])
                    Just c => c
            where
                pred : (String, Control) -> Bool
                pred  (x, _) = if (var == x) then True else False

-- вспомогательная штука которая добовляет окружение операции к каждому ее аргументу
conv : Vect n Expr -> Environment -> Vect n Control
conv [] _ = []
conv (x::xs) e = (CS x e) :: conv xs e

--когда посчитаны все аргументы операции приходит время вычислить значение операци от полученных аргументов
--данная функция ставит выбирает по символу тип операции и выпомняет вычисление
--можно придумать N-нарную операцию, обозначить ее какимнибудь символом и добавить сюда
calc : Char -> Vect n Control -> Control -> Expr
calc f ((CS (Const a) _ )::Nill) (CS (Const b) _ ) = case f of
                                                        '+' => (Const (a+b))
                                                        '-' => (Const (a-b))
                                                        '*' => (Const (a*b))

-- собственно функция переходов, которая делает шаг если это возможно
step : (Control, Context) -> Maybe (Control, Context)
step (CS (App m n) e , k) = pure (CS m e, Arg (CS n e) k)  --cek1
step (CS (Op f (m :: t)) e, k) = pure (CS m e, Opd [] f (conv t e) k) --cek2
step (CS (Var x) e, k) = pure (find x e, k) --cek7
step (cs1, Arg cs2 k) = pure (cs2, Fun cs1 k)  --cek4
step (cs,Fun (CS (Lambda x m) (Env e)) k) = pure (CS m (Env ((x, cs)::e)),k) --cek3
step (cs, Opd arg f [] k) = pure (CS (calc f arg cs)(Env []), k) --cek6
step (cs1, Opd val f (cs2 :: arg) k) = pure (cs2, Opd (cs1::val) f arg k) --cek5
step _ = Nothing

--зависимый тип данных по типу DoorCmd
--Я ПОНЯТИЯ НЕ ИМЕЮ КАК ЕГО ИСПОЛЬЗОВАТЬ

-- тип возвращаемого занчения(он тут не нужен честно говоря но может пригодится)
-- состояние машины до перехода
-- состояние после перехода
-- переходы абсолютно такие же как в функции step
data StateCMD : Type -> StateCEK -> StateCEK -> Type
    where
        Cek1 : StateCMD () (State (CS (App m n) e ) k) (State (CS m e) (Arg (CS n e) k))
        Cek2 : StateCMD () (State (CS (Op f (m :: t)) e) k) (State (CS m e) (Opd [] f (conv t e) k))
        Cek3 : StateCMD () (State cs (Fun (CS (Lambda x m) (Env e)) k)) (State (CS m (Env ((x, cs)::e))) k)
        Cek4 : StateCMD () (State cs1 (Arg cs2 k)) (State cs2 (Fun cs1 k))
        Cek5 : StateCMD () (State cs1 (Opd val f (cs2 :: arg) k)) (State cs2 (Opd (cs1::val) f arg k))
        Cek6 : StateCMD () (State cs (Opd arg f [] k)) (State (CS (calc f arg cs) (Env [])) k)
        Cek7 : StateCMD () (State (CS (Var x) e) k) (State (find x e) k)

--главная проблема состоит в том, что я не знаю как используя StateCMD написать функцию которая бы делала
--переходы сама (по типу функции описанной ниже). В примере из книжки это все используется для того чтобы показать
-- что нельзя, например,из Open перети в Open и т.п., но совсем не ясно как делать переходы до тех пор, пока это возможно.


--функция которая делает переходы step пока это возможно
--возвращает последнее состояние на котором остановиласть машина
cek : StateCEK -> StateCEK
cek s@(State c cs) = case step (c,cs) of
                Nothing => s
                Just (c1,cs1) => cek (State c1 cs1)

--примерчики
term1 : Expr
term1 =  App (Lambda "x" (Var "x")) (Const 5)

term2 : Expr
term2 =  App (App (Lambda "f" (Lambda "x" (App (Var "f")(Var "x"))))(Lambda "y" (Op '+' [Var "y", Var "y"]))) (Const 1)

term3 : Expr
term3 =  App (App (Lambda "y" (Lambda "x" (Op '-' [Var "x", Var "y"])))(Const 7)) (Const 3)

--функция-обертка, которая упаковывает начальное выражение в пустое окружение и контекст
start : Expr -> StateCEK
start term =  State (CS term (Env [])) Mt

--функция, которая получает из завершающего состояния результирующее выражение
get_term : StateCEK -> Expr
get_term (State (CS term _) _ )= term

main : IO ()
main = do
    putStrLn "Test for CEK machine"
    putStrLn ((show term1) ++ "  --->  " ++ (show $ get_term $ cek $ start term1))
    putStrLn ((show term2) ++ "  --->  " ++ (show $ get_term $ cek $ start term2))
    putStrLn ((show term3) ++ "  --->  " ++ (show $ get_term $ cek $ start term3))
