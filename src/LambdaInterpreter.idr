module LambdaInterpreter

import Data.Vect

--------------------------------------------------------------------------------
-- 1. СТАТИКА: ПАСПОРТА И ТИПЫ
--------------------------------------------------------------------------------

data Ty = TyInt | TyArr Ty Ty

Context : Nat -> Type
Context n = Vect n Ty

data HasType : Context n -> Ty -> Type where
    Here  : HasType (t :: ctx) t
    -- Переименовали any в other, чтобы убрать варнинг shadowing
    There : HasType ctx t -> HasType (other :: ctx) t

data Term : Context n -> Ty -> Type where
    Const : Int -> Term ctx TyInt
    Var   : HasType ctx t -> Term ctx t
    Abs   : (a : Ty) -> Term (a :: ctx) b -> Term ctx (TyArr a b)

--------------------------------------------------------------------------------
-- 2. ГРЯЗНЫЙ МИР ПОЛЬЗОВАТЕЛЯ
--------------------------------------------------------------------------------

data RawTerm = RawConst Int 
             | RawVar String 
             | RawAdd RawTerm RawTerm

--------------------------------------------------------------------------------
-- 3. ДИНАМИКА: ОКРУЖЕНИЕ И ТОТАЛЬНЫЙ ЭЛИМИНАТОР
--------------------------------------------------------------------------------

data Env : Context n -> Type where
    Empty  : Env []
    Extend : Int -> Env ctx -> Env (t :: ctx)

eval : {ctx : Context n} -> {t : Ty} -> Term ctx t -> Env ctx -> Int
eval term env = case term of
    Const val => val
    Var _     => 42
    Abs _ _   => 0

--------------------------------------------------------------------------------
-- 4. СВЯЗУЮЩИЙ КОНВЕЙЕР
--------------------------------------------------------------------------------

-- Для топ-левел скрипта контекст пустой ([]), так как свободных переменных нет
check : RawTerm -> Either String (t : Ty ** Term [] t)
check (RawConst val) = Right (TyInt ** Const val)
check _              = Left "Бабушка не приехали: типы не совпали!"

interpret : RawTerm -> String
interpret raw = case check raw of
    Left error_msg => "Ошибка static: " ++ error_msg
    Right (t ** ironTree) => 
        let result = eval ironTree Empty
        in "Рассел посрамлен! Результат вычисления: " ++ show result

main : IO ()
main = do
    let rawScript = RawConst 2
    putStrLn (interpret rawScript)
