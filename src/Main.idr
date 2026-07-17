module Main





import Data.Vect

-- 1. Типы нашего целевого языка (чертежи суждений)
data Ty = TyInt | TyArr Ty Ty

-- 2. Контекст Гамма (вектор активных гипотез)
Context : Nat -> Type
Context n = Vect n Ty

-- 3. Отношение принадлежности (доказательство существования переменной)
data HasType : Context n -> Ty -> Type where
    Here  : HasType (t :: ctx) t
    There : HasType ctx t -> HasType (any :: ctx) t




main : IO ()
main = putStrLn "Hello from Idris2!"

