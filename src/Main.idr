module Main

import Decidable.Equality
import Data.Vect

data Ty = TyInt

DecEq Ty where
  decEq TyInt TyInt = Yes Refl

Context : Nat -> Type
Context n = Vect n Ty

data HasType : (idx : Nat) -> (t : Ty) -> (ctx : Context n) -> Type where
  First : HasType Z t (t :: ctx)
  Later : HasType idx t ctx -> HasType (S idx) t (u :: ctx)

data RawTerm : Type where
  RawConst : Int -> RawTerm
  RawVar   : String -> RawTerm
  RawAdd   : RawTerm -> RawTerm -> RawTerm
  RawLet   : String -> RawTerm -> RawTerm -> RawTerm

data Term : Context n -> Ty -> Type where
  Const : Int -> Term ctx TyInt
  Var   : HasType idx t ctx -> Term ctx t
  Add   : Term ctx TyInt -> Term ctx TyInt -> Term ctx TyInt
  Let   : (t1 : Ty) -> Term ctx t1 -> Term (t1 :: ctx) t2 -> Term ctx t2

data Env : Context n -> Type where
  Empty  : Env []
  Extend : Int -> Env ctx -> Env (t :: ctx)

lookup : HasType idx t ctx -> Env ctx -> Int
lookup First     (Extend val _)   = val
lookup (Later k) (Extend _   env) = lookup k env

lookupVar : String -> List String -> Maybe Nat
lookupVar name [] = Nothing
lookupVar name (x :: xs) = 
  if name == x 
     then Just Z 
     else map S (lookupVar name xs)

mkHasType : (idx : Nat) -> (ctx : Context n) -> Maybe (t : Ty ** HasType idx t ctx)
mkHasType Z     (t :: ctx) = Just (t ** First)
mkHasType (S k) (u :: ctx) = do
  (t ** prf) <- mkHasType k ctx
  pure (t ** Later prf)
mkHasType _     []         = Nothing

check : {n : Nat} -> 
        (names : List String) -> 
        (ctx : Context n) -> 
        RawTerm -> 
        Either String (t : Ty ** Term ctx t)
check names ctx (RawConst val) = Right (TyInt ** Const val)
check names ctx (RawVar name) = case lookupVar name names of
  Nothing => Left ("Переменная не найдена: " ++ name)
  Just idx => case mkHasType idx ctx of
    Nothing => Left "Ошибка индекса контекста"
    Just (t ** prf) => Right (t ** Var prf)
check names ctx (RawAdd left right) = do
  (tyL ** termL) <- check names ctx left
  (tyR ** termR) <- check names ctx right
  case (decEq tyL TyInt, decEq tyR TyInt) of
    (Yes Refl, Yes Refl) => Right (TyInt ** Add termL termR)
    _ => Left "Сложить можно только Int"
check names ctx (RawLet varName valExpr bodyExpr) = do
  (t1 ** termVal) <- check names ctx valExpr
  (t2 ** termBody) <- check (varName :: names) (t1 :: ctx) bodyExpr
  Right (t2 ** Let t1 termVal termBody)

eval : {ctx : Context n} -> {t : Ty} -> Term ctx t -> Env ctx -> Int
eval (Const val)        env = val
eval (Var prf)          env = lookup prf env
eval (Add l r)          env = eval l env + eval r env
eval (Let t1 valExpr body) env = 
  let val = eval valExpr env
  in eval body (Extend val env)

interpret : RawTerm -> String
interpret raw = case check [] [] raw of
  Left err => "Ошибка static: " ++ err
  Right (t ** ironTree) => 
    let res = eval ironTree Empty
    in "Result is : " ++ show res

main : IO ()
main = do
  -- let expr = RawLet "x" (RawConst 5) (RawAdd (RawVar "x") (RawConst 2))
  let expr = RawConst 5
  putStrLn (interpret expr)
