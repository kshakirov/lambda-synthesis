module Check

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
  RawMul   : RawTerm -> RawTerm -> RawTerm
  RawLet   : String -> RawTerm -> RawTerm -> RawTerm

data Term : Context n -> Ty -> Type where
  Const : Int -> Term ctx TyInt
  Var   : HasType idx t ctx -> Term ctx t
  Add   : Term ctx TyInt -> Term ctx TyInt -> Term ctx TyInt
  Mul   : Term ctx TyInt -> Term ctx TyInt -> Term ctx TyInt
  Let   : Term ctx t1 -> Term (t1 :: ctx) t2 -> Term ctx t2

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
check names ctx (RawConst val) = Right ( TyInt ** Const val)
check names ctx (RawVar name)   = case lookupVar name names of 
  Nothing => Left "No such variable name"
  Just (idx)=>  case mkHasType idx ctx of 
    Nothing => Left "No such type "
    Just (t ** prf) => Right (t ** Var prf)


check names ctx (RawAdd l r)   = case check names ctx l of 
  Left s => Left "Fst operand is invalid"
  Right (t1 ** prf1) => case check names ctx r of 
       Left s2 => Left "Snd operand is invalid"
       Right (t2 ** prf2) => case decEq  t2 t1 of
         Yes _ =>    Right (t2 **  prf2) 
         No _ =>  Left ("Types dont match")
       
check names ctx (RawMul l r)   = ?check_mul
check names ctx (RawLet v val body) = ?check_let
