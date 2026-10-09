module Syntax

public export
data Ty = TyInt | TyFun Ty Ty


Show Ty where                    -- инстанс, где угодно ниже
  show TyInt = "TyInt"
  show (TyFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

public export
data RawTerm : Type where
  RawConst : Int -> RawTerm
  RawVar   : String -> RawTerm
  RawAdd   : RawTerm -> RawTerm -> RawTerm
  RawMul   : RawTerm -> RawTerm -> RawTerm
  RawLet   : String -> RawTerm -> RawTerm -> RawTerm
  RawLam   : String -> Ty -> RawTerm -> RawTerm
  RawApp   : RawTerm -> RawTerm -> RawTerm

Show RawTerm where 
 show   (RawConst a)  = "(" ++ "Const (" ++ show a ++ " ))"
 show   (RawVar  s)  = "(" ++ "Var (\"" ++ show s ++ "\" ))"
 show   (RawAdd a b) =   "(" ++ "Add(" ++ show a ++ "  " ++ show b ++ " ))"
 show   (RawMul a b) =   "(" ++ "Mul(" ++ show a ++ "  " ++ show b ++ " ))"
 show   (RawLet    s t1 t2 ) = "( Let "  ++ show s ++ " = " ++ show t1 ++ " in "++ show t2 ++ " )" 
 show   (RawLam  s ty  t2) = "( Lambda " ++  show s ++ " : " ++ show ty ++ " = " ++ show t2 ++ " )"
 show (RawApp  t1 t2) = "( " ++ show t1 ++ "(" ++ show t2 ++ "))"
