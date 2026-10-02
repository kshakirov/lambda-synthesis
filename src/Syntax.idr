module Syntax

public export
data Ty = TyInt | TyFun Ty Ty

public export
data RawTerm : Type where
  RawConst : Int -> RawTerm
  RawVar   : String -> RawTerm
  RawAdd   : RawTerm -> RawTerm -> RawTerm
  RawMul   : RawTerm -> RawTerm -> RawTerm
  RawLet   : String -> RawTerm -> RawTerm -> RawTerm
  RawLam   : String -> Ty -> RawTerm -> RawTerm
  RawApp   : RawTerm -> RawTerm -> RawTerm
