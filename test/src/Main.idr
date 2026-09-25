module Main

import Check
import Data.Vect

rawLamIdentityHasExpectedType : Bool
rawLamIdentityHasExpectedType =
  case check (the (List String) []) (the (Context 0) [])
             (RawLam "x" TyInt (RawVar "x")) of
    Right (TyFun TyInt TyInt ** _) => True
    _                              => False

main : IO ()
main =
  if rawLamIdentityHasExpectedType
    then putStrLn "PASS: RawLam identity has type Int -> Int"
    else assert_total $ idris_crash "FAIL: RawLam identity has wrong type"
