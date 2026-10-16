module StackIR

import Syntax


Stack: Type
Stack = List Ty

data Op : (s1: Stack) -> (s2: Stack) -> Type  where 
PushInt  :  (s1:Stack) -> Int -> Op  s1 (TyInt::s1)
AddInt   : (s1: Stack)   -> Op (TyInt::(TyInt::s1)) (TyInt::s1)
testIR : IO ()
testIR = putStrLn "New Chez"

