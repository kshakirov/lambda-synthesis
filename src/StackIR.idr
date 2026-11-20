module StackIR 

import Syntax


Stack: Type
Stack = List Ty

data Op : (s1: Stack) -> (s2: Stack) -> Type  where 
                      PushInt  :  (s1:Stack) -> Int -> Op  s1 (TyInt::s1)
                      AddInt   : (s1: Stack)   -> Op (TyInt::(TyInt::s1)) (TyInt::s1)


data Code: Stack -> Stack -> Type where 
Done : Code s s
Step : Op s1 s2 -> Op s2 s3 -> Op s1 s3 


testRunPushInt : Op s (TyInt :: s) -> List Int -> List Int 
testRunPushInt  (PushInt _ n )  stack  = n :: stack

testAddInt : Op  (TyInt::(TyInt :: s1)) (TyInt :: s1) -> List Int -> List Int 
testAddInt  (AddInt  _  ) (a :: (b :: c))=  (a + b ) :: c 
testAddInt  (AddInt  _  ) [a]=   []
testAddInt  (AddInt  _  ) []=   []



testIR : IO ()
testIR =  do 
       let r =  testRunPushInt (PushInt [] 3) [] 
       putStrLn ("Result of Running PushInt  is " ++ (show r))

