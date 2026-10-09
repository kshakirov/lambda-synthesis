module LambdaInterpreter

import Syntax
import Lib.Parser

import Data.List1
--------------------------------------------------------------------------------
-- 1. СТАТИКА: ПАСПОРТА И ТИПЫ
--------------------------------------------------------------------------------


main : IO ()
main = do
    let rawText = "add 2 3" 
    let tokens  = forgetfulTokenize  rawText
    let rawTerms = parseComp tokens 
    print (show  tokens)
