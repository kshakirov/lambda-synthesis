module LambdaInterpreter

import Syntax
import Lib.Parser
import Check
import Data.List1
import Data.Vect
--------------------------------------------------------------------------------
-- 1. СТАТИКА: ПАСПОРТА И ТИПЫ
--------------------------------------------------------------------------------


main : IO ()
main = do
    let rawText = "( add 2 3 )" 
    let tokens  = forgetfulTokenize  rawText
    print (show tokens)
    let rawTerms = parseComp tokens 
    case rawTerms of 
      Right (r,rl) =>  
        let tree = check [] [] r in
        case tree of 
        Right t => print (show  (fst t))
        Left ss => print  "All is very bad too "
      Left s => print  "All is very bad too "
