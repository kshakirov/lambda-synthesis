module Lib.Parser 
import Data.String
import Data.List1
import Data.List 
--------------------------------------------------------------------------------
-- 0. ЛЕКСИЧЕСКИЙ БАЗИС ДЛЯ ПАРСЕРА ПО ВИРТУ
--------------------------------------------------------------------------------

data Token = OPAR         -- Открывающая скобка (
           | CPAR         -- Закрывающая скобка )
           | COLON        -- Двоеточие :
           | ARROW        -- Стрелка =>
           | FN           -- Ключевое слово fn
           | CALL         -- Ключевое слово call
           | ADD          -- Ключевое слово add
           | INT_KW       -- Ключевое слово Int
           | NUMBER Int   -- Числовой литерал (хранит значение)
           | LET
           | IN 
           | EQ
           | IDENT String -- Идентификатор (хранит имя переменной)
           


isNumeric : String -> (Bool, Int)
isNumeric s = case parseInteger  s of
  Just  num => (True, num)
  Nothing =>(False, 0)

data RawTerm = RawConst Int 
             | RawVar String 
             | RawAdd RawTerm RawTerm

             |RawLet   String  RawTerm RawTerm




data ParserError = NotClosedPars
|FuncNotImlemented
|UknownError

strToToken : String -> Token 
strToToken s = case s of
  "(" => OPAR
  ")" => CPAR
  ":" => COLON
  "=>" => ARROW
  "fn" => FN
  "call" => CALL
  "add" => ADD
  "Int" => INT_KW
  "let" => LET
  "in"  => IN
  "="  => EQ
  x => 
    let (r, num)  = isNumeric x 
    in  if r then NUMBER num else  IDENT x -- latаваer change 1 for realnumbr


matchHelper : Char -> Char ->  Bool 
matchHelper r s    = if r == s then True else False 
  

srM : List Char -> List Char ->  List Char ->  Nat   -> Int  -> (Nat, Int)
srM r []  origR  c i  = (c, i)
srM [] s origR c i = (c,i)
srM (r::rs) ( s:: xs) origR   c i = case  s of 
   x => if (matchHelper r s )  then srM rs xs origR (c + 1) (i + 1) else srM origR xs origR 0 (i + 1)




runMarkovStep : (String, String) -> String -> String 
runMarkovStep rule s = 
  let left_s =unpack $  fst rule 
  in 
  let right_r = unpack $  snd rule 
  in 
  let list_string = unpack s 

  
  in s 
  
  
  
-- Примерная идея прокрутки правил
-- runRules : List (String, String) -> List Char -> List Char
-- runRules []        str = str  -- Ни одно правило не подошло, нормализация окончена
-- runRules (r :: rs) str =
--   matchPrefix (unpack $ fst) r str
  -- Пробуем применить текущее правило r через srM...
  -- Если совпало — подставляем и начинаем заново с полного списка правил!
  -- Если не совпало — выбывает r, и запускаем (runRules rs str)


tokenize : String -> List1 Token
tokenize s = map strToToken   (split (== ' ')  s )
forgetfulTokenize : String ->List Token
forgetfulTokenize s = 
  forget (tokenize s)
--tokenize s = [""]
testTokenize = 
  forgetfulTokenize "( add 2 1 )"


parseAdd : List Token -> Either ParserError (RawTerm, List Token)
parseLet : List Token -> Either ParserError (RawTerm, List Token)
parseAtom : List Token -> Either ParserError (RawTerm, List Token)

parseAtom [] = Right (RawVar "", [])
parseAtom (NUMBER x :: rest) = Right (RawConst x, rest)
parseAtom (IDENT x :: rest) = Right (RawVar x, rest)
parseAtom ( _ :: rest) = Left UknownError

parseComp : List Token -> Either ParserError (RawTerm, List Token)
parseComp (OPAR :: ADD ::tail) = parseAdd tail
parseComp (OPAR :: LET ::tail) = parseLet tail
parseComp tokens = parseAtom tokens  


parseAdd rest =
  case  parseComp rest of 
    Right (left , restL) =>
      case parseComp restL of 
        Right  (right, restR) =>  
          case restR of 
            (CPAR :: restOther) => Right (RawAdd left right , restOther)
            _ =>Left NotClosedPars
        Left error => Left error
    Left error => Left error 

--parseAdd other = Left FuncNotImlemented

parseLet rest = 
  case parseComp rest of
    Right(RawVar x, (EQ :: rightTail)) => 

      case parseComp rightTail of 
        Right (right, nextTail) => 
          case parseComp nextTail of 
            Right (inn, otherTail) => Right( RawLet x right inn, otherTail)
            Left err => Left err
        Left err => Left err
    Right(_,_) => Left FuncNotImlemented    
    Left err => Left err
           

testParseComp terms = 
  parseComp terms



