module Lib.Parser 
import Data.String
import Data.List1
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
           | IDENT String -- Идентификатор (хранит имя переменной)


isNumeric : String -> (Bool, Int)
isNumeric s = case parseInteger  s of
  Just  num => (True, num)
  Nothing =>(False, 0)



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
  x => 
    let (r, num)  = isNumeric x 
    in  if r then NUMBER num else  IDENT x -- later change 1 for realnumbr


tokenize : String -> List1 Token
tokenize s = map strToToken   (split (== ' ')  s )
--tokenize s = [""]
         




