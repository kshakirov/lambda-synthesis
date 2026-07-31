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


strToToken : String -> Token 
strToToken s = OPAR

tokenize : String -> List1 Token
tokenize s = map strToToken   (split (== ' ')  s )
--tokenize s = [""]
         




