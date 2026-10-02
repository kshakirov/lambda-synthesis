# Примеры границы Parser → Check

## 1. Сложение

Строка программы:

```text
( add 2 3 )
```

После токенизации:

```text
[OPAR, ADD, NUMBER 2, NUMBER 3, CPAR]
```

Вход `parseComp`:

```text
[OPAR, ADD, NUMBER 2, NUMBER 3, CPAR]
```

Выход `parseComp`:

```text
Right (RawAdd (RawConst 2) (RawConst 3), [])
```

Пустой список во второй части означает, что парсер потребил весь вход.

Вход `check` после успешного полного разбора:

```text
names = []
ctx   = []
term  = RawAdd (RawConst 2) (RawConst 3)
```

Результат по смыслу:

```text
Right (TyInt ** железное дерево Add)
```

## 2. Let

Строка программы:

```text
( let x = 2 in ( add x 3 ) )
```

После токенизации:

```text
[ OPAR, LET, IDENT "x", EQ, NUMBER 2, IN,
  OPAR, ADD, IDENT "x", NUMBER 3, CPAR, CPAR ]
```

Ожидаемое сырое дерево:

```text
RawLet "x"
  (RawConst 2)
  (RawAdd (RawVar "x") (RawConst 3))
```

Ожидаемый полный результат парсера:

```text
Right
  ( RawLet "x" (RawConst 2) (RawAdd (RawVar "x") (RawConst 3))
  , []
  )
```

Вход `check`:

```text
names = []
ctx   = []
term  = RawLet "x" (RawConst 2) (RawAdd (RawVar "x") (RawConst 3))
```

Результат по смыслу:

```text
Right (TyInt ** железное дерево Let)
```

Текущее состояние: `parseLet` ещё не потребляет токен `IN`, поэтому этот
пример пока не доходит до ожидаемого результата парсера. Сам `check` уже умеет
проверять показанный `RawLet`, если передать дерево напрямую.

## Текущая нестыковка

```text
parseComp : List Token
         -> Either ParserError (RawTerm, List Token)

check : List String
     -> Context n
     -> RawTerm
     -> Either String (t : Ty ** Term ctx t)
```

Общий `RawTerm` теперь один. Не хватает верхней границы, которая принимает
результат парсера только при пустом хвосте и затем передаёт дерево в `check`.
