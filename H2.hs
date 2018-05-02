type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show

type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

--sem :: Prog -> D
sem :: [Cmd] -> Stack -> Stack
sem [] stack = stack
sem (x:xs) c = sem xs (semCmd x c)



--type Macros = [(String,Prog)]
--type State = (Macros, Stack)
--type Prg = [C]

-------------------Exercise 2 part c---------------------------------------------------------

