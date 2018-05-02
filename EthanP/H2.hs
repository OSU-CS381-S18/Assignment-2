type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show

type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

sem :: Prog -> D
sem [] stack = stack
-- Using a Lambda Lifing expresion, this is a way to turn variables into arguments.
-- There are like five different ways to do this, check it out fellow hummans.
-- https://wiki.haskell.org/Let_vs._Where
sem (x:xs) (Just c) = if (sem xs(semCmd x (Just c))) == Nothing
                      then Nothing
                      else (sem xs(semCmd x (Just c)))

semCmd :: Cmd -> D
semCmd (LD x) (Just list) = (Just (x:list))

semCmd ADD(Just list) = if length list == 0
                        then Nothing
                        else if length list == 1
                        then Nothing
                        else Just (((head list) + (head (tail list))) : drop 2 list)

semCmd MULT(Just list) = if length list == 0
                       then Nothing 
                       else if length list == 1
                       then Nothing
                       else Just (((head list) * (head (tail list))) : drop 2 list)

semCmd DUP(Just list) = if length list == 0
                       then Nothing 
                       else Just ((list !! 0) : list)
-- example programs
--
p :: Prog
p = [LD 3,DUP,ADD,DUP,MULT]

q :: Prog
q = [LD 3,ADD]
e = []

empty = Just []
test1 = sem p empty
test2 = sem q empty
test3 = sem e empty
-------------------Exercise 2 part a---------------------------------------------------------
--
--data C = LD Int
--       | ADD
--       | MULT
--       | DUP
--       | Def String
--       | Call String

-------------------Exercise 2 part b---------------------------------------------------------

--type Macros = [(String,Prog)]
--type State = (Macros, Stack)
--type Prg = [C]

-------------------Exercise 2 part c---------------------------------------------------------

--semCmd2 :: C -> D
--semCmd2 (LD num) (x,y) = (x, (num:y))
--semCmd2 (ADD) (x,())
