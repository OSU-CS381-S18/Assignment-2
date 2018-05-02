type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         deriving Show

type Stack = [Int]
type D = Stack -> Stack

--sem :: Prog -> D
sem :: [Cmd] -> Stack -> Stack
sem [] stack = stack
sem (x:xs) c = sem xs (semCmd x c)

--semCmd :: Cmd -> D
semCmd :: Cmd -> Stack -> Stack
--semCmd (LD x) = x
-- sem (Add) (StackList) = ((StackList !! 0) + (stackList !! 1))
-- sem (Add) (x:stackList) = ((head stackList) + (head (tail stackList))) -- (drop 2 stackList)
-- sem (Add) (x:stackList) = (x + (head stackList))
-- semCmd Add (List == []) = Nothing
semCmd (ADD) ([]) = []
semCmd (ADD) (x:y:stackList) = (x+y):stackList
semCmd (MULT) ([]) = []
semCmd (MULT) (x:y:stackList) =  (x*y):stackList
semCmd (DUP) (x:stackList) = (x:x:stackList)

-- example programs
--
p :: Prog
p = [LD 3,DUP,ADD,DUP,MULT]

q :: Prog
q = [LD 3,ADD]

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
