type Prog = [Cmd]
data Cmd = Ld Int
         | Add
         | Mult
         | Dup
         
type Stack = [Int]
type D = Stack -> Stack

sem :: Prog -> D
sem [] stack = stack

semCmd :: Cmd -> D
semCmd (Ld x) stackList = (x:stackList)
-- sem (Add) (StackList) = ((StackList !! 0) + (stackList !! 1))
-- sem (Add) (x:stackList) = ((head stackList) + (head (tail stackList))) -- (drop 2 stackList)
-- sem (Add) (x:stackList) = (x + (head stackList))
-- semCmd Add (List == []) = Nothing
semCmd (Add) ([]) = []
semCmd (Add) (x:y:stackList) = (x+y):stackList
semCmd (Mult) ([]) = []
semCmd (Mult) (x:y:stackList) =  (x*y):stackList 
semCmd (Dup) (x:stackList) = (x:x:stackList)

-------------------Exercise 2 part a---------------------------------------------------------

data C = LD Int
       | ADD
       | MULT
       | DUP
       | Def String
       | Call String 
	   
-------------------Exercise 2 part b---------------------------------------------------------      

type Macros = [(String,Prog)]
type State = (Macros, Stack)
type Prg = [C]

-------------------Exercise 2 part c---------------------------------------------------------

semCmd2 :: C -> D
semCmd2 (LD num) (x,y) = (x, (num:y))
--semCmd2 (ADD) (x,())