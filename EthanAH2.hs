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
--sem (x:xs) (Just c) | (sem xs(semCmd x (Just c))) == Nothing = Nothing
--                    | otherwise = (sem xs(semCmd x (Just c)))

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


{-semCmd Add(Just list) = case length list of
                        0 -> Nothing
                        1 -> Nothing
                        _ -> Just (((head list) + (head (tail list))) : drop 2 list)-}
-- sem (Add) (StackList) = ((StackList !! 0) + (stackList !! 1))
-- sem (Add) (x:stackList) = ((head stackList) + (head (tail stackList))) -- (drop 2 stackList)
-- sem (Add) (x:stackList) = (x + (head stackList))
-- semCmd Add (List == []) = Nothing
--semCmd (ADD) ([]) = []
--semCmd (ADD) (x:y:stackList) = (x+y):stackList
{-semCmd (MULT) ([]) = []
semCmd (MULT) (x:y:stackList) =  (x*y):stackList
semCmd (DUP) (x:stackList) = (x:x:stackList)
-}
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
