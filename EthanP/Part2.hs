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
------ Part 2
--type Stack = [Int]
type Macros = [(String,Prog2)]
type State = (Macros, Stack)
type Prog2 = [Cmd2]
data Cmd2 = LD2 Int
          | ADD2
          | MULT2
          | DUP2
          | DEF String Prog2
          | CALL String
          deriving Show

type D2 = Maybe State -> Maybe State
          
sem2 :: Prog2 -> D2
sem2 [] stack = stack

sem2 (x:xs) (Just c) = (sem2 xs (semCmd2 x (Just c)))

semCmd2 :: Cmd2 -> D2
semCmd2 (LD2 n) (Just(x,y)) = Just(x, (n:y))

semCmd2 ADD2 (Just(x, list)) = if length list == 0
                               then Nothing
                               else if length list == 1
                               then Nothing 
                               else Just (x, (((head list) + (head (tail list))) : drop 2 list))

semCmd2 MULT2 (Just(x, list)) = if length list == 0
                               then Nothing
                               else if length list == 1
                               then Nothing 
                               else Just (x, (((head list) * (head (tail list))) : drop 2 list))
                               
semCmd2 DUP2 (Just(x, list)) = if length list == 0
                               then Nothing
                               else Just (x, ((list !! 0) : list))

semCmd2 (DEF list stack) (Just (x,y)) = (Just(((list,stack):x), y))

semCmd2 (CALL p) (Just(x,y)) = case lookup p x of
                                        Just r -> sem2 r (Just (x,y));
                                        Nothing -> (Just(x,y));




