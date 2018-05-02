--Daniel Domme

--1

--S ::= C | C;S
--C ::= LD Int | ADD | MULT | DUP

type Prog = [Cmd]
type Stack = [Int]
data Cmd = LD Int
 | ADD
 | MULT
 | DUP
 | DEF String Prog
 | CALL String 
  deriving Show
type D = Maybe Stack -> Maybe Stack
semCmd :: Cmd -> D
semCmd (LD x) (Just s) = Just (x:s)

--where (Just (s:t:ss)) = Just s'		   
--semCmd (ADD) (s) = case s of 
semCmd (ADD) (Just (s:t:ss)) = Just ((s+t):ss)
semCmd (ADD) (Just _) = Nothing
semCmd (MULT) (Just (s:t:ss)) = Just ((s*t):(ss))
semCmd (MULT) (Just _) = Nothing
semCmd (DUP) (Just (s:ss)) = Just (s:s:ss)
semCmd (DUP) (Just _) = Nothing

some100 = semCmd (ADD) (Just [1])
some101 = semCmd (ADD) (Just [])
some = semCmd (LD 3) (Just [1..3])
some1 = semCmd (DUP) (Just [1])
some2 = semCmd (DUP) (Just []) 
some3 = semCmd (MULT) (Just [3,6])
some4 = semCmd (MULT) (Just [2])
some5 = semCmd (MULT) (Just [])
{-some1 = semCmd (ADD) ([1..4])
some2 = semCmd (MULT) ([1..5])
some3 = semCmd (DUP) ([5,4..1])
some4 = semCmd (ADD) ([1,2])
some5 =  lenth [6,8]-}

sem :: Prog -> D
sem _ Nothing = Nothing
sem [] s = s
sem (x:xs) s = sem xs (semCmd x s)
{-sem :: Prog -> D
sem [] s = s
sem (x:xs) s = do {semCmd (x) (s);
               sem (xs) (s)}-}
           
dosome = [LD 3,DUP,DUP,ADD,DUP,MULT]

t = (Just [])
list = sem dosome (Just [1..3])
dosome1 = [LD 3]
list1 = sem dosome1 (Just [])
dosome2 = []
list2 = sem dosome2 (Just [1])
list3 = sem dosome2 (Just [])
dosome3 = [MULT]
list4 = sem dosome3 (Just [4])
list5 = sem dosome3 (Just [])
list6 = sem dosome3 (Just [2,2])
dosome4 = [LD 2, MULT, DUP, MULT]
list7 = sem dosome4 (Just [])
list8 = sem dosome (Just [])
dosome5 = [LD 3, DUP, DUP]
list9 = sem dosome5 (Just [])
dosome6 = [LD 3, DUP, DUP, ADD, MULT]
list10 = sem dosome6 (Just [])

--2
--A
{-
type Prog = [Cmd]
type Stack = [Int]
data Cmd = LD Int
 | ADD
 | MULT
 | DUP
 | DEF String Prog
 | CALL String 
  deriving Show
-}
--B
type Macros = [(String,Prog)]
data State = Status Macros Stack   deriving Show
--C
semCmd2 :: Cmd -> State -> State
semCmd2 (LD s') (Status m s) = Status m (s':s)
semCmd2 (ADD) (Status m s') = case s' of 
                            (s:t:ss) -> (Status m ((s+t):ss))
                            _       -> error ("Problem with the operation")
semCmd2 (MULT) (Status m s') = case s' of 
                            (s:t:ss) -> (Status m ((s*t):ss))
                            _       -> error ("Problem with the operation")
semCmd2 (DUP) (Status m s') = case s' of 
                            (s:ss) -> (Status m (s:s:ss))
                            _       -> error ("Problem with the operation")
semCmd2 (DEF s p) (Status m s') = (Status ((s, p):m) s')
semCmd2 (CALL s) (Status m s') = case (lookup s m) of
                                  Nothing -> error ("macro doesn't exist")
                                  Just res -> sem2 (res) (Status m s')
{-semCmd2 (CALL s) (Status m s') = if (lookup s m == Nothing) 
                                  then error ("macro doesn't exist")
                                   else Just res
                                       sem2 (res) (Status m s')-}
--sem2
sem2 :: Prog -> State -> State
sem2 [] s = s
sem2  (x:xs) s = sem2 xs (semCmd2 x s)

--3
data CmdLogo = Pen Mode
 | MoveTo Int Int
 | Seq CmdLogo CmdLogo
 deriving Show
data Mode = Up | Down
  deriving Show

type StateLogo = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: CmdLogo -> StateLogo -> (StateLogo, Lines)
semS (Pen x) (m, n, o) = ((x, n, o), [])
semS (MoveTo x y) (Down, n, o) = ((Down, x, y), [(n, o, x, y)])
semS (Seq a  b) curState = (newest, line1 ++ line2)
                             where (newer, line1) = semS a curState
                                   (newest, line2) = semS b newer 

sem' :: CmdLogo -> Lines
sem' x = snd(semS (Pen Up) (Up, 0, 0))