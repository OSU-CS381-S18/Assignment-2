----------------------------------
------------Lucien------------
--
--C ::= LD Int | ADD | MULT | DUP
--
--type Prog = [Cmd]
--data Cmd = LD Int
--                | ADD
--                | MULT
--                | DUP
--
-- type Stack = [Int]
--
-- sem :: Prog -> D
--
-- semCmd :: Cmd -> D

-- 1)-----------------------------
type Stack = [Int]
type Prog = [Cmd]
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
       deriving Show

type D = (Maybe Stack) -> (Maybe Stack)

sem :: Prog -> D 
sem []  s = s
sem (p:ps) s = sem ps (semCmd p s)

semCmd :: Cmd -> D
semCmd (LD p) (Just lst) = Just (p:lst)
semCmd ADD ( Just (s:r:lst)) = Just ((s+r):lst)
semCmd MULT (Just(s:r:lst)) = Just ((s*r):lst)
semCmd DUP ( Just (s:r:lst)) = Just (s:s:lst)

p :: Prog
p = [LD 3,DUP,ADD,DUP,MULT]

q :: Prog
q = [LD 3,ADD]
e = []

--Exo2-------------------------------
--A

type Prog1 = [Cmd]
type Stack1 = [Int]
data Cmd1 = LD1 Int
 | ADD1
 | MULT1
 | DUP1
 | DEF1 String Prog
 | CALL1 String 
  Show deriving 

--B------------------------------

type Macros = [(String,Prog2)]
type Prog2 = [Cmd2]
type State = (Macros,Stack)
data Cmd2 = LD2 Int
          | ADD2
          | MULT2
          | DUP2
          | DEF String Prog2
          | CALL String

          Show deriving 

sem2 :: Prog2 -> State -> State
sem2 [] s = s
sem2 (p:ps) s = sem2 ps (semCmd2 p s)
semCmd2 :: Cmd2 -> State -> State
semCmd2 (LD2 p) (x,y) = (x,(p:y))
semCmd2 ADD2 (x,(s:r:y)) = (x,((s+r):y))
semCmd2 MULT2 (x,(s:r:y)) = (x,((s*r):y))
semCmd2 DUP2 (x,(s:y)) = (x,(s:s:y))
semCmd2 (DEF p s) (x,y) = (((p,s):x),y)
semCmd2 (CALL p) (x,y) = case lookup p x of
                                        Just r -> sem2 r (x,y);
                                        Nothing -> (x,y);


-- Exo3-----------------------------------------------------------
data CmdLogo = Pen Mode
           | MoveTo Int Int
           | Seq CmdLogo CmdLogo
            Show deriving 

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