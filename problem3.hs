-- Exercise 3. Mini Logo
-- Taylor Griffin

data Cmd = Pen Mode
  | MoveTo Int Int
  | Seq Cmd Cmd
  deriving (Eq,Show)

data Mode = Up | Down
  deriving (Eq,Show)

type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

-- semS

semS :: Cmd -> State -> (State,Lines)
semS (Pen mode) (_, x, y) = ((mode, x, y),[])
semS (MoveTo a b) (mode, x, y) =
  if mode == Up
    then ((Up, a, b),[])
    else ((Down, a, b),[(x,y,a,b)])
semS (Seq cmd1 cmd2) state = (lastState, l1 ++ l2)
  where (newState, l1) = semS cmd1 state
        (lastState, l2) = semS cmd2 newState

-- sem'

sem' :: Cmd -> Lines
sem' cmd = snd (semS cmd (Up, 0, 0))
