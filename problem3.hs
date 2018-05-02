-- Exercise 3. Mini Logo


-- Pre-defined stuff
data Cmd = Pen Mode
  | MoveTo Int Int
  | Seq Cmd Cmd
  deriving (Eq)

data Mode = Up | Down
  deriving (Eq)

type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

-- my function
semS :: Cmd -> State -> (State,Lines)
semS (Pen Up) (mode, x, y) = ((Up, x, y),[(x,y,x,y)])
semS (Pen Down) (mode, x, y) = ((Down, x, y),[(x,y,x,y)])
semS (MoveTo a b) (mode, x, y) =
  if mode == Up
    then ((Up, a, b),[(a,b,a,b)])
    else ((Down, a, b),[(x,y,a,b)])
-- semS (Seq cmd1 cmd2) = semS cmd2 (semS cmd1)
