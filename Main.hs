import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import Debug.Trace

-- it's a triangle
ship :: Form
ship = filled white $ polygon $ path [(0.0, 0.0), (30.0, 0.0), (15.0, -60.0)]

data Obj = Obj {
  x :: Int,
  y :: Int,
  vx :: Int,
  vy :: Int
}

data State = State {
  player :: Obj,
  asteroids :: [Obj]
}

step :: (Int, Int) -> State -> State
-- step (dx, dy) (State (Obj x y vx vy) asteroids) | trace (show dx) False = undefined
step (dx, dy) (State (Obj x y vx vy) asteroids) = 
  State {
    player = Obj (x + (10 * dx)) (y + (10 * dy)) dx dy,
    asteroids = asteroids
  }

render :: State -> Element
render (State (Obj x y vx vy) asteroids) =
  centeredCollage 500 500 [move ((realToFrac x), (realToFrac y)) $ ship]

sig :: Signal (Int, Int)
sig = lift2 (\ x y -> x) Keyboard.arrows (Time.every Time.millisecond)

main :: IO ()
main = run defaultConfig $ render <~ stepper
  where
    defaultPlayer = Obj 100 100 5 5
    defaultAsteroid = [Obj 20 20 5 5]
    state = State defaultPlayer defaultAsteroid
    stepper = foldp step state sig
