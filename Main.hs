import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import Debug.Trace

ship :: Form
ship = filled white $ ngon 3 20.0

-- wow. unit circle comes in handy
thrust :: Int -> Obj -> Obj
thrust dy obj | dy >= 0 = obj
thrust dy (Obj x y vx vy r) = Obj (x + (round nx)) (y + (round ny)) vx vy r
      where (nx, ny) = (4.0 * cos r, 4.0 * sin r)

data Obj = Obj {
  x :: Int,
  y :: Int,
  vx :: Int,
  vy :: Int,
  rotation :: Double
}

data State = State {
  player :: Obj,
  asteroids :: [Obj]
}

step :: (Int, Int) -> State -> State
-- step (dx, dy) (State (Obj x y vx vy) asteroids) | trace (show dx) False = undefined
step (dx, dy) (State (Obj x y vx vy r) asteroids) = 
  State {
    player = thrust dy $ Obj x y dx dy (r + ((realToFrac dx) / 10.0)),
    asteroids = asteroids
  }

render :: State -> Element
render (State (Obj x y vx vy r) asteroids) =
  centeredCollage 500 500 [move ((realToFrac x), (realToFrac y)) $ rotate r ship]

sig :: Signal (Int, Int)
sig = lift2 (\ x y -> x) Keyboard.arrows (Time.every Time.millisecond)

main :: IO ()
main = run defaultConfig $ render <~ stepper
  where
    defaultPlayer = Obj 100 100 5 5 0.0
    defaultAsteroid = [Obj 20 20 5 5 0.0]
    state = State defaultPlayer defaultAsteroid
    stepper = foldp step state sig
