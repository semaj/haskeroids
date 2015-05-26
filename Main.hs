import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import Debug.Trace

ship :: Form
ship = filled white $ ngon 3 20.0

friction = 0.95
acceleration = 1.0
worldWidth = 800
worldHeight = 600
startBLife = 10

data Player = Player {
  x :: Int,
    y :: Int,
    vx :: Int,
    vy :: Int,
    rotation :: Double
}

data Asteroid = Asteroid {
  ax :: Int,
     ay :: Int,
     avx :: Int,
     avy :: Int
}

data Bullet = Bullet {
  bx :: Int,
  by :: Int,
  life :: Int
}

data State = State {
  player :: Player,
  asteroids :: [Asteroid]
}

-- wow. unit circle comes in handy
thrust :: Int -> Player -> Player
thrust dy player | dy >= 0 = player
thrust dy (Player x y vx vy r) = Player x y (vx + nx) (vy + ny) r
      where (nx, ny) = (round $ acceleration * cos r, round $ acceleration * sin r)

moveP :: Player -> Player
moveP (Player x y vx vy r) = Player nx ny vx vy r
          where nx = round $ friction * (fromIntegral (x + vx)) -- friction
                ny = round $ friction * (fromIntegral (y + vy))


step :: (Int, Int) -> State -> State
-- step (dx, dy) (State (Obj x y vx vy) asteroids) | trace (show dx) False = undefined
step (dx, dy) (State (Player x y vx vy r) asteroids) = 
    State player asteroids
    where newRotation = r + ((realToFrac dx) / 10.0)
          player = moveP $ thrust dy $ Player x y vx vy newRotation

render :: State -> Element
render (State (Player x y vx vy r) asteroids) =
  collage worldWidth worldHeight [move ((wrapX x), (wrapY y)) $ rotate r ship]

wrapX :: Int -> Double
wrapX x = realToFrac $ mod x worldWidth

wrapY :: Int -> Double
wrapY y = realToFrac $ mod y worldHeight

sig :: Signal (Int, Int)
sig = lift2 (\ x y -> x) Keyboard.arrows (Time.every Time.millisecond)

main :: IO ()
main = run defaultConfig $ render <~ stepper
  where
    defaultPlayer = Player 100 100 0 0 0.0
    defaultAsteroid = [Asteroid 20 20 5 5]
    state = State defaultPlayer defaultAsteroid
    stepper = foldp step state sig
