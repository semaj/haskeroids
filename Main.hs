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
  asteroids :: [Asteroid],
  bullets :: [Bullet]
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

stepPlayer :: (Int, Int) ->  Player -> Player
stepPlayer (dx, dy) (Player x y vx vy r) = moveP $ thrust dy $ Player x y vx vy newRotation
            where newRotation = r + ((realToFrac dx) / 10.0)

collisions :: State -> State
collisions s = s

step :: ((Int, Int), Bool) -> State -> State
step ((dx, dy), space) (State player asteroids bullets) = 
    collisions $ State (stepPlayer (dx, dy) player) asteroids bullets

render :: State -> Element
render (State (Player x y vx vy r) asteroids bullets) =
  collage worldWidth worldHeight [move ((wrapX x), (wrapY y)) $ rotate r ship]

wrapX :: Int -> Double
wrapX x = realToFrac $ mod x worldWidth

wrapY :: Int -> Double
wrapY y = realToFrac $ mod y worldHeight

sig :: Signal ((Int, Int), Bool)
sig = lift3 (\ x y z -> (x, z))
            Keyboard.arrows 
            (Time.every Time.millisecond) 
            (Keyboard.isDown Keyboard.SpaceKey)

main :: IO ()
main = run defaultConfig $ render <~ stepper
  where
    defaultPlayer = Player 100 100 0 0 0.0
    defaultAsteroid = [Asteroid 20 20 5 5]
    defaultBullets = []
    state = State defaultPlayer defaultAsteroid defaultBullets
    stepper = foldp step state sig
