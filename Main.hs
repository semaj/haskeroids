import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import Debug.Trace

ship :: Form
ship = filled white $ ngon 3 20.0

bullet :: Form
bullet = filled white $ rect 10.0 2.0

friction = 0.95
acceleration = 1.0
worldWidth = 600
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
  life :: Int,
  rot :: Double
} deriving Show

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

isAlive :: Bullet -> Bool
isAlive (Bullet _ _ life _) = life /= 50

newBullet :: Player -> Bullet
newBullet (Player x y _ _ r) = Bullet x y 0 r

moveBullet :: Bullet -> Bullet
moveBullet (Bullet x y life rot) = Bullet (x + nx) (y + ny) (life + 1) rot
        where nx = round $ 10.0 * cos rot
              ny = round $ 10.0 * sin rot

stepBullets :: Bool -> Player -> [Bullet] -> [Bullet]
stepBullets space player bullets = map moveBullet $ filter isAlive (newB ++ bullets)
          where newB = if space then [newBullet player] else []

collisions :: State -> State
collisions s = s

step :: ((Int, Int), Bool) -> State -> State
step ((dx, dy), space) (State player asteroids bullets) = 
    collisions $ State (stepPlayer (dx, dy) player)
                        asteroids 
                        (stepBullets space player bullets)

drawBullet :: Bullet -> Form
drawBullet (Bullet x y life r) =
  move ((wrapX x), (wrapY y)) $ rotate r bullet

render :: State -> Element
render (State p a b) | trace (show b) False = undefined
render (State (Player x y vx vy r) asteroids bullets) =
  collage worldWidth 
          worldHeight 
          ([move ((wrapX x), (wrapY y)) $ rotate r ship] ++ (map drawBullet bullets))

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
