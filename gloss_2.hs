import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import System.Random

-- posição e velocidade
type Ball = (Point, Vector)

-- raio da bola
ballRadius :: Float
ballRadius = 10

-- limites da “caixa” virtual
maxX, maxY :: Float
maxX = 300
maxY = 300

-- desenhar a bola
drawBall :: Ball -> Picture
drawBall ((x,y), _) =
  translate x y $
    color red $
      circleSolid ballRadius

-- atualizar a bola (movimento + colisão)
updateBall :: ViewPort -> Float -> Ball -> Ball
updateBall _ dt ((x,y),(dx,dy)) = ((x',y'), (dx',dy'))
  where
    (x',dx') = clip x dx (maxX - ballRadius)
    (y',dy') = clip y dy (maxY - ballRadius)

    clip h dh max
      | h' >  max = ( max, -dh)
      | h' < -max = (-max, -dh)
      | otherwise = (h', dh)
      where h' = h + dt * dh

-- número de frames por segundo
fps :: Int
fps = 60

-- janela
window :: Display
window = InWindow "Gloss Ball" (800, 600) (100, 100)

-- criar bola aleatória
randomBall :: IO Ball
randomBall = do
  x  <- randomRIO (-maxX, maxX)
  y  <- randomRIO (-maxY, maxY)
  dx <- randomRIO (-200, 200)
  dy <- randomRIO (-200, 200)
  return ((x,y),(dx,dy))

-- main
main :: IO ()
main = do
  ball <- randomBall
  simulate
    window
    black
    fps
    ball
    drawBall
    updateBall