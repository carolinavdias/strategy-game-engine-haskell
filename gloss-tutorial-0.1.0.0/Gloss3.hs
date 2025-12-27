module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Estado = (Float,Float)

estadoInicial :: Estado
estadoInicial = (0,0)

poligno :: Picture
poligno = Polygon [(0,0),(10,0),(10,10),(0,10),(0,0)]

desenhaEstado :: Estado -> Picture
desenhaEstado (x,y) = Translate x y poligno

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (x,y) = (x,y+5)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (x,y) = (x,y-5)
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (x,y) = (x-5,y)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (x,y) = (x+5,y)
reageEvento _ s = s -- ignora qualquer outro evento

reageTempo :: Float -> Estado -> Estado
reageTempo n (x,y) = (x,y-0.1)

fr :: Int
fr = 50

dm :: Display
dm = InWindow "Novo Jogo" (400, 400) (0, 0)

main :: IO ()
main = do 
          play dm              -- janela onde irá correr o jogo
               (greyN 0.5)     -- côr do fundo da janela
               fr              -- frame rate
               estadoInicial   -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo