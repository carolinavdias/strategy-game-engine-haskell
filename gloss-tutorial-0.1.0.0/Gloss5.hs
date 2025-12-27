module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

-- * Estado Jogo

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

-- * Estado Gloss

type EstadoGloss = (Estado,Picture)

estadoGlossInicial :: Picture -> EstadoGloss
estadoGlossInicial bola = (estadoInicial,bola)

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss ((x,y),bola) = Translate x y bola

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e,bola) = (reageEvento ev e,bola)

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (e,bola) = (reageTempo t e,bola)

fr :: Int
fr = 50

dm :: Display
dm = InWindow "Novo Jogo" (400, 400) (0, 0)

main :: IO ()
main = do 
    Just bola <- loadJuicy "bola.png"
    play dm                       -- janela onde irá correr o jogo
        (greyN 0.5)               -- côr do fundo da janela
        fr                        -- frame rate
        (estadoGlossInicial bola) -- estado inicial
        desenhaEstadoGloss        -- desenha o estado do jogo
        reageEventoGloss          -- reage a um evento
        reageTempoGloss           -- reage ao passar do tempo