module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

circulo :: Picture
circulo = Circle 10

window :: Display
window = InWindow "Janela de Exemplo" -- título da janela
                  (1000, 1000)          -- dimensão da janela
                  (10,10)             -- posição no ecrã

type Estado = (Ponto,Velocidade,[Ponto])
type Ponto = (Float,Float)
type Velocidade = Ponto

desenhaEstado :: Estado -> Picture
desenhaEstado (ponto,_,pontos) = Pictures (desenhaPonto ponto : map desenhaPonto pontos)
    where
    desenhaPonto :: Ponto -> Picture
    desenhaPonto (x,y) = Translate x y circulo

-- altera a posição do círculo ao longo do tempo, sofrendo o efeito da gravidade
reageTempo :: Float -> Estado -> Estado
reageTempo t ((x,y),(vx,vy),pontos) = ((x',y'),(vx',vy'),(x',y'):pontos)
    where
    ax = 0
    ay = -9.8
    vx' = vx + ax * t
    vy' = vy + ay * t
    x' = x + vx' * t
    y' = y + vy' * t

estadoInicial :: Estado
estadoInicial = ((0,0),(20,20),[(0,0)])

main :: IO ()
main = simulate window white 40 estadoInicial desenhaEstado (const reageTempo)

