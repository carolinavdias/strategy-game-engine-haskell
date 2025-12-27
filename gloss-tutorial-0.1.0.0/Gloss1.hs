module Main where

import Graphics.Gloss

circulo :: Picture
circulo = Circle 50

outroCirculo :: Picture
outroCirculo = Translate (-40) 30 circulo

circuloVermelho = Color red circulo

circuloAzul = Color blue outroCirculo

circulos = Pictures [circuloVermelho, circuloAzul]

window :: Display
window = InWindow "Janela de Exemplo" -- título da janela
                  (200, 200)          -- dimensão da janela
                  (10,10)             -- posição no ecrã

background :: Color
background = greyN 0.8

main :: IO ()
main = display window background circulos