module Main where

import Graphics.Gloss

import Desenhar
import Eventos
import Worms
import Tempo


janela :: Display
janela = InWindow "Worms - LI1 2025" (1920, 1080) (0, 0)

fundo :: Color
fundo = makeColorI 20 20 40 255  -- azul escuro

fr :: Int
fr = 60

-- | Função principal que invoca o Gloss para correr o jogo.
main :: IO ()
main = do
  putStrLn "========================================="
  putStrLn "       WORMS - LI1 2025"
  putStrLn "========================================="
  putStrLn ""
  putStrLn "Controlos:"
  putStrLn "  WASD - Mover (Q/E para diagonais)"
  putStrLn "  1 - Jetpack"
  putStrLn "  2 - Escavadora"
  putStrLn "  3 - Bazuca"
  putStrLn "  4 - Mina"
  putStrLn "  5 - Dinamite"
  putStrLn "  SPACE - Próximo turno"
  putStrLn "  R - Reiniciar jogo"
  putStrLn ""
  putStrLn "A iniciar o jogo..."
  putStrLn "========================================="

  play janela fundo fr estadoInicial desenha reageEventos reageTempo
