module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import Worms
import Labs2025
import Tarefa2
import Tarefa1

-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Worms -> Worms
reageEventos (EventKey key Down _ _) worms = processaTecla key worms
reageEventos _ worms = worms

--------------------------------------------------------------------------------
-- * PROCESSAMENTO DE TECLAS

processaTecla :: Key -> Worms -> Worms
processaTecla (Char 'r') _ = estadoInicial  -- Reiniciar
processaTecla (Char ' ') worms = proximoTurno worms  -- Próximo turno
processaTecla key worms = 
  case modoJogo worms of
    Esperando -> processaJogada key worms
    _ -> worms

--------------------------------------------------------------------------------
-- * MOVIMENTOS

processaJogada :: Key -> Worms -> Worms
processaJogada (Char 'w') = executaJogada (Move Norte)
processaJogada (Char 'a') = executaJogada (Move Oeste)
processaJogada (Char 's') = executaJogada (Move Sul)
processaJogada (Char 'd') = executaJogada (Move Este)
processaJogada (Char 'q') = executaJogada (Move Noroeste)
processaJogada (Char 'e') = executaJogada (Move Nordeste)
processaJogada (Char 'z') = executaJogada (Move Sudoeste)
processaJogada (Char 'c') = executaJogada (Move Sudeste)

-- * ARMAS (disparo na direção que a minhoca está virada - Norte por padrão)
processaJogada (Char '1') = executaJogada (Dispara Jetpack Norte)
processaJogada (Char '2') = executaJogada (Dispara Escavadora Norte)
processaJogada (Char '3') = executaJogada (Dispara Bazuca Este)
processaJogada (Char '4') = executaJogada (Dispara Mina Sul)
processaJogada (Char '5') = executaJogada (Dispara Dinamite Sul)

processaJogada _ = id

--------------------------------------------------------------------------------
-- * EXECUÇÃO DE JOGADAS

executaJogada :: Jogada -> Worms -> Worms
executaJogada jogada worms =
  let num = minhocaAtual worms
      estadoAtual = estadoJogo worms
      novoEstado = efetuaJogada num jogada estadoAtual
  in if validaEstado novoEstado
     then worms 
       { estadoJogo = novoEstado
       , modoJogo = Animando 1.0  -- 1 segundo de animação
       , mensagem = descreveJogada num jogada
       }
     else worms { mensagem = "Jogada invalida!" }

descreveJogada :: NumMinhoca -> Jogada -> String
descreveJogada num (Move dir) = 
  "Minhoca " ++ show num ++ " moveu-se para " ++ show dir
descreveJogada num (Dispara arma dir) = 
  "Minhoca " ++ show num ++ " disparou " ++ show arma ++ " para " ++ show dir

--------------------------------------------------------------------------------
-- * GESTÃO DE TURNOS

proximoTurno :: Worms -> Worms
proximoTurno worms = 
  let estado = estadoJogo worms
      minhocas = minhocasEstado estado
      atual = minhocaAtual worms
      proximo = encontraProximaMinhocaViva (atual + 1) minhocas
  in case proximo of
       Just num -> worms 
         { minhocaAtual = num
         , modoJogo = Esperando
         , mensagem = "Jogador " ++ show (num + 1) ++ " - Sua vez!"
         }
       Nothing -> worms 
         { modoJogo = GameOver
         , mensagem = "GAME OVER - Pressione R para reiniciar"
         }

encontraProximaMinhocaViva :: NumMinhoca -> [Minhoca] -> Maybe NumMinhoca
encontraProximaMinhocaViva inicio minhocas = 
  encontra inicio ++ encontra 0
  where
    encontra i = case drop i minhocas of
      [] -> []
      (m:ms) -> case vidaMinhoca m of
        Viva _ -> [i]
        Morta -> encontra (i + 1)