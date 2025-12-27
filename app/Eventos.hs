module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Labs2025
import Tarefa2
import Tarefa1

-- | Função que altera o estado do jogo no Gloss.
{-|
Module      : Eventos
Description : Processamento de eventos durante o jogo.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Processa input do jogador durante uma partida:
movimento, seleção de armas, disparos, pausa, etc.
-}

--------------------------------------------------------------------------------
-- * EVENTO PRINCIPAL DO JOGO

-- | Processa eventos durante uma partida
eventoJogo :: Event -> EstadoPartida -> EstadoJogo
eventoJogo evento partida
  | pausado partida = eventoJogoPausado evento partida
  | otherwise = eventoJogoAtivo evento partida

--------------------------------------------------------------------------------
-- * EVENTOS QUANDO PAUSADO

-- | Eventos quando o jogo está pausado
eventoJogoPausado :: Event -> EstadoPartida -> EstadoJogo
eventoJogoPausado evento partida = case evento of
  -- P - Despausa
  EventKey (Char 'p') Down _ _ ->
    Jogando (partida { pausado = False })
  
  EventKey (Char 'P') Down _ _ ->
    Jogando (partida { pausado = False })
  
  -- ESC - Volta ao menu
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    Menu (EstadoMenu OpcaoPlay 0.0)
  
  _ -> Jogando partida

--------------------------------------------------------------------------------
-- * EVENTOS DURANTE O JOGO

-- | Eventos quando o jogo está ativo
eventoJogoAtivo :: Event -> EstadoPartida -> EstadoJogo
eventoJogoAtivo evento partida = case evento of
  -- P - Pausa
  EventKey (Char 'p') Down _ _ ->
    Jogando (partida { pausado = True })
  
  EventKey (Char 'P') Down _ _ ->
    Jogando (partida { pausado = True })
  
  -- ESC - Volta ao menu
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    Menu (EstadoMenu OpcaoPlay 0.0)
  
  -- Movimento com WASD
  EventKey (Char 'w') Down _ _ -> moverMinhocaAtual Norte partida
  EventKey (Char 'W') Down _ _ -> moverMinhocaAtual Norte partida
  EventKey (Char 's') Down _ _ -> moverMinhocaAtual Sul partida
  EventKey (Char 'S') Down _ _ -> moverMinhocaAtual Sul partida
  EventKey (Char 'a') Down _ _ -> moverMinhocaAtual Oeste partida
  EventKey (Char 'A') Down _ _ -> moverMinhocaAtual Oeste partida
  EventKey (Char 'd') Down _ _ -> moverMinhocaAtual Este partida
  EventKey (Char 'D') Down _ _ -> moverMinhocaAtual Este partida
  
  -- Movimento com setas
  EventKey (SpecialKey KeyUp) Down _ _ -> moverMinhocaAtual Norte partida
  EventKey (SpecialKey KeyDown) Down _ _ -> moverMinhocaAtual Sul partida
  EventKey (SpecialKey KeyLeft) Down _ _ -> moverMinhocaAtual Oeste partida
  EventKey (SpecialKey KeyRight) Down _ _ -> moverMinhocaAtual Este partida
  
  -- Seleção de armas (1-5)
  EventKey (Char '1') Down _ _ -> selecionarArma Bazuca partida
  EventKey (Char '2') Down _ _ -> selecionarArma Dinamite partida
  EventKey (Char '3') Down _ _ -> selecionarArma Mina partida
  EventKey (Char '4') Down _ _ -> selecionarArma Escavadora partida
  EventKey (Char '5') Down _ _ -> selecionarArma Jetpack partida
  
  -- Espaço - Disparar (na última direção movida ou padrão)
  EventKey (SpecialKey KeySpace) Down _ _ -> dispararArmaAtual partida
  
  -- Enter - Passar turno
  EventKey (SpecialKey KeyEnter) Down _ _ -> passarTurno partida
  
  _ -> Jogando partida

--------------------------------------------------------------------------------
-- * MOVIMENTO

-- | Move a minhoca atual numa direção
moverMinhocaAtual :: Direcao -> EstadoPartida -> EstadoJogo
moverMinhocaAtual dir partida =
  let numMinhoca = jogadorAtual partida
      estadoAtual = estadoWorms partida
      novoEstado = efetuaJogada numMinhoca (Move dir) estadoAtual
  in if validaEstado novoEstado
       then Jogando (partida 
         { estadoWorms = novoEstado
         , ultimaDirecao = dir  -- ATUALIZA última direção!
         })
       else Jogando partida  -- Movimento inválido, mantém estado

--------------------------------------------------------------------------------
-- * ARMAS

-- | Arma atualmente selecionada (guardamos no estado ou usamos default)
type ArmaSelecionada = Maybe TipoArma

-- | Seleciona uma arma (verifica se tem munição)
selecionarArma :: TipoArma -> EstadoPartida -> EstadoJogo
selecionarArma arma partida =
  let numMinhoca = jogadorAtual partida
      minhocas = minhocasEstado (estadoWorms partida)
  in if numMinhoca < length minhocas
       then let minhoca = minhocas !! numMinhoca
            in if temMunicaoArma minhoca arma
                 then Jogando (partida { armaSelecionada = Just arma })  -- GUARDA arma!
                 else Jogando partida  -- Sem munição, ignora
       else Jogando partida

-- | Verifica se minhoca tem munição de uma arma
temMunicaoArma :: Minhoca -> TipoArma -> Bool
temMunicaoArma m Jetpack = jetpackMinhoca m > 0
temMunicaoArma m Escavadora = escavadoraMinhoca m > 0
temMunicaoArma m Bazuca = bazucaMinhoca m > 0
temMunicaoArma m Mina = minaMinhoca m > 0
temMunicaoArma m Dinamite = dinamiteMinhoca m > 0

-- | Dispara a arma atualmente selecionada
dispararArmaAtual :: EstadoPartida -> EstadoJogo
dispararArmaAtual partida =
  let numMinhoca = jogadorAtual partida
      arma = case armaSelecionada partida of
               Just a -> a
               Nothing -> Bazuca  -- Arma padrão se nenhuma selecionada
      dir = ultimaDirecao partida  -- USA última direção!
      estadoAtual = estadoWorms partida
      novoEstado = efetuaJogada numMinhoca (Dispara arma dir) estadoAtual
  in if validaEstado novoEstado
       then Jogando (partida 
         { estadoWorms = novoEstado
         , armaSelecionada = Nothing  -- Limpa seleção após disparar
         })
       else Jogando partida

--------------------------------------------------------------------------------
-- * TURNOS

-- | Passa o turno para a próxima minhoca viva
passarTurno :: EstadoPartida -> EstadoJogo
passarTurno partida =
  let novoTurno = turnoAtual partida + 1
      minhocas = minhocasEstado (estadoWorms partida)
      proximaMinhoca = encontrarProximaMinhocaViva (jogadorAtual partida) minhocas
  in case proximaMinhoca of
       Just numProxima -> 
         Jogando (partida 
           { turnoAtual = novoTurno
           , jogadorAtual = numProxima
           })
       Nothing -> 
         -- Sem minhocas vivas - fim de jogo
         verificarFimDeJogo partida

-- | Encontra a próxima minhoca viva após o índice dado
encontrarProximaMinhocaViva :: Int -> [Minhoca] -> Maybe Int
encontrarProximaMinhocaViva atual minhocas =
  let tentativas = [atual + 1 .. length minhocas - 1] ++ [0 .. atual]
      vivas = [ i | i <- tentativas
              , i < length minhocas
              , minhocaEstaViva (minhocas !! i)
              ]
  in case vivas of
       (i:_) -> Just i
       [] -> Nothing

-- | Verifica se uma minhoca está viva
minhocaEstaViva :: Minhoca -> Bool
minhocaEstaViva m = case vidaMinhoca m of
  Viva _ -> True
  Morta -> False

--------------------------------------------------------------------------------
-- * FIM DE JOGO

-- | Verifica se o jogo terminou e retorna estado apropriado
verificarFimDeJogo :: EstadoPartida -> EstadoJogo
verificarFimDeJogo partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      verdes = [ m | (i, m) <- zip [0..] minhocas, even i, minhocaEstaViva m ]
      azuis = [ m | (i, m) <- zip [0..] minhocas, odd i, minhocaEstaViva m ]
      pontos = turnoAtual partida * 10  -- Pontuação baseada em turnos
  in if null verdes && null azuis
       then GameOver (EstadoFinal pontos Restart)  -- Empate
       else if null verdes
              then Victory (EstadoFinal pontos Restart)  -- Azul vence
              else if null azuis
                     then Victory (EstadoFinal pontos Restart)  -- Verde vence
                     else Jogando partida  -- Jogo continua

--------------------------------------------------------------------------------
-- * EVENTOS DAS TELAS FINAIS

-- | Eventos no Game Over
eventoGameOver :: Event -> EstadoFinal -> EstadoJogo
eventoGameOver evento estadoFinal = case evento of
  EventKey (SpecialKey KeyDown) Down _ _ ->
    GameOver (estadoFinal { opcaoFinal = proximaOpcaoFinal (opcaoFinal estadoFinal) })
  
  EventKey (SpecialKey KeyUp) Down _ _ ->
    GameOver (estadoFinal { opcaoFinal = opcaoFinalAnterior (opcaoFinal estadoFinal) })
  
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    case opcaoFinal estadoFinal of
      Restart -> SelecaoModo (EstadoSelecao DoisJogadores 0.0)
      VoltarMenu -> Menu (EstadoMenu OpcaoPlay 0.0)
  
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    Menu (EstadoMenu OpcaoPlay 0.0)
  
  _ -> GameOver estadoFinal

-- | Eventos na Victory (mesma lógica do Game Over)
eventoVictory :: Event -> EstadoFinal -> EstadoJogo
eventoVictory = eventoGameOver

-- | Eventos no Tutorial
eventoTutorial :: Event -> EstadoTutorial -> EstadoJogo
eventoTutorial evento tutorial = case evento of
  EventKey (SpecialKey KeyRight) Down _ _ ->
    Tutorial (tutorial { paginaTutorial = min 2 (paginaTutorial tutorial + 1) })
  
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    Tutorial (tutorial { paginaTutorial = max 0 (paginaTutorial tutorial - 1) })
  
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    Menu (EstadoMenu OpcaoPlay 0.0)
  
  _ -> Tutorial tutorial

--------------------------------------------------------------------------------
-- * FUNÇÕES AUXILIARES

proximaOpcaoFinal :: OpcaoFinal -> OpcaoFinal
proximaOpcaoFinal Restart = VoltarMenu
proximaOpcaoFinal VoltarMenu = Restart

opcaoFinalAnterior :: OpcaoFinal -> OpcaoFinal
opcaoFinalAnterior Restart = VoltarMenu
opcaoFinalAnterior VoltarMenu = Restart