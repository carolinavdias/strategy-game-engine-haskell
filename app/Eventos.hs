{-|
Module      : Eventos
Description : Processamento de eventos durante o jogo.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Processa input do jogador durante uma partida:
movimento, seleção de armas, disparos, pausa, etc.
-}

module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Labs2025
import Tarefa2
import Tarefa1

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
  
  -- ====== JOGADOR 1 (VERDE) - WASD + 12345 + C ======
  -- Movimento WASD (sempre pode mover)
  EventKey (Char 'w') Down _ _ -> moverJogador 0 Norte partida
  EventKey (Char 'W') Down _ _ -> moverJogador 0 Norte partida
  EventKey (Char 's') Down _ _ -> moverJogador 0 Sul partida
  EventKey (Char 'S') Down _ _ -> moverJogador 0 Sul partida
  EventKey (Char 'a') Down _ _ -> moverJogador 0 Oeste partida
  EventKey (Char 'A') Down _ _ -> moverJogador 0 Oeste partida
  EventKey (Char 'd') Down _ _ -> moverJogador 0 Este partida
  EventKey (Char 'D') Down _ _ -> moverJogador 0 Este partida
  
  -- Seleção de armas Jogador 1
  EventKey (Char '1') Down _ _ -> selecionarArmaJogador 0 Bazuca partida
  EventKey (Char '2') Down _ _ -> selecionarArmaJogador 0 Dinamite partida
  EventKey (Char '3') Down _ _ -> selecionarArmaJogador 0 Mina partida
  EventKey (Char '4') Down _ _ -> selecionarArmaJogador 0 Escavadora partida
  EventKey (Char '5') Down _ _ -> selecionarArmaJogador 0 Jetpack partida
  
  -- Disparar Jogador 1 (C)
  EventKey (Char 'c') Down _ _ -> dispararJogador 0 partida
  EventKey (Char 'C') Down _ _ -> dispararJogador 0 partida
  
  -- ====== JOGADOR 2 (AZUL) - SETAS + 67890 + Ç ======
  -- Movimento com setas (sempre pode mover)
  EventKey (SpecialKey KeyUp) Down _ _ -> moverJogador 1 Norte partida
  EventKey (SpecialKey KeyDown) Down _ _ -> moverJogador 1 Sul partida
  EventKey (SpecialKey KeyLeft) Down _ _ -> moverJogador 1 Oeste partida
  EventKey (SpecialKey KeyRight) Down _ _ -> moverJogador 1 Este partida
  
  -- Seleção de armas Jogador 2 (6-0)
  EventKey (Char '6') Down _ _ -> selecionarArmaJogador 1 Bazuca partida
  EventKey (Char '7') Down _ _ -> selecionarArmaJogador 1 Dinamite partida
  EventKey (Char '8') Down _ _ -> selecionarArmaJogador 1 Mina partida
  EventKey (Char '9') Down _ _ -> selecionarArmaJogador 1 Escavadora partida
  EventKey (Char '0') Down _ _ -> selecionarArmaJogador 1 Jetpack partida
  
  -- Disparar Jogador 2 (Ç)
  EventKey (Char 'ç') Down _ _ -> dispararJogador 1 partida
  EventKey (Char 'Ç') Down _ _ -> dispararJogador 1 partida
  
  _ -> Jogando partida

--------------------------------------------------------------------------------
-- * MOVIMENTO (MODO SIMULTÂNEO)

-- | Move um jogador específico (0=verde, 1=azul) independentemente
moverJogador :: Int -> Direcao -> EstadoPartida -> EstadoJogo
moverJogador numJogador dir partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      -- Encontra índice da minhoca do jogador (par=verde, ímpar=azul)
      indicesMinhocas = if numJogador == 0
                        then [i | i <- [0..length minhocas-1], even i, minhocaEstaViva (minhocas !! i)]
                        else [i | i <- [0..length minhocas-1], odd i, minhocaEstaViva (minhocas !! i)]
      -- Só ativa animação se mover horizontal (Este/Oeste)
      novoFrame = if dir == Este || dir == Oeste then 1 else 0
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let estadoAtual = estadoWorms partida
             novoEstado = efetuaJogada numMinhoca (Move dir) estadoAtual
         in if validaEstado novoEstado
              then Jogando (partida 
                { estadoWorms = novoEstado
                , ultimaDirecaoP1 = if numJogador == 0 then dir else ultimaDirecaoP1 partida
                , ultimaDirecaoP2 = if numJogador == 1 then dir else ultimaDirecaoP2 partida
                , frameAnimacao = novoFrame  -- WALK só se Este/Oeste!
                })
              else Jogando partida
       [] -> Jogando partida  -- Sem minhocas vivas desse jogador

--------------------------------------------------------------------------------
-- * ARMAS (MODO SIMULTÂNEO)

-- | Seleciona arma para um jogador específico
selecionarArmaJogador :: Int -> TipoArma -> EstadoPartida -> EstadoJogo
selecionarArmaJogador numJogador arma partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = if numJogador == 0
                        then [i | i <- [0..length minhocas-1], even i, minhocaEstaViva (minhocas !! i)]
                        else [i | i <- [0..length minhocas-1], odd i, minhocaEstaViva (minhocas !! i)]
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let minhoca = minhocas !! numMinhoca
         in if temMunicaoArma minhoca arma
              then Jogando (partida 
                { armaSelecionadaP1 = if numJogador == 0 then Just arma else armaSelecionadaP1 partida
                , armaSelecionadaP2 = if numJogador == 1 then Just arma else armaSelecionadaP2 partida
                })
              else Jogando partida  -- Sem munição
       [] -> Jogando partida

-- | Dispara arma de um jogador específico
dispararJogador :: Int -> EstadoPartida -> EstadoJogo
dispararJogador numJogador partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = if numJogador == 0
                        then [i | i <- [0..length minhocas-1], even i, minhocaEstaViva (minhocas !! i)]
                        else [i | i <- [0..length minhocas-1], odd i, minhocaEstaViva (minhocas !! i)]
      armaAtual = if numJogador == 0 then armaSelecionadaP1 partida else armaSelecionadaP2 partida
      dirAtual = if numJogador == 0 then ultimaDirecaoP1 partida else ultimaDirecaoP2 partida
  in case (indicesMinhocas, armaAtual) of
       ((numMinhoca:_), Just arma) ->
         let estadoAtual = estadoWorms partida
             novoEstado = efetuaJogada numMinhoca (Dispara arma dirAtual) estadoAtual
         in if validaEstado novoEstado
              then Jogando (partida 
                { estadoWorms = novoEstado
                , armaSelecionadaP1 = if numJogador == 0 then Nothing else armaSelecionadaP1 partida
                , armaSelecionadaP2 = if numJogador == 1 then Nothing else armaSelecionadaP2 partida
                })
              else Jogando partida
       _ -> Jogando partida  -- Sem arma ou sem minhoca

-- | Verifica se minhoca tem munição de uma arma
temMunicaoArma :: Minhoca -> TipoArma -> Bool
temMunicaoArma m Jetpack = jetpackMinhoca m > 0
temMunicaoArma m Escavadora = escavadoraMinhoca m > 0
temMunicaoArma m Bazuca = bazucaMinhoca m > 0
temMunicaoArma m Mina = minaMinhoca m > 0
temMunicaoArma m Dinamite = dinamiteMinhoca m > 0

--------------------------------------------------------------------------------
-- * TURNOS (REMOVIDO - MODO SIMULTÂNEO NÃO USA TURNOS)

--------------------------------------------------------------------------------
-- * FIM DE JOGO

-- | Verifica se uma minhoca está viva
minhocaEstaViva :: Minhoca -> Bool
minhocaEstaViva m = case vidaMinhoca m of
  Viva _ -> True
  Morta -> False

-- | Verifica se o jogo terminou e retorna estado apropriado (SEM USAR - feito no Main)
verificarFimDeJogo :: EstadoPartida -> EstadoJogo
verificarFimDeJogo partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      verdes = [ m | (i, m) <- zip [0..] minhocas, even i, minhocaEstaViva m ]
      azuis = [ m | (i, m) <- zip [0..] minhocas, odd i, minhocaEstaViva m ]
      pontos = turnoAtual partida * 10
  in if null verdes && null azuis
       then GameOver (EstadoFinal pontos Restart)
       else if null verdes
              then Victory (EstadoFinal pontos Restart)
              else if null azuis
                     then Victory (EstadoFinal pontos Restart)
                     else Jogando partida

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