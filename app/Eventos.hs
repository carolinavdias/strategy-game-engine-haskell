{-|
Module      : Eventos
Description : Processamento de eventos do jogo
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
-}

module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Labs2025
import Tarefa2
import Tarefa1
import Tempo (passarTurno)

-- Processamento de eventos durante jogo ativo
eventoJogo :: Event -> EstadoPartida -> EstadoJogo
eventoJogo evento partida
  | pausado partida = eventoJogoPausado evento partida
  | otherwise = eventoJogoAtivo evento partida

-- Eventos durante pausa
eventoJogoPausado :: Event -> EstadoPartida -> EstadoJogo
eventoJogoPausado evento partida = case evento of
  EventKey (Char 'p') Down _ _ -> Jogando (partida { pausado = False })
  EventKey (Char 'P') Down _ _ -> Jogando (partida { pausado = False })
  EventKey (Char 'x') Down _ _ -> SelecaoModo (EstadoSelecao DoisJogadores 0.0)
  EventKey (Char 'X') Down _ _ -> SelecaoModo (EstadoSelecao DoisJogadores 0.0)
  EventKey (SpecialKey KeyEsc) Down _ _ -> SelecaoModo (EstadoSelecao DoisJogadores 0.0)
  _ -> Jogando partida

-- Eventos durante jogo ativo
eventoJogoAtivo :: Event -> EstadoPartida -> EstadoJogo
eventoJogoAtivo evento partida = case evento of
  EventKey (Char 'p') Down _ _ -> Jogando (partida { pausado = True })
  EventKey (Char 'P') Down _ _ -> Jogando (partida { pausado = True })
  EventKey (Char 'x') Down _ _ -> SelecaoModo (EstadoSelecao DoisJogadores 0.0)
  EventKey (Char 'X') Down _ _ -> SelecaoModo (EstadoSelecao DoisJogadores 0.0)
  EventKey (SpecialKey KeySpace) Down _ _ -> Jogando (passarTurno partida)
  EventKey (Char 'q') Down _ _ -> toggleMenuArmas partida
  EventKey (Char 'Q') Down _ _ -> toggleMenuArmas partida
  _ -> if jogadorAtual partida == 0
       then eventoJogador1 evento partida
       else eventoJogador2 evento partida

toggleMenuArmas :: EstadoPartida -> EstadoJogo
toggleMenuArmas partida =
  if jogadorAtual partida == 0
  then Jogando (partida { menuArmasAbertoP1 = not (menuArmasAbertoP1 partida) })
  else Jogando (partida { menuArmasAbertoP2 = not (menuArmasAbertoP2 partida) })

-- Eventos específicos do Jogador 1
eventoJogador1 :: Event -> EstadoPartida -> EstadoJogo
eventoJogador1 evento partida = case evento of
  EventKey (Char 'w') Down _ _ -> moverJogador 0 Norte partida
  EventKey (Char 'W') Down _ _ -> moverJogador 0 Norte partida
  EventKey (Char 's') Down _ _ -> moverJogador 0 Sul partida
  EventKey (Char 'S') Down _ _ -> moverJogador 0 Sul partida
  EventKey (Char 'a') Down _ _ -> moverJogador 0 Oeste partida
  EventKey (Char 'A') Down _ _ -> moverJogador 0 Oeste partida
  EventKey (Char 'd') Down _ _ -> moverJogador 0 Este partida
  EventKey (Char 'D') Down _ _ -> moverJogador 0 Este partida
  EventKey (Char '1') Down _ _ -> selecionarArma 0 Bazuca partida
  EventKey (Char '2') Down _ _ -> selecionarArma 0 Dinamite partida
  EventKey (Char '3') Down _ _ -> selecionarArma 0 Mina partida
  EventKey (Char '4') Down _ _ -> selecionarArma 0 Escavadora partida
  EventKey (Char '5') Down _ _ -> selecionarArma 0 Jetpack partida
  EventKey (Char 'c') Down _ _ -> dispararJogador 0 partida
  EventKey (Char 'C') Down _ _ -> dispararJogador 0 partida
  _ -> Jogando partida

-- Eventos específicos do Jogador 2
eventoJogador2 :: Event -> EstadoPartida -> EstadoJogo
eventoJogador2 evento partida = case evento of
  EventKey (Char 'i') Down _ _ -> moverJogador 1 Norte partida
  EventKey (Char 'I') Down _ _ -> moverJogador 1 Norte partida
  EventKey (Char 'k') Down _ _ -> moverJogador 1 Sul partida
  EventKey (Char 'K') Down _ _ -> moverJogador 1 Sul partida
  EventKey (Char 'j') Down _ _ -> moverJogador 1 Oeste partida
  EventKey (Char 'J') Down _ _ -> moverJogador 1 Oeste partida
  EventKey (Char 'l') Down _ _ -> moverJogador 1 Este partida
  EventKey (Char 'L') Down _ _ -> moverJogador 1 Este partida
  EventKey (Char '6') Down _ _ -> selecionarArma 1 Bazuca partida
  EventKey (Char '7') Down _ _ -> selecionarArma 1 Dinamite partida
  EventKey (Char '8') Down _ _ -> selecionarArma 1 Mina partida
  EventKey (Char '9') Down _ _ -> selecionarArma 1 Escavadora partida
  EventKey (Char '0') Down _ _ -> selecionarArma 1 Jetpack partida
  EventKey (Char 'm') Down _ _ -> dispararJogador 1 partida
  EventKey (Char 'M') Down _ _ -> dispararJogador 1 partida
  _ -> Jogando partida

-- Movimentação de jogador
moverJogador :: Int -> Direcao -> EstadoPartida -> EstadoJogo
moverJogador numJogador dir partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = if numJogador == 0
                        then [i | i <- [0..length minhocas-1], even i, minhocaViva (minhocas !! i)]
                        else [i | i <- [0..length minhocas-1], odd i, minhocaViva (minhocas !! i)]
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let estadoAtual = estadoWorms partida
             novoEstado = efetuaJogada numMinhoca (Move dir) estadoAtual
         in if validaEstado novoEstado
              then Jogando (partida 
                { estadoWorms = novoEstado
                , ultimaDirecaoP1 = if numJogador == 0 then dir else ultimaDirecaoP1 partida
                , ultimaDirecaoP2 = if numJogador == 1 then dir else ultimaDirecaoP2 partida
                , frameCounter = 20
                })
              else Jogando partida
       [] -> Jogando partida

-- Seleção de arma
selecionarArma :: Int -> TipoArma -> EstadoPartida -> EstadoJogo
selecionarArma numJogador arma partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = if numJogador == 0
                        then [i | i <- [0..length minhocas-1], even i, minhocaViva (minhocas !! i)]
                        else [i | i <- [0..length minhocas-1], odd i, minhocaViva (minhocas !! i)]
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let minhoca = minhocas !! numMinhoca
         in if temMunicaoArma minhoca arma
              then Jogando (partida 
                { armaSelecionadaP1 = if numJogador == 0 then Just arma else armaSelecionadaP1 partida
                , armaSelecionadaP2 = if numJogador == 1 then Just arma else armaSelecionadaP2 partida
                })
              else Jogando partida
       [] -> Jogando partida

-- Disparo de arma
dispararJogador :: Int -> EstadoPartida -> EstadoJogo
dispararJogador numJogador partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = if numJogador == 0
                        then [i | i <- [0..length minhocas-1], even i, minhocaViva (minhocas !! i)]
                        else [i | i <- [0..length minhocas-1], odd i, minhocaViva (minhocas !! i)]
      armaAtual = if numJogador == 0 then armaSelecionadaP1 partida else armaSelecionadaP2 partida
      dirAtual = if numJogador == 0 then ultimaDirecaoP1 partida else ultimaDirecaoP2 partida
  in case (indicesMinhocas, armaAtual) of
       ((numMinhoca:_), Just arma) ->
         let estadoAtual = estadoWorms partida
             novoEstado = efetuaJogada numMinhoca (Dispara arma dirAtual) estadoAtual
             passaTurno = arma `notElem` [Escavadora, Jetpack]
         in if validaEstado novoEstado
              then if passaTurno
                   then Jogando (passarTurno (partida { estadoWorms = novoEstado }))
                   else Jogando (partida { estadoWorms = novoEstado })
              else Jogando partida
       _ -> Jogando partida

temMunicaoArma :: Minhoca -> TipoArma -> Bool
temMunicaoArma m Jetpack = jetpackMinhoca m > 0
temMunicaoArma m Escavadora = escavadoraMinhoca m > 0
temMunicaoArma m Bazuca = bazucaMinhoca m > 0
temMunicaoArma m Mina = minaMinhoca m > 0
temMunicaoArma m Dinamite = dinamiteMinhoca m > 0

minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
  Viva _ -> True
  Morta -> False

-- Eventos nas telas de vitória e game over
eventoVictory :: Event -> EstadoFinal -> EstadoJogo
eventoVictory (EventKey (Char 'r') Down _ _) _ = 
  Jogando (criarPartida DoisJogadores estadoInicialWorms)
  where
    mapaExemplo = 
      [ [Pedra | _ <- [1..34]] ] ++
      [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++
      [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++
      [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]
    
    barrisExemplo = 
      [ Barril (9, 8) False
      , Barril (9, 13) False
      , Barril (9, 17) False
      , Barril (9, 24) False
      ]
    
    minhocasExemplo = 
      [ Minhoca (Just (8, 6)) (Viva 100) 2 3 5 2 3
      , Minhoca (Just (8, 28)) (Viva 100) 2 3 5 2 3
      ]
    
    estadoInicialWorms = Estado mapaExemplo barrisExemplo minhocasExemplo

eventoVictory (EventKey (Char 'R') Down m p) estado = eventoVictory (EventKey (Char 'r') Down m p) estado
eventoVictory (EventKey (Char 'x') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoVictory (EventKey (Char 'X') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)

eventoVictory (EventKey (SpecialKey KeyUp) Down _ _) estado = Victory (estado { opcaoFinal = Restart })
eventoVictory (EventKey (SpecialKey KeyDown) Down _ _) estado = Victory (estado { opcaoFinal = VoltarMenu })

eventoVictory (EventKey (SpecialKey KeyEnter) Down _ _) estado =
  case opcaoFinal estado of
    Restart -> Jogando (criarPartida DoisJogadores estadoInicialWorms)
      where
        mapaExemplo = 
          [ [Pedra | _ <- [1..34]] ] ++
          [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++
          [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++
          [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]
        
        barrisExemplo = 
          [ Barril (9, 8) False
          , Barril (9, 13) False
          , Barril (9, 17) False
          , Barril (9, 24) False
          ]
        
        minhocasExemplo = 
          [ Minhoca (Just (8, 6)) (Viva 100) 2 3 5 2 3
          , Minhoca (Just (8, 28)) (Viva 100) 2 3 5 2 3
          ]
        
        estadoInicialWorms = Estado mapaExemplo barrisExemplo minhocasExemplo
    VoltarMenu -> Menu (EstadoMenu OpcaoPlay 0.0)

eventoVictory _ estado = Victory estado

eventoGameOver :: Event -> EstadoFinal -> EstadoJogo
eventoGameOver ev estado = eventoVictory ev estado

eventoTutorial :: Event -> EstadoTutorial -> EstadoJogo
eventoTutorial (EventKey (Char 'x') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoTutorial (EventKey (Char 'X') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoTutorial _ tut = Tutorial tut