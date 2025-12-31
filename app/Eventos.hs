{-|
Module      : Eventos
Description : Processamento de eventos do jogo
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo processa todos os eventos de input do jogo, incluindo:

  * Navegação nos menus
  * Movimento das minhocas
  * Seleção e disparo de armas
  * Sistema de turnos

== Controlos

=== Jogador 1 (Verde)

  * __WASD__ — Mover (ou voar\/escavar se Jetpack\/Escavadora selecionados)
  * __1-5__ — Selecionar arma (1=Bazuca, 2=Dinamite, 3=Mina, 4=Escavadora, 5=Jetpack)
  * __C__ — Disparar arma ofensiva (Bazuca\/Dinamite\/Mina)
  * __Q__ — Abrir menu de armas

=== Jogador 2 (Azul)

  * __IJKL__ — Mover (ou voar\/escavar)
  * __6-0__ — Selecionar arma
  * __M__ — Disparar
  * __Q__ — Abrir menu de armas

=== Geral

  * __P__ — Pausar\/Resumir
  * __X__ — Voltar ao menu
  * __Space__ — Passar turno
-}
module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Labs2025
import Tarefa2
import Tarefa1
import Tempo (passarTurno)

--------------------------------------------------------------------------------
-- * Processamento Principal

-- | Processa eventos durante uma partida ativa
eventoJogo :: Event -> EstadoPartida -> EstadoJogo
eventoJogo evento partida
  | pausado partida = eventoJogoPausado evento partida
  | otherwise = eventoJogoAtivo evento partida

--------------------------------------------------------------------------------
-- * Eventos Durante Pausa

-- | Processa eventos quando o jogo está pausado
eventoJogoPausado :: Event -> EstadoPartida -> EstadoJogo
eventoJogoPausado evento partida = case evento of
  EventKey (Char 'p') Down _ _ -> Jogando (partida { pausado = False })
  EventKey (Char 'P') Down _ _ -> Jogando (partida { pausado = False })
  EventKey (Char 'x') Down _ _ -> voltarDoJogo partida
  EventKey (Char 'X') Down _ _ -> voltarDoJogo partida
  EventKey (SpecialKey KeyEsc) Down _ _ -> voltarDoJogo partida
  _ -> Jogando partida

--------------------------------------------------------------------------------
-- * Eventos Durante Jogo Ativo

-- | Processa eventos durante jogo não pausado
eventoJogoAtivo :: Event -> EstadoPartida -> EstadoJogo
eventoJogoAtivo evento partida = case evento of
  -- Pausa
  EventKey (Char 'p') Down _ _ -> Jogando (partida { pausado = True })
  EventKey (Char 'P') Down _ _ -> Jogando (partida { pausado = True })
  
  -- Voltar (X)
  EventKey (Char 'x') Down _ _ -> voltarDoJogo partida
  EventKey (Char 'X') Down _ _ -> voltarDoJogo partida
    
  -- Passar turno
  EventKey (SpecialKey KeySpace) Down _ _ -> Jogando (passarTurnoComReset partida)
  
  -- Menu de armas
  EventKey (Char 'q') Down _ _ -> toggleMenuArmas partida
  EventKey (Char 'Q') Down _ _ -> toggleMenuArmas partida
  
  -- Outros eventos para jogadores
  _ -> processarEventoJogador evento partida

-- | Passa turno e desativa modo voo
passarTurnoComReset :: EstadoPartida -> EstadoPartida
passarTurnoComReset partida = 
  let partidaSemVoo = partida { modoVooP1 = False, modoVooP2 = False }
  in passarTurno partidaSemVoo

--------------------------------------------------------------------------------
-- * Distribuição de Eventos por Jogador

-- | Decide qual jogador processa o evento baseado no turno
processarEventoJogador :: Event -> EstadoPartida -> EstadoJogo
processarEventoJogador evento partida
  -- Modo VsBot e turno do bot: ignora input
  | modoPartida partida == VsBot && jogadorAtual partida == 1 = Jogando partida
  -- Turno do jogador 1 (verde)
  | jogadorAtual partida == 0 = eventoJogador1 evento partida
  -- Turno do jogador 2 e não é bot
  | jogadorAtual partida == 1 && modoPartida partida /= VsBot = eventoJogador2 evento partida
  -- Caso contrário, ignora
  | otherwise = Jogando partida

-- | Toggle do menu de armas
toggleMenuArmas :: EstadoPartida -> EstadoJogo
toggleMenuArmas partida
  -- Não abre menu se for turno do bot
  | modoPartida partida == VsBot && jogadorAtual partida == 1 = Jogando partida
  | jogadorAtual partida == 0 = 
      Jogando (partida { menuArmasAbertoP1 = not (menuArmasAbertoP1 partida) })
  | otherwise = 
      Jogando (partida { menuArmasAbertoP2 = not (menuArmasAbertoP2 partida) })

--------------------------------------------------------------------------------
-- * Eventos do Jogador 1 (Verde)

-- | Processa eventos específicos do jogador 1
eventoJogador1 :: Event -> EstadoPartida -> EstadoJogo
eventoJogador1 evento partida = case evento of
  -- Movimento / Ação direcional
  EventKey (Char 'w') Down _ _ -> acaoDirecional 0 Norte partida
  EventKey (Char 'W') Down _ _ -> acaoDirecional 0 Norte partida
  EventKey (Char 's') Down _ _ -> acaoDirecional 0 Sul partida
  EventKey (Char 'S') Down _ _ -> acaoDirecional 0 Sul partida
  EventKey (Char 'a') Down _ _ -> acaoDirecional 0 Oeste partida
  EventKey (Char 'A') Down _ _ -> acaoDirecional 0 Oeste partida
  EventKey (Char 'd') Down _ _ -> acaoDirecional 0 Este partida
  EventKey (Char 'D') Down _ _ -> acaoDirecional 0 Este partida
  
  -- Seleção de armas
  EventKey (Char '1') Down _ _ -> selecionarArma 0 Bazuca partida
  EventKey (Char '2') Down _ _ -> selecionarArma 0 Dinamite partida
  EventKey (Char '3') Down _ _ -> selecionarArma 0 Mina partida
  EventKey (Char '4') Down _ _ -> selecionarArma 0 Escavadora partida
  EventKey (Char '5') Down _ _ -> selecionarArma 0 Jetpack partida
  
  -- Disparar arma ofensiva (Bazuca/Dinamite/Mina)
  EventKey (Char 'c') Down _ _ -> dispararArmaOfensiva 0 partida
  EventKey (Char 'C') Down _ _ -> dispararArmaOfensiva 0 partida
  
  _ -> Jogando partida

--------------------------------------------------------------------------------
-- * Eventos do Jogador 2 (Azul)

-- | Processa eventos específicos do jogador 2
eventoJogador2 :: Event -> EstadoPartida -> EstadoJogo
eventoJogador2 evento partida = case evento of
  -- Movimento / Ação direcional
  EventKey (Char 'i') Down _ _ -> acaoDirecional 1 Norte partida
  EventKey (Char 'I') Down _ _ -> acaoDirecional 1 Norte partida
  EventKey (Char 'k') Down _ _ -> acaoDirecional 1 Sul partida
  EventKey (Char 'K') Down _ _ -> acaoDirecional 1 Sul partida
  EventKey (Char 'j') Down _ _ -> acaoDirecional 1 Oeste partida
  EventKey (Char 'J') Down _ _ -> acaoDirecional 1 Oeste partida
  EventKey (Char 'l') Down _ _ -> acaoDirecional 1 Este partida
  EventKey (Char 'L') Down _ _ -> acaoDirecional 1 Este partida
  
  -- Seleção de armas
  EventKey (Char '6') Down _ _ -> selecionarArma 1 Bazuca partida
  EventKey (Char '7') Down _ _ -> selecionarArma 1 Dinamite partida
  EventKey (Char '8') Down _ _ -> selecionarArma 1 Mina partida
  EventKey (Char '9') Down _ _ -> selecionarArma 1 Escavadora partida
  EventKey (Char '0') Down _ _ -> selecionarArma 1 Jetpack partida
  
  -- Disparar arma ofensiva
  EventKey (Char 'm') Down _ _ -> dispararArmaOfensiva 1 partida
  EventKey (Char 'M') Down _ _ -> dispararArmaOfensiva 1 partida
  
  _ -> Jogando partida

--------------------------------------------------------------------------------
-- * Ação Direcional (WASD/IJKL)

-- | Processa ação direcional: move, voa (Jetpack) ou escava (Escavadora)
--
-- __Comportamento:__
--
--   * Se Jetpack selecionado → Voa na direção (gasta combustível)
--   * Se Escavadora selecionada → Escava na direção
--   * Caso contrário → Move normalmente
acaoDirecional :: Int -> Direcao -> EstadoPartida -> EstadoJogo
acaoDirecional numJogador dir partida =
  let armaAtual = if numJogador == 0 
                  then armaSelecionadaP1 partida 
                  else armaSelecionadaP2 partida
  in case armaAtual of
       -- JETPACK: Voa na direção!
       Just Jetpack -> usarJetpack numJogador dir partida
       
       -- ESCAVADORA: Escava na direção!
       Just Escavadora -> usarEscavadora numJogador dir partida
       
       -- Outra arma ou nenhuma: Move normalmente
       _ -> moverJogador numJogador dir partida

--------------------------------------------------------------------------------
-- * Jetpack (Modo Voo)

-- | Usa o Jetpack para voar numa direção
--
-- O Jetpack permite voar em qualquer direção, ignorando a gravidade.
-- Cada uso gasta 1 de combustível.
--
-- __Nota:__ Voar com Jetpack NÃO atualiza a direção de disparo!
-- Assim a Bazuca dispara na direção que andaste, não na que voaste.
usarJetpack :: Int -> Direcao -> EstadoPartida -> EstadoJogo
usarJetpack numJogador dir partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = obterMinhocasDoJogador numJogador minhocas
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let minhoca = minhocas !! numMinhoca
         in if jetpackMinhoca minhoca > 0
            then -- Usa Jetpack para voar
                 let estadoAtual = estadoWorms partida
                     novoEstado = efetuaJogada numMinhoca (Dispara Jetpack dir) estadoAtual
                 in if validaEstado novoEstado
                    then Jogando (partida 
                      { estadoWorms = novoEstado
                      -- NÃO atualiza ultimaDirecao! Jetpack é para voar, não para apontar
                      , modoVooP1 = if numJogador == 0 then True else modoVooP1 partida
                      , modoVooP2 = if numJogador == 1 then True else modoVooP2 partida
                      , frameCounter = 10
                      })
                    else Jogando partida
            else -- Sem combustível: move normalmente
                 moverJogador numJogador dir partida
       [] -> Jogando partida

--------------------------------------------------------------------------------
-- * Escavadora

-- | Usa a Escavadora para escavar numa direção
--
-- A Escavadora destrói blocos de Terra e move a minhoca para essa posição.
--
-- __Nota:__ Escavar NÃO atualiza a direção de disparo!
usarEscavadora :: Int -> Direcao -> EstadoPartida -> EstadoJogo
usarEscavadora numJogador dir partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = obterMinhocasDoJogador numJogador minhocas
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let minhoca = minhocas !! numMinhoca
         in if escavadoraMinhoca minhoca > 0
            then -- Usa Escavadora
                 let estadoAtual = estadoWorms partida
                     novoEstado = efetuaJogada numMinhoca (Dispara Escavadora dir) estadoAtual
                 in if validaEstado novoEstado
                    then Jogando (partida 
                      { estadoWorms = novoEstado
                      -- NÃO atualiza ultimaDirecao! Escavar é para cavar, não para apontar
                      , frameCounter = 10
                      })
                    else Jogando partida
            else -- Sem escavadora: move normalmente
                 moverJogador numJogador dir partida
       [] -> Jogando partida

--------------------------------------------------------------------------------
-- * Movimento Normal

-- | Move a minhoca numa direção (movimento normal, sem armas)
moverJogador :: Int -> Direcao -> EstadoPartida -> EstadoJogo
moverJogador numJogador dir partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = obterMinhocasDoJogador numJogador minhocas
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let estadoAtual = estadoWorms partida
             novoEstado = efetuaJogada numMinhoca (Move dir) estadoAtual
         in if validaEstado novoEstado
              then Jogando (partida 
                { estadoWorms = novoEstado
                , ultimaDirecaoP1 = if numJogador == 0 then dir else ultimaDirecaoP1 partida
                , ultimaDirecaoP2 = if numJogador == 1 then dir else ultimaDirecaoP2 partida
                , frameCounter = 15
                })
              else Jogando partida
       [] -> Jogando partida

--------------------------------------------------------------------------------
-- * Seleção de Armas

-- | Seleciona uma arma para o jogador
--
-- __Nota:__ Ao trocar de arma, desativa o modo voo do Jetpack.
selecionarArma :: Int -> TipoArma -> EstadoPartida -> EstadoJogo
selecionarArma numJogador arma partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = obterMinhocasDoJogador numJogador minhocas
  in case indicesMinhocas of
       (numMinhoca:_) ->
         let minhoca = minhocas !! numMinhoca
         in if temMunicaoArma minhoca arma
              then Jogando (partida 
                { armaSelecionadaP1 = if numJogador == 0 then Just arma else armaSelecionadaP1 partida
                , armaSelecionadaP2 = if numJogador == 1 then Just arma else armaSelecionadaP2 partida
                -- Desativa modo voo se trocar de arma
                , modoVooP1 = if numJogador == 0 && arma /= Jetpack then False else modoVooP1 partida
                , modoVooP2 = if numJogador == 1 && arma /= Jetpack then False else modoVooP2 partida
                })
              else Jogando partida
       [] -> Jogando partida

--------------------------------------------------------------------------------
-- * Disparo de Armas Ofensivas

-- | Dispara uma arma ofensiva (Bazuca/Dinamite/Mina)
--
-- Apenas dispara se a arma selecionada for ofensiva.
-- Jetpack e Escavadora são usados com WASD, não com C.
dispararArmaOfensiva :: Int -> EstadoPartida -> EstadoJogo
dispararArmaOfensiva numJogador partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      indicesMinhocas = obterMinhocasDoJogador numJogador minhocas
      armaAtual = if numJogador == 0 then armaSelecionadaP1 partida else armaSelecionadaP2 partida
      dirAtual = if numJogador == 0 then ultimaDirecaoP1 partida else ultimaDirecaoP2 partida
  in case (indicesMinhocas, armaAtual) of
       ((numMinhoca:_), Just arma) ->
         -- Apenas dispara armas ofensivas (Bazuca, Dinamite, Mina)
         if arma `elem` [Bazuca, Dinamite, Mina]
         then let estadoAtual = estadoWorms partida
                  novoEstado = efetuaJogada numMinhoca (Dispara arma dirAtual) estadoAtual
              in if validaEstado novoEstado
                 then -- Armas ofensivas passam turno
                      Jogando (passarTurnoComReset (partida { estadoWorms = novoEstado }))
                 else Jogando partida
         else -- Jetpack/Escavadora não disparam com C
              Jogando partida
       _ -> Jogando partida

--------------------------------------------------------------------------------
-- * Funções Auxiliares

-- | Obtém os índices das minhocas vivas de um jogador
obterMinhocasDoJogador :: Int -> [Minhoca] -> [Int]
obterMinhocasDoJogador numJogador minhocas =
  if numJogador == 0
  then [i | i <- [0..length minhocas-1], even i, minhocaViva (minhocas !! i)]
  else [i | i <- [0..length minhocas-1], odd i, minhocaViva (minhocas !! i)]

-- | Verifica se a minhoca tem munição de uma arma
temMunicaoArma :: Minhoca -> TipoArma -> Bool
temMunicaoArma m Jetpack    = jetpackMinhoca m > 0
temMunicaoArma m Escavadora = escavadoraMinhoca m > 0
temMunicaoArma m Bazuca     = bazucaMinhoca m > 0
temMunicaoArma m Mina       = minaMinhoca m > 0
temMunicaoArma m Dinamite   = dinamiteMinhoca m > 0

-- | Verifica se a minhoca está viva (vida >= 1)
minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
  Viva hp -> hp >= 1  -- CORRIGIDO: vida tem que ser >= 1!
  Morta -> False

--------------------------------------------------------------------------------
-- * Voltar do Jogo

-- | Volta do jogo para o ecrã apropriado
-- No modo Treino: volta para seleção de mapa
-- Outros modos: volta para seleção de modo
voltarDoJogo :: EstadoPartida -> EstadoJogo
voltarDoJogo partida
  | modoPartida partida == Treino = 
      SelecaoMapaTreino (EstadoSelecaoMapa (mapaAtualIdx partida) 0.0)
  | otherwise = 
      SelecaoModo (EstadoSelecao (modoPartida partida) 0.0)
      
--------------------------------------------------------------------------------
-- * Eventos de Telas Finais

-- | Processa eventos na tela de vitória
eventoVictory :: Event -> EstadoFinal -> EstadoJogo
eventoVictory (EventKey (Char 'r') Down _ _) _ = criarNovaPartida
eventoVictory (EventKey (Char 'R') Down m p) estado = eventoVictory (EventKey (Char 'r') Down m p) estado
eventoVictory (EventKey (Char 'x') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoVictory (EventKey (Char 'X') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoVictory (EventKey (SpecialKey KeyUp) Down _ _) estado = Victory (estado { opcaoFinal = Restart })
eventoVictory (EventKey (SpecialKey KeyDown) Down _ _) estado = Victory (estado { opcaoFinal = VoltarMenu })
eventoVictory (EventKey (SpecialKey KeyEnter) Down _ _) estado =
  case opcaoFinal estado of
    Restart -> criarNovaPartida
    VoltarMenu -> Menu (EstadoMenu OpcaoPlay 0.0)
eventoVictory _ estado = Victory estado

-- | Cria uma nova partida com o mapa padrão
criarNovaPartida :: EstadoJogo
criarNovaPartida = Jogando (criarPartida DoisJogadores estadoInicialWorms)
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
    
    -- Munições: Jetpack=50, Escavadora=30, Bazuca=15, Mina=10, Dinamite=10
    minhocasExemplo = 
      [ Minhoca (Just (8, 6)) (Viva 100) 50 30 15 10 10
      , Minhoca (Just (8, 28)) (Viva 100) 50 30 15 10 10
      ]
    
    estadoInicialWorms = Estado mapaExemplo barrisExemplo minhocasExemplo

-- | Processa eventos na tela de game over
eventoGameOver :: Event -> EstadoFinal -> EstadoJogo
eventoGameOver ev estado = eventoVictory ev estado

-- | Processa eventos no tutorial
eventoTutorial :: Event -> EstadoTutorial -> EstadoJogo
eventoTutorial (EventKey (Char 'x') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoTutorial (EventKey (Char 'X') Down _ _) _ = Menu (EstadoMenu OpcaoPlay 0.0)
eventoTutorial _ tut = Tutorial tut