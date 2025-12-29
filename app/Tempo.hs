{-|
Module      : Tempo
Description : Atualização temporal e sistema de turnos
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
-}

module Tempo where

import EstadoJogo
import Tarefa3
import Tarefa4 (jogadaTatica)
import Tarefa2 (efetuaJogada)
import Tarefa1 (validaEstado)
import Labs2025

type Segundos = Float


-- Atualização completa da partida a cada frame
atualizarPartidaCompleta :: Segundos -> EstadoPartida -> EstadoJogo
atualizarPartidaCompleta dt partida
  | pausado partida = Jogando partida
  | otherwise = 
      let 
          -- Se for modo VsBot e turno do bot (jogador 1 = azul) E frameCounter a 0, executa jogada
          partidaComBot = if modoPartida partida == VsBot && jogadorAtual partida == 1 && frameCounter partida == 0
                          then executarJogadaBot partida
                          else partida
          
          partidaComTimer = atualizarTimer dt partida
          novoFrame = if frameCounter partidaComTimer > 0
                     then frameCounter partidaComTimer - 1
                     else 0
          partidaComFrame = partidaComTimer { frameCounter = novoFrame }
          partidaComAnimacoes = partidaComFrame 
            { animacoes = atualizarAnimacoes dt (animacoes partidaComFrame) }
          partidaComFisica = avancarFisica dt partidaComAnimacoes
      in verificarCondicaoVitoria partidaComFisica

-- Executa jogada inteligente do bot
executarJogadaBot :: EstadoPartida -> EstadoPartida
executarJogadaBot partida =
  let estadoAtual = estadoWorms partida
      -- Calcula ticks baseado no tempo restante para variar comportamento
      ticksAtual = floor (tempoRestante partida * 10)
      
      -- Obtém jogada inteligente do bot
      (numMinhoca, jogada) = jogadaTatica ticksAtual estadoAtual
      
      -- Verifica se a minhoca é azul (ímpar) e está viva
      minhocas = minhocasEstado estadoAtual
      minhocaValida = numMinhoca < length minhocas && 
                      odd numMinhoca && 
                      minhocaViva (minhocas !! numMinhoca)
      
  in if minhocaValida
     then let novoEstado = efetuaJogada numMinhoca jogada estadoAtual
          in if validaEstado novoEstado
             then case jogada of
                    -- Armas ofensivas passam turno automaticamente
                    Dispara Bazuca _ -> trocarTurno (partida { estadoWorms = novoEstado })
                    Dispara Dinamite _ -> trocarTurno (partida { estadoWorms = novoEstado })
                    Dispara Mina _ -> trocarTurno (partida { estadoWorms = novoEstado })
                    -- Movimentos e ferramentas não passam turno
                    _ -> partida { estadoWorms = novoEstado, frameCounter = 20 }
             else partida
     else partida


-- Atualiza o timer de turno
atualizarTimer :: Segundos -> EstadoPartida -> EstadoPartida
atualizarTimer dt partida =
  let novoTempo = tempoRestante partida - dt
  in if novoTempo <= 0
     then trocarTurno partida
     else partida { tempoRestante = novoTempo }

-- Troca o turno entre jogadores
trocarTurno :: EstadoPartida -> EstadoPartida
trocarTurno partida = partida
  { jogadorAtual = if jogadorAtual partida == 0 then 1 else 0
  , tempoRestante = tempoTotal partida
  , turnoAtual = turnoAtual partida + 1
  , menuArmasAbertoP1 = False
  , menuArmasAbertoP2 = False
  , armaSelecionadaP1 = Nothing
  , armaSelecionadaP2 = Nothing
  , frameCounter = 0
  }

-- Passa o turno manualmente
passarTurno :: EstadoPartida -> EstadoPartida
passarTurno = trocarTurno

-- Avança a física do jogo
avancarFisica :: Segundos -> EstadoPartida -> EstadoPartida
avancarFisica _ partida =
  let estadoAtual = estadoWorms partida
      novoEstado = avancaEstado estadoAtual
      novasAnimacoes = detectarExplosoes estadoAtual novoEstado
  in partida 
       { estadoWorms = novoEstado
       , animacoes = animacoes partida ++ novasAnimacoes
       }

-- Detecta explosões e danos entre estados
detectarExplosoes :: Estado -> Estado -> [AnimacaoAtiva]
detectarExplosoes estadoAntes estadoDepois =
  let objetosAntes = objetosEstado estadoAntes
      objetosDepois = objetosEstado estadoDepois
      objetosRemovidos = [ obj | obj <- objetosAntes, obj `notElem` objetosDepois ]
      explosoes = [ AnimExplosao (posicaoObjeto obj) 0.0 0 
                  | obj <- objetosRemovidos, eObjetoExplosivo obj ]
      minhocasAntes = minhocasEstado estadoAntes
      minhocasDepois = minhocasEstado estadoDepois
      danos = detectarDanosMinhocas minhocasAntes minhocasDepois
  in explosoes ++ danos

-- Verifica se objeto é explosivo
eObjetoExplosivo :: Objeto -> Bool
eObjetoExplosivo (Barril _ True) = True
eObjetoExplosivo (Disparo _ _ Bazuca _ _) = True
eObjetoExplosivo (Disparo _ _ Mina (Just 0) _) = True
eObjetoExplosivo (Disparo _ _ Dinamite (Just 0) _) = True
eObjetoExplosivo _ = False

-- Obtém posição de um objeto
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril pos _) = pos
posicaoObjeto (Disparo pos _ _ _ _) = pos

-- Detecta danos nas minhocas
detectarDanosMinhocas :: [Minhoca] -> [Minhoca] -> [AnimacaoAtiva]
detectarDanosMinhocas antes depois =
  [ AnimDano pos dano 1.0
  | (m1, m2) <- zip antes depois
  , Just pos <- [posicaoMinhoca m2]
  , let dano = calcularDano (vidaMinhoca m1) (vidaMinhoca m2)
  , dano > 0
  ]

-- Calcula dano entre dois estados de vida
calcularDano :: VidaMinhoca -> VidaMinhoca -> Int
calcularDano (Viva v1) (Viva v2) = max 0 (v1 - v2)
calcularDano (Viva v1) Morta = v1
calcularDano _ _ = 0

-- Atualiza todas as animações ativas
atualizarAnimacoes :: Segundos -> [AnimacaoAtiva] -> [AnimacaoAtiva]
atualizarAnimacoes dt = filter animacaoAtiva . map (atualizarAnimacao dt)
  where
    atualizarAnimacao :: Segundos -> AnimacaoAtiva -> AnimacaoAtiva
    atualizarAnimacao dtLocal (AnimExplosao pos tempo frame) =
      let novoTempo = tempo + dtLocal
          novoFrame = if novoTempo > 0.15 * fromIntegral (frame + 1)
                      then frame + 1 else frame
      in AnimExplosao pos novoTempo novoFrame
    atualizarAnimacao dtLocal (AnimDano pos dano tempo) =
      AnimDano pos dano (tempo - dtLocal)
    atualizarAnimacao dtLocal (AnimMovimento num de para prog) =
      AnimMovimento num de para (min 1.0 (prog + dtLocal * 3))
    
    animacaoAtiva (AnimExplosao _ _ frame) = frame < 3
    animacaoAtiva (AnimDano _ _ tempo) = tempo > 0
    animacaoAtiva (AnimMovimento _ _ _ prog) = prog < 1.0

-- Verifica condição de vitória
verificarCondicaoVitoria :: EstadoPartida -> EstadoJogo
verificarCondicaoVitoria partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      verdesVivas = [m | (i, m) <- zip [0..] minhocas, even i, minhocaViva m]
      azuisVivas = [m | (i, m) <- zip [0..] minhocas, odd i, minhocaViva m]
  in if null verdesVivas
     then Victory (EstadoFinal 0 (Just VenceuAzul) VoltarMenu)
     else if null azuisVivas
          then Victory (EstadoFinal 0 (Just VenceuVerde) VoltarMenu)
          else Jogando partida

-- Verifica se minhoca está viva
minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
  Viva _ -> True
  Morta -> False