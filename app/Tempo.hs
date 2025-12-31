{-|
Module      : Tempo
Description : Atualização temporal e sistema de turnos
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo gere a evolução temporal do jogo, incluindo:

  * Atualização do timer de turno
  * Execução de jogadas do bot
  * Aplicação de física (gravidade, explosões)
  * Verificação de condições de vitória

== Sistema de Turnos

Cada jogador tem 30 segundos por turno. O turno passa automaticamente quando:

  * O tempo acaba
  * O jogador dispara uma arma ofensiva (Bazuca\/Dinamite\/Mina)
  * O jogador passa manualmente (Space)

== Modo Voo (Jetpack)

Quando o Jetpack está ativo, a gravidade não afeta a minhoca.
Isto permite voar livremente até trocar de arma ou acabar o combustível.
-}
module Tempo where

import EstadoJogo
import Tarefa3
import Tarefa4 (jogadaTatica)
import Tarefa2 (efetuaJogada)
import Tarefa1 (validaEstado)
import Mapas (selecionarMapa, criarEstadoTreino)
import Labs2025

type Segundos = Float

--------------------------------------------------------------------------------
-- * Atualização Principal

-- | Atualiza a partida completa a cada frame
atualizarPartidaCompleta :: Segundos -> EstadoPartida -> EstadoJogo
atualizarPartidaCompleta dt partida
  | pausado partida = Jogando partida
  | otherwise = 
      let -- 1. Se modo VsBot e turno do bot, executa jogada
          deveJogarBot = modoPartida partida == VsBot && 
                        jogadorAtual partida == 1 && 
                        tempoRestante partida < 29.0 &&
                        frameCounter partida == 0
          
          partidaComBot = if deveJogarBot
                          then executarJogadaBot partida
                          else partida
          
          -- 2. Atualiza timer
          partidaComTimer = atualizarTimer dt partidaComBot
          
          -- 3. Atualiza frame counter
          novoFrame = max 0 (frameCounter partidaComTimer - 1)
          partidaComFrame = partidaComTimer { frameCounter = novoFrame }
          
          -- 4. Atualiza animações
          partidaComAnimacoes = partidaComFrame 
            { animacoes = atualizarAnimacoes dt (animacoes partidaComFrame) }
          
          -- 5. Avança física (com verificação de modo voo!)
          partidaComFisica = avancarFisicaComModoVoo dt partidaComAnimacoes
          
      in verificarCondicaoVitoria partidaComFisica

--------------------------------------------------------------------------------
-- * Bot (Modo VsBot)

-- | Executa jogada do bot - faz UMA jogada e passa turno
executarJogadaBot :: EstadoPartida -> EstadoPartida
executarJogadaBot partida =
  let estadoAtual = estadoWorms partida
      minhocas = minhocasEstado estadoAtual
      
      -- Encontra minhoca azul viva (índice ímpar)
      minhocasAzuis = [(i, m) | (i, m) <- zip [0..] minhocas, odd i, minhocaViva m]
      
  in case minhocasAzuis of
       [] -> trocarTurno partida  -- Sem minhocas azuis vivas, passa turno
       ((numMinhoca, _):_) ->
         let -- Usa turnoAtual como seed para variar jogadas
             turno = turnoAtual partida
             (_, jogada) = jogadaTatica turno estadoAtual
             novoEstado = efetuaJogada numMinhoca jogada estadoAtual
         in if validaEstado novoEstado
            then trocarTurno (partida { estadoWorms = novoEstado })
            else trocarTurno partida  -- Jogada inválida, passa turno

--------------------------------------------------------------------------------
-- * Timer e Turnos

-- | Atualiza o timer de turno
-- MODIFICADO: No modo Treino, não decrementa o timer
atualizarTimer :: Segundos -> EstadoPartida -> EstadoPartida
atualizarTimer dt partida
  | modoPartida partida == Treino = partida  -- Treino: sem timer!
  | otherwise = 
      let novoTempo = tempoRestante partida - dt
      in if novoTempo <= 0
         then trocarTurno partida
         else partida { tempoRestante = novoTempo }

-- | Troca o turno entre jogadores
trocarTurno :: EstadoPartida -> EstadoPartida
trocarTurno partida = 
  let novoJogador = if jogadorAtual partida == 0 then 1 else 0
      minhocas = minhocasEstado (estadoWorms partida)
      
      -- Verifica se o próximo jogador tem minhocas vivas
      temMinhocasProximoJogador = 
        if novoJogador == 0
        then any (\(i, m) -> even i && minhocaViva m) (zip [0..] minhocas)
        else any (\(i, m) -> odd i && minhocaViva m) (zip [0..] minhocas)
      
      -- Se próximo jogador não tem minhocas, mantém o atual
      jogadorFinal = if temMinhocasProximoJogador then novoJogador else jogadorAtual partida
      
  in partida
       { jogadorAtual = jogadorFinal
       , tempoRestante = tempoTotal partida
       , turnoAtual = turnoAtual partida + 1
       , menuArmasAbertoP1 = False
       , menuArmasAbertoP2 = False
       , armaSelecionadaP1 = Nothing
       , armaSelecionadaP2 = Nothing
       -- Desativa modo voo ao trocar turno
       , modoVooP1 = False
       , modoVooP2 = False
       , frameCounter = 0
       }

-- | Passa o turno manualmente
passarTurno :: EstadoPartida -> EstadoPartida
passarTurno = trocarTurno

--------------------------------------------------------------------------------
-- * Física com Modo Voo

-- | Avança a física do jogo, respeitando o modo voo do Jetpack
--
-- Quando uma minhoca está em modo voo:
--
--   * A gravidade __não__ a afeta
--   * Pode mover-se livremente no ar
avancarFisicaComModoVoo :: Segundos -> EstadoPartida -> EstadoPartida
avancarFisicaComModoVoo _ partida =
  let estadoAtual = estadoWorms partida
      
      -- Aplica física normal (Tarefa3)
      estadoComFisica = avancaEstado estadoAtual
      
      -- Se alguma minhoca está em modo voo, restaura a sua posição (não cai)
      minhocasOriginais = minhocasEstado estadoAtual
      minhocasComFisica = minhocasEstado estadoComFisica
      
      -- Corrige posições das minhocas em modo voo
      minhocasCorrigidas = zipWith3 corrigirModoVoo 
                                    [0..] 
                                    minhocasOriginais 
                                    minhocasComFisica
      
      -- NOVO: Garante que minhocas com HP <= 0 ficam Mortas
      minhocasFinais = map garantirMorteSeVida0 minhocasCorrigidas
      
      novoEstado = estadoComFisica { minhocasEstado = minhocasFinais }
      
      novasAnimacoes = detectarExplosoes estadoAtual novoEstado
      
  in partida 
       { estadoWorms = novoEstado
       , animacoes = animacoes partida ++ novasAnimacoes
       }
  where
    -- Corrige posição se minhoca está em modo voo
    corrigirModoVoo :: Int -> Minhoca -> Minhoca -> Minhoca
    corrigirModoVoo idx original comFisica =
      let emModoVoo = (even idx && modoVooP1 partida) || 
                      (odd idx && modoVooP2 partida)
      in if emModoVoo && minhocaViva original
         then -- Mantém posição original (não cai)
              comFisica { posicaoMinhoca = posicaoMinhoca original }
         else comFisica
    
    -- NOVO: Se minhoca tem Viva 0 ou menos, marca como Morta
    garantirMorteSeVida0 :: Minhoca -> Minhoca
    garantirMorteSeVida0 m = case vidaMinhoca m of
      Viva hp | hp <= 0 -> m { vidaMinhoca = Morta }
      _ -> m

--------------------------------------------------------------------------------
-- * Deteção de Eventos

-- | Detecta explosões e danos entre estados
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

-- | Verifica se objeto é explosivo
eObjetoExplosivo :: Objeto -> Bool
eObjetoExplosivo (Barril _ True) = True
eObjetoExplosivo (Disparo _ _ Bazuca _ _) = True
eObjetoExplosivo (Disparo _ _ Mina (Just 0) _) = True
eObjetoExplosivo (Disparo _ _ Dinamite (Just 0) _) = True
eObjetoExplosivo _ = False

-- | Obtém posição de um objeto
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril pos _) = pos
posicaoObjeto (Disparo pos _ _ _ _) = pos

-- | Detecta danos nas minhocas
detectarDanosMinhocas :: [Minhoca] -> [Minhoca] -> [AnimacaoAtiva]
detectarDanosMinhocas antes depois =
  [ AnimDano pos dano 1.0
  | (m1, m2) <- zip antes depois
  , Just pos <- [posicaoMinhoca m2]
  , let dano = calcularDano (vidaMinhoca m1) (vidaMinhoca m2)
  , dano > 0
  ]

-- | Calcula dano entre dois estados de vida
calcularDano :: VidaMinhoca -> VidaMinhoca -> Int
calcularDano (Viva v1) (Viva v2) = max 0 (v1 - v2)
calcularDano (Viva v1) Morta = v1
calcularDano _ _ = 0

--------------------------------------------------------------------------------
-- * Animações

-- | Atualiza todas as animações ativas
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

--------------------------------------------------------------------------------
-- * Condição de Vitória

-- | Verifica se algum jogador venceu
-- MODIFICADO: No modo Treino, faz restart automático quando morre
verificarCondicaoVitoria :: EstadoPartida -> EstadoJogo
verificarCondicaoVitoria partida
  | modoPartida partida == Treino = 
      -- Verifica se minhoca de treino morreu
      let minhocas = minhocasEstado (estadoWorms partida)
          verdesVivas = length [m | (i, m) <- zip [0 :: Int ..] minhocas, even i, estaRealmenteViva m]
      in if verdesVivas == 0
         then -- RESTART AUTOMÁTICO!
              let idx = mapaAtualIdx partida
                  mapa = selecionarMapa idx
              in Jogando (criarPartidaComMapa Treino (criarEstadoTreino mapa) idx)
         else Jogando partida
  | otherwise = 
      let minhocas = minhocasEstado (estadoWorms partida)
          modo = modoPartida partida
          
          verdesVivas = length [m | (i, m) <- zip [0 :: Int ..] minhocas, even i, estaRealmenteViva m]
          azuisVivas = length [m | (i, m) <- zip [0 :: Int ..] minhocas, odd i, estaRealmenteViva m]
      in 
         if verdesVivas == 0 && azuisVivas > 0
         then Victory (EstadoFinal 0 (Just VenceuAzul) VoltarMenu modo)
         else if azuisVivas == 0 && verdesVivas > 0
         then Victory (EstadoFinal 0 (Just VenceuVerde) VoltarMenu modo)
         else if verdesVivas == 0 && azuisVivas == 0
         then Victory (EstadoFinal 0 Nothing VoltarMenu modo)
         else Jogando partida

-- | Verifica se minhoca está REALMENTE viva (Viva com hp >= 1)
estaRealmenteViva :: Minhoca -> Bool
estaRealmenteViva m = case vidaMinhoca m of
  Viva hp -> hp >= 1
  Morta -> False

-- | Verifica se minhoca tem vida >= 1 (MUITO SIMPLES)
vidaPositiva :: Minhoca -> Bool
vidaPositiva = estaRealmenteViva

--------------------------------------------------------------------------------
-- * Utilidades

-- | Verifica se minhoca está viva (vida >= 1)
--
-- __IMPORTANTE:__ Uma minhoca com @Viva 0@ ou menos está morta!
minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
  Viva hp -> hp >= 1
  Morta -> False

-- | Verifica se minhoca tem posição válida e vida >= 1
minhocaAtiva :: Minhoca -> Bool
minhocaAtiva m = case (vidaMinhoca m, posicaoMinhoca m) of
  (Viva hp, Just _) -> hp >= 1
  _ -> False