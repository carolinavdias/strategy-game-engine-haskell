{-|
Module      : Tempo
Description : Atualização temporal do jogo.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Integra a Tarefa3 para avançar o estado do jogo ao longo do tempo:
- Gravidade e quedas das minhocas
- Movimento de projéteis
- Explosões
- Animações
-}

module Tempo where

import EstadoJogo
import Tarefa3
import Labs2025

-- | Tempo em segundos
type Segundos = Float

-- | Tempo de tick do jogo (quantos segundos entre cada avancaEstado)
tempoTick :: Segundos
tempoTick = 0.1  -- 10 ticks por segundo

-- | Acumulador de tempo para controlar ticks
type AcumuladorTempo = Float

--------------------------------------------------------------------------------
-- * ATUALIZAÇÃO PRINCIPAL

-- | Atualiza o estado da partida ao longo do tempo
atualizarPartida :: Segundos -> EstadoPartida -> EstadoPartida
atualizarPartida dt partida
  | pausado partida = partida  -- Não atualiza se pausado
  | otherwise = 
      let -- Atualiza animações (sempre, independente de ticks)
          partidaComAnimacoes = partida { animacoes = atualizarAnimacoes dt (animacoes partida) }
          
          -- Atualiza animação de walk
          partidaComWalk = atualizarAnimacaoWalk dt partidaComAnimacoes
          
          -- Avança física do jogo (gravidade, projéteis, explosões)
          partidaComFisica = avancarFisica dt partidaComWalk
          
          -- Verifica fim de jogo
          partidaFinal = verificarCondicaoVitoria partidaComFisica
          
      in partidaFinal

--------------------------------------------------------------------------------
-- * ANIMAÇÃO DE WALK

-- | Atualiza animação de andar (alterna entre idle, walk1, walk2)
atualizarAnimacaoWalk :: Segundos -> EstadoPartida -> EstadoPartida
atualizarAnimacaoWalk dt partida =
  let novoTempo = tempoAnimacao partida + dt
      -- Alterna frame a cada 0.15 segundos
      (novoFrame, tempoReset) = if novoTempo > 0.15
                                then case frameAnimacao partida of
                                  0 -> (1, 0.0)  -- idle -> walk1
                                  1 -> (2, 0.0)  -- walk1 -> walk2
                                  _ -> (0, 0.0)  -- walk2 -> idle
                                else (frameAnimacao partida, novoTempo)
  in partida
       { tempoAnimacao = tempoReset
       , frameAnimacao = novoFrame
       }

--------------------------------------------------------------------------------
-- * FÍSICA DO JOGO (TAREFA 3)

-- | Avança a física do jogo usando Tarefa3
avancarFisica :: Segundos -> EstadoPartida -> EstadoPartida
avancarFisica dt partida =
  let estadoAtual = estadoWorms partida
      -- Aplica avancaEstado da Tarefa3
      novoEstado = avancaEstado estadoAtual
      
      -- Detecta explosões e cria animações
      novasAnimacoes = detectarExplosoes estadoAtual novoEstado
      
  in partida 
       { estadoWorms = novoEstado
       , animacoes = animacoes partida ++ novasAnimacoes
       }

-- | Detecta explosões comparando estados antes/depois
detectarExplosoes :: Estado -> Estado -> [AnimacaoAtiva]
detectarExplosoes estadoAntes estadoDepois =
  let objetosAntes = objetosEstado estadoAntes
      objetosDepois = objetosEstado estadoDepois
      objetosRemovidos = [ obj | obj <- objetosAntes, obj `notElem` objetosDepois ]
      explosoes = [ AnimExplosao (posicaoObjeto obj) 0.0 0 
                  | obj <- objetosRemovidos
                  , eObjetoExplosivo obj
                  ]
      
      -- Detecta dano em minhocas
      minhocasAntes = minhocasEstado estadoAntes
      minhocasDepois = minhocasEstado estadoDepois
      danos = detectarDanosMinhocas minhocasAntes minhocasDepois
      
  in explosoes ++ danos

-- | Verifica se um objeto é explosivo (barril, mina, dinamite, bazuca)
eObjetoExplosivo :: Objeto -> Bool
eObjetoExplosivo (Barril _ True) = True
eObjetoExplosivo (Disparo _ _ Bazuca _ _) = True
eObjetoExplosivo (Disparo _ _ Mina (Just 0) _) = True
eObjetoExplosivo (Disparo _ _ Dinamite (Just 0) _) = True
eObjetoExplosivo _ = False

-- | Posição de um objeto
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril pos _) = pos
posicaoObjeto (Disparo pos _ _ _ _) = pos

-- | Detecta dano causado a minhocas
detectarDanosMinhocas :: [Minhoca] -> [Minhoca] -> [AnimacaoAtiva]
detectarDanosMinhocas antes depois =
  [ AnimDano pos dano 1.0
  | (m1, m2) <- zip antes depois
  , Just pos <- [posicaoMinhoca m2]
  , let dano = calcularDano (vidaMinhoca m1) (vidaMinhoca m2)
  , dano > 0
  ]

-- | Calcula dano sofrido comparando vidas
calcularDano :: VidaMinhoca -> VidaMinhoca -> Int
calcularDano (Viva v1) (Viva v2) = max 0 (v1 - v2)
calcularDano (Viva v1) Morta = v1
calcularDano _ _ = 0

--------------------------------------------------------------------------------
-- * ANIMAÇÕES

-- | Atualiza todas as animações ativas
atualizarAnimacoes :: Segundos -> [AnimacaoAtiva] -> [AnimacaoAtiva]
atualizarAnimacoes dt = filter animacaoAtiva . map (atualizarAnimacao dt)
  where
    atualizarAnimacao :: Segundos -> AnimacaoAtiva -> AnimacaoAtiva
    atualizarAnimacao dt (AnimExplosao pos tempo frame) =
      let novoTempo = tempo + dt
          -- Cada frame dura 0.15 segundos
          novoFrame = if novoTempo > 0.15 * fromIntegral (frame + 1)
                      then frame + 1
                      else frame
      in AnimExplosao pos novoTempo novoFrame
    
    atualizarAnimacao dt (AnimDano pos dano tempo) =
      AnimDano pos dano (tempo - dt)
    
    atualizarAnimacao dt (AnimMovimento num de para prog) =
      AnimMovimento num de para (min 1.0 (prog + dt * 3))  -- 3x velocidade
    
    -- Verifica se animação ainda está ativa
    animacaoAtiva :: AnimacaoAtiva -> Bool
    animacaoAtiva (AnimExplosao _ _ frame) = frame < 3  -- 3 frames
    animacaoAtiva (AnimDano _ _ tempo) = tempo > 0
    animacaoAtiva (AnimMovimento _ _ _ prog) = prog < 1.0

--------------------------------------------------------------------------------
-- * CONDIÇÕES DE VITÓRIA

-- | Verifica se o jogo terminou
verificarCondicaoVitoria :: EstadoPartida -> EstadoPartida
verificarCondicaoVitoria partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVivas = filter minhocaViva minhocas
      
      -- Conta minhocas vivas por equipe (par = verde, ímpar = azul)
      verdesVivas = length [ m | (i, m) <- zip [0..] minhocas, even i, minhocaViva m ]
      azuisVivas = length [ m | (i, m) <- zip [0..] minhocas, odd i, minhocaViva m ]
      
  in if verdesVivas == 0 && azuisVivas == 0
       then partida  -- Empate (mas mantém jogando)
     else if verdesVivas == 0
       then partida  -- Azul vence (será tratado em Eventos)
     else if azuisVivas == 0
       then partida  -- Verde vence (será tratado em Eventos)
     else partida  -- Jogo continua

-- | Verifica se uma minhoca está viva
minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
  Viva _ -> True
  Morta -> False

--------------------------------------------------------------------------------
-- * CÂMERA FIXA (NÃO FAZ NADA AGORA)

-- | Câmera é fixa, não precisa atualizar
atualizarCamera :: EstadoPartida -> EstadoPartida
atualizarCamera = id  -- Retorna estado inalterado

--------------------------------------------------------------------------------
-- * MODO BOT (TAREFA 4)

-- | Se for modo VS Bot, executa jogada do bot
executarBotSeNecessario :: EstadoPartida -> EstadoPartida
executarBotSeNecessario partida
  | modoPartida partida /= VsBot = partida
  | odd (jogadorAtual partida) = partida  -- Bot é sempre jogador ímpar (azul)
  | otherwise = partida  -- TODO: Integrar Tarefa4.tatica

--------------------------------------------------------------------------------
-- * INTEGRAÇÃO COMPLETA

-- | Atualização completa da partida (usar no Main.hs)
atualizarPartidaCompleta :: Segundos -> EstadoPartida -> EstadoPartida
atualizarPartidaCompleta dt =
  atualizarPartida dt        -- Física + Animações
  . atualizarCamera          -- Câmera segue jogador
  . executarBotSeNecessario  -- IA se necessário