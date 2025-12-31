{-|
Module      : EstadoJogo
Description : Estados do jogo e estruturas de dados
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo define todas as estruturas de dados que representam o estado do jogo
em diferentes fases: menu, seleção de modo, partida ativa, vitória, etc.
-}
module EstadoJogo where

import Graphics.Gloss
import Labs2025

--------------------------------------------------------------------------------
-- * Estados Principais

-- | Estado global do jogo - representa todas as fases possíveis
data EstadoJogo
  = Menu EstadoMenu              -- ^ Menu principal
  | SelecaoModo EstadoSelecao    -- ^ Seleção de modo de jogo
  | SelecaoMapaTreino EstadoSelecaoMapa  -- ^ NOVO: Seleção de mapa para treino
  | Jogando EstadoPartida        -- ^ Partida em curso
  | GameOver EstadoFinal         -- ^ Fim de jogo (derrota)
  | Victory EstadoFinal          -- ^ Fim de jogo (vitória)
  | Tutorial EstadoTutorial      -- ^ Ecrã de tutorial
  deriving (Show)

--------------------------------------------------------------------------------
-- * Estado do Menu

-- | Estado do menu principal
data EstadoMenu = EstadoMenu
  { opcaoSelecionadaMenu :: OpcaoMenu  -- ^ Opção atualmente selecionada
  , animacaoMenu :: Float              -- ^ Tempo de animação
  } deriving (Show)

-- | Opções disponíveis no menu principal
data OpcaoMenu
  = OpcaoPlay      -- ^ Jogar
  | OpcaoTutorial  -- ^ Ver tutorial
  | OpcaoExit      -- ^ Sair
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * Seleção de Modo

-- | Estado da seleção de modo de jogo
data EstadoSelecao = EstadoSelecao
  { modoSelecionado :: ModoJogo       -- ^ Modo atualmente selecionado
  , animacaoGlowSelecao :: Float      -- ^ Animação de brilho
  } deriving (Show)

-- | Modos de jogo disponíveis
data ModoJogo
  = DoisJogadores  -- ^ 1 vs 1 local
  | VsBot          -- ^ Jogador vs Bot
  | Treino         -- ^ Modo treino
  | Voltar         -- ^ Voltar ao menu
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * NOVO: Seleção de Mapa (Treino)

-- | Estado da seleção de mapa para treino
data EstadoSelecaoMapa = EstadoSelecaoMapa
  { mapaSelecionado :: Int        -- ^ Índice do mapa selecionado (0-4)
  , animacaoSelecaoMapa :: Float  -- ^ Tempo de animação
  } deriving (Show)

--------------------------------------------------------------------------------
-- * Partida em Jogo

-- | Estado completo de uma partida em curso
data EstadoPartida = EstadoPartida
  { estadoWorms :: Estado           -- ^ Estado do jogo (mapa, objetos, minhocas)
  , modoPartida :: ModoJogo         -- ^ Modo de jogo atual
  , pausado :: Bool                 -- ^ Jogo pausado?
  , modoJogo :: ModoJogo            -- ^ Modo de jogo (duplicado para compatibilidade)
  , camera :: Camera                -- ^ Câmera do jogo
  , animacoes :: [AnimacaoAtiva]    -- ^ Animações em curso
  , turnoAtual :: Int               -- ^ Número do turno
  , frameCounter :: Int             -- ^ Contador de frames (para delays)
  
  -- Sistema de Turnos
  , jogadorAtual :: Int             -- ^ 0 = Verde, 1 = Azul
  , tempoRestante :: Float          -- ^ Segundos restantes no turno
  , tempoTotal :: Float             -- ^ Tempo total por turno (30s)
  
  -- Menu de Armas
  , menuArmasAbertoP1 :: Bool       -- ^ Menu de armas do jogador 1 aberto?
  , menuArmasAbertoP2 :: Bool       -- ^ Menu de armas do jogador 2 aberto?
  
  -- Direções e Armas Selecionadas
  , ultimaDirecaoP1 :: Direcao      -- ^ Última direção do jogador 1
  , ultimaDirecaoP2 :: Direcao      -- ^ Última direção do jogador 2
  , armaSelecionadaP1 :: Maybe TipoArma  -- ^ Arma selecionada pelo jogador 1
  , armaSelecionadaP2 :: Maybe TipoArma  -- ^ Arma selecionada pelo jogador 2
  
  -- NOVO: Modo Voo (Jetpack)
  , modoVooP1 :: Bool               -- ^ Jogador 1 está em modo voo?
  , modoVooP2 :: Bool               -- ^ Jogador 2 está em modo voo?
  
  -- Animação
  , frameAnimacao :: Int            -- ^ Frame atual de animação
  , tempoAnimacao :: Float          -- ^ Tempo de animação
  
  -- Índice do mapa (para restart no treino)
  , mapaAtualIdx :: Int             -- ^ Índice do mapa atual
  } deriving (Show)

-- | Câmera do jogo
data Camera = Camera
  { posCamera :: (Float, Float)     -- ^ Posição da câmera
  , zoomCamera :: Float             -- ^ Nível de zoom
  } deriving (Show)

--------------------------------------------------------------------------------
-- * Animações

-- | Tipos de animação ativa no jogo
data AnimacaoAtiva
  = AnimExplosao Posicao Float Int      -- ^ Explosão: posição, tempo, frame
  | AnimDano Posicao Int Float          -- ^ Dano: posição, valor, tempo restante
  | AnimMovimento Int Posicao Posicao Float  -- ^ Movimento: índice, de, para, progresso
  deriving (Show)

--------------------------------------------------------------------------------
-- * Telas Finais

-- | Estado do ecrã de fim de jogo
data EstadoFinal = EstadoFinal
  { pontuacaoFinal :: Int               -- ^ Pontuação final
  , vencedorJogo :: Maybe VencedorJogo  -- ^ Quem venceu (ou empate)
  , opcaoFinal :: OpcaoFinal            -- ^ Opção selecionada
  , modoJogoFinal :: ModoJogo           -- ^ Modo de jogo (para restart)
  } deriving (Show)

-- | Opções no ecrã de fim de jogo
data OpcaoFinal
  = Restart      -- ^ Jogar novamente
  | VoltarMenu   -- ^ Voltar ao menu
  deriving (Show, Eq)

-- | Possíveis vencedores
data VencedorJogo
  = VenceuVerde  -- ^ Jogador 1 (Verde) venceu
  | VenceuAzul   -- ^ Jogador 2 (Azul) venceu
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * Tutorial

-- | Estado do ecrã de tutorial
data EstadoTutorial = EstadoTutorial 
  { paginaTutorial :: Int
  , menuSelecionado :: Bool
  } deriving (Show)

--------------------------------------------------------------------------------
-- * Criação de Partida

-- | Cria uma nova partida com o modo e estado inicial especificados
criarPartida :: ModoJogo -> Estado -> EstadoPartida
criarPartida modo estadoInicial = EstadoPartida
  { estadoWorms = estadoInicial
  , modoPartida = modo
  , modoJogo = modo
  , pausado = False
  , camera = Camera (0, 0) 1.0
  , animacoes = []
  , turnoAtual = 1
  
  -- Turnos (começa jogador 1 - Verde)
  , jogadorAtual = 0
  , tempoRestante = 30.0
  , tempoTotal = 30.0
  
  -- Menus de armas
  , menuArmasAbertoP1 = False
  , menuArmasAbertoP2 = False
  
  -- Direções e armas
  , ultimaDirecaoP1 = Este
  , ultimaDirecaoP2 = Oeste
  , armaSelecionadaP1 = Nothing
  , armaSelecionadaP2 = Nothing
  
  -- NOVO: Modo voo desativado por defeito
  , modoVooP1 = False
  , modoVooP2 = False
  
  -- Animação
  , frameAnimacao = 0
  , tempoAnimacao = 0.0
  , frameCounter = 0
  
  -- Índice do mapa (será definido depois)
  , mapaAtualIdx = 0
  }

-- | Cria partida com índice do mapa
criarPartidaComMapa :: ModoJogo -> Estado -> Int -> EstadoPartida
criarPartidaComMapa modo estadoInicial idx = 
  (criarPartida modo estadoInicial) { mapaAtualIdx = idx }