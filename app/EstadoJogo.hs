{-|
Module      : EstadoJogo
Description : Estados do jogo e estruturas de dados.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
-}

module EstadoJogo where

import Graphics.Gloss
import Labs2025

--------------------------------------------------------------------------------
-- * ESTADOS PRINCIPAIS

-- | Estado global do jogo
data EstadoJogo
  = Menu EstadoMenu
  | SelecaoModo EstadoSelecao
  | Jogando EstadoPartida
  | GameOver EstadoFinal
  | Victory EstadoFinal
  | Tutorial EstadoTutorial
  deriving (Show)

--------------------------------------------------------------------------------
-- * ESTADO DO MENU

data EstadoMenu = EstadoMenu
  { opcaoSelecionadaMenu :: OpcaoMenu
  , animacaoMenu :: Float
  } deriving (Show)

data OpcaoMenu
  = OpcaoPlay
  | OpcaoTutorial
  | OpcaoExit
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * SELEÇÃO DE MODO

data EstadoSelecao = EstadoSelecao
  { modoSelecionado :: ModoJogo
  , animacaoGlowSelecao :: Float
  } deriving (Show)

data ModoJogo
  = DoisJogadores
  | VsBot
  | Treino
  | Voltar
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * PARTIDA EM JOGO (COM TURNOS!)

data EstadoPartida = EstadoPartida
  { estadoWorms :: Estado
  , modoPartida :: ModoJogo
  , pausado :: Bool
  , modoJogo :: ModoJogo
  , camera :: Camera
  , animacoes :: [AnimacaoAtiva]
  , turnoAtual :: Int
  , frameCounter :: Int
  
  -- SISTEMA DE TURNOS (NOVO!)
  , jogadorAtual :: Int              -- 0 = Verde, 1 = Azul
  , tempoRestante :: Float           -- Segundos restantes (30s)
  , tempoTotal :: Float              -- Tempo total do turno (30s)
  
  -- MENU DE ARMAS
  , menuArmasAbertoP1 :: Bool        -- Menu armas jogador 1 aberto?
  , menuArmasAbertoP2 :: Bool        -- Menu armas jogador 2 aberto?
  
  -- ESTADO ANTERIOR
  , ultimaDirecaoP1 :: Direcao
  , ultimaDirecaoP2 :: Direcao
  , armaSelecionadaP1 :: Maybe TipoArma
  , armaSelecionadaP2 :: Maybe TipoArma
  
  -- ANIMAÇÃO
  , frameAnimacao :: Int
  , tempoAnimacao :: Float
  } deriving (Show)

-- | Câmera do jogo
data Camera = Camera
  { posCamera :: (Float, Float)
  , zoomCamera :: Float
  } deriving (Show)

--------------------------------------------------------------------------------
-- * ANIMAÇÕES

data AnimacaoAtiva
  = AnimExplosao Posicao Float Int
  | AnimDano Posicao Int Float
  | AnimMovimento Int Posicao Posicao Float
  deriving (Show)

--------------------------------------------------------------------------------
-- * TELAS FINAIS

data EstadoFinal = EstadoFinal
  { pontuacaoFinal :: Int
  , vencedorJogo :: Maybe VencedorJogo  -- ← ADICIONA!
  , opcaoFinal :: OpcaoFinal
  } deriving (Show)

data OpcaoFinal
  = Restart
  | VoltarMenu
  deriving (Show, Eq)

data VencedorJogo
  = VenceuVerde  -- Jogador 1
  | VenceuAzul   -- Jogador 2
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- * TUTORIAL

data EstadoTutorial = EstadoTutorial
  { paginaTutorial :: Int
  } deriving (Show)

--------------------------------------------------------------------------------
-- * CRIAÇÃO DE PARTIDA

-- | Cria uma nova partida no modo especificado
criarPartida :: ModoJogo -> Estado -> EstadoPartida
criarPartida modo estadoInicial = EstadoPartida
  { estadoWorms = estadoInicial
  , modoPartida = modo
  , modoJogo = modo  -- ← ADICIONA ESTA LINHA!
  , pausado = False
  , camera = Camera (0, 0) 1.0
  , animacoes = []
  , turnoAtual = 1
  
  -- TURNOS (COMEÇA JOGADOR 1 - VERDE)
  , jogadorAtual = 0
  , tempoRestante = 30.0
  , tempoTotal = 30.0
  
  -- MENUS DE ARMAS
  , menuArmasAbertoP1 = False
  , menuArmasAbertoP2 = False
  
  -- DIREÇÕES E ARMAS
  , ultimaDirecaoP1 = Este
  , ultimaDirecaoP2 = Oeste
  , armaSelecionadaP1 = Nothing
  , armaSelecionadaP2 = Nothing
  
  -- ANIMAÇÃO
  , frameAnimacao = 0
  , tempoAnimacao = 0.0
  , frameCounter = 0
  }