{-|
Module      : EstadoJogo
Description : Estados do jogo e transições entre eles.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Define os diferentes estados do jogo (Menu, Seleção de Modo, Jogando, etc)
e as transições entre eles.
-}

module EstadoJogo where

import Graphics.Gloss
import Labs2025
import Assets

-- | Estado principal do jogo - pode estar em diferentes "telas"
data EstadoJogo
  = Menu EstadoMenu
  | SelecaoModo EstadoSelecao
  | Jogando EstadoPartida
  | GameOver EstadoFinal
  | Victory EstadoFinal
  | Tutorial EstadoTutorial
  deriving (Show)

-- | Estado do menu principal
data EstadoMenu = EstadoMenu
  { opcaoSelecionadaMenu :: OpcaoMenu
  , animacaoGlow :: Float  -- para efeito de pulsar nos botões
  } deriving (Show)

-- | Opções do menu principal
data OpcaoMenu = OpcaoPlay | OpcaoTutorial | OpcaoExit
  deriving (Show, Eq)

-- | Estado da seleção de modo de jogo
data EstadoSelecao = EstadoSelecao
  { modoSelecionado :: ModoJogo
  , animacaoGlowSelecao :: Float
  } deriving (Show)

-- | Modos de jogo disponíveis
data ModoJogo 
  = DoisJogadores    -- Humano vs Humano
  | VsBot            -- Humano vs IA (usa Tarefa4)
  | Treino           -- Modo livre, sem limite de jogadas
  deriving (Show, Eq)

-- | Estado durante uma partida
data EstadoPartida = EstadoPartida
  { estadoWorms :: Estado           -- Estado do jogo Worms (Labs2025)
  , modoPartida :: ModoJogo          -- Que modo está a jogar
  , turnoAtual :: Int                -- Número do turno
  , jogadorAtual :: NumMinhoca       -- Qual minhoca está a jogar
  , animacoes :: [AnimacaoAtiva]     -- Animações ativas
  , camera :: Camera                 -- Posição da câmera
  , pausado :: Bool                  -- Jogo pausado?
  } deriving (Show)

-- | Câmera do jogo (para scroll/zoom)
data Camera = Camera
  { posCamera :: (Float, Float)  -- Posição x,y
  , zoomCamera :: Float          -- Nível de zoom
  } deriving (Show)

-- | Animação ativa no jogo
data AnimacaoAtiva
  = AnimExplosao Posicao Float Int          -- posição, tempo, frame atual
  | AnimDano Posicao Int Float              -- posição, dano, tempo restante
  | AnimMovimento NumMinhoca Posicao Posicao Float  -- num, de, para, progresso
  deriving (Show)

-- | Estado das telas finais (Game Over ou Victory)
data EstadoFinal = EstadoFinal
  { pontuacaoFinal :: Int
  , opcaoFinal :: OpcaoFinal
  } deriving (Show)

-- | Opções nas telas finais
data OpcaoFinal = Restart | VoltarMenu
  deriving (Show, Eq)

-- | Estado do tutorial
data EstadoTutorial = EstadoTutorial
  { paginaTutorial :: Int  -- Qual página do tutorial (0-N)
  } deriving (Show)

-- | Estado inicial do jogo (começa no menu)
estadoInicial :: Assets -> EstadoJogo
estadoInicial _ = Menu (EstadoMenu OpcaoPlay 0.0)

-- | Câmera inicial (centrada)
cameraInicial :: Camera
cameraInicial = Camera (0, 0) 1.0

-- | Cria estado de partida inicial para um modo
criarPartida :: ModoJogo -> Estado -> EstadoPartida
criarPartida modo estadoWorms = EstadoPartida
  { estadoWorms = estadoWorms
  , modoPartida = modo
  , turnoAtual = 0
  , jogadorAtual = 0
  , animacoes = []
  , camera = cameraInicial
  , pausado = False
  }