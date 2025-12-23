module Worms where

import Labs2025

-- | Estado do jogo no Gloss.
data Worms = Worms 
  { estadoJogo :: Estado
  , minhocaAtual :: NumMinhoca
  , modoJogo :: ModoJogo
  , mensagem :: String
  }

-- | Modo de jogo atual
data ModoJogo 
  = Esperando        -- Aguardando jogada
  | Animando Float   -- Animando (contador de tempo)
  | GameOver
  deriving (Eq, Show)

-- | Cria um estado inicial do jogo com um mapa simples
estadoInicial :: Worms
estadoInicial = Worms
  { estadoJogo = Estado
      { mapaEstado = mapaSimples
      , objetosEstado = []
      , minhocasEstado = 
          [ Minhoca
              { posicaoMinhoca = Just (3, 5)
              , vidaMinhoca = Viva 100
              , jetpackMinhoca = 5
              , escavadoraMinhoca = 3
              , bazucaMinhoca = 5
              , minaMinhoca = 3
              , dinamiteMinhoca = 2
              }
          , Minhoca
              { posicaoMinhoca = Just (3, 15)
              , vidaMinhoca = Viva 100
              , jetpackMinhoca = 5
              , escavadoraMinhoca = 3
              , bazucaMinhoca = 5
              , minaMinhoca = 3
              , dinamiteMinhoca = 2
              }
          ]
      }
  , minhocaAtual = 0
  , modoJogo = Esperando
  , mensagem = "Jogador 1 - Pressione WASD para mover, 1-5 para armas"
  }

-- | Mapa simples para teste (20x30)
mapaSimples :: Mapa
mapaSimples = 
  [ replicate 30 Ar    -- linha 0
  , replicate 30 Ar    -- linha 1
  , replicate 30 Ar    -- linha 2
  , replicate 30 Ar    -- linha 3
  , replicate 30 Ar    -- linha 4
  , replicate 30 Ar    -- linha 5
  , replicate 30 Ar    -- linha 6
  , replicate 30 Ar    -- linha 7
  , replicate 30 Ar    -- linha 8
  , replicate 30 Ar    -- linha 9
  , replicate 30 Ar    -- linha 10
  , replicate 30 Ar    -- linha 11
  , terraCom 10 20     -- linha 12 - plataforma
  , replicate 30 Terra -- linha 13
  , replicate 30 Terra -- linha 14
  , replicate 30 Pedra -- linha 15
  , replicate 30 Pedra -- linha 16
  , replicate 30 Pedra -- linha 17
  , replicate 30 Pedra -- linha 18
  , replicate 30 Pedra -- linha 19
  ]
  where
    terraCom inicio fim = replicate inicio Ar ++ replicate (fim - inicio) Terra ++ replicate (30 - fim) Ar