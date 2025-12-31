{-|
Module      : Mapas
Description : Coleção de mapas para o jogo
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo contém a coleção de mapas disponíveis no jogo.

Características:

* 5 mapas únicos com layouts distintos
* Estrutura consistente (pedra, ar, terra e água)
* Bordas sempre indestrutíveis
* Função de rotação automática de mapas

No modo Treino, permite a seleção manual do mapa,
mantendo o mesmo mapa em caso de restart.
-}
module Mapas where

import Labs2025

--------------------------------------------------------------------------------
-- * Mapa 1: Clássico

-- | Mapa clássico com terreno plano.
--
-- __Estrutura:__
--
--   * 20 linhas totais (índices 0-19)
--   * Linha 0: Teto de Pedra
--   * Linhas 1-9: Ar (9 linhas)
--   * Linhas 10-17: Terra (8 linhas)
--   * Linhas 18-19: Água (2 linhas)
--
-- __Posições das minhocas:__
--
--   * Verde: (9, 8) — em cima da Terra (linha 10)
--   * Azul: (9, 26) — em cima da Terra (linha 10)
mapaClassico :: Mapa
mapaClassico = 
  [ [Pedra | _ <- [1..34]] ] ++                                                 -- Linha 0: Teto
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++ -- Linhas 1-9: Ar
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++ -- Linhas 10-17: Terra
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]    -- Linhas 18-19: Água

-- | Barris do mapa clássico.
-- Posicionados na linha 9 (última linha de Ar, em cima da Terra).
barrisClassico :: [Objeto]
barrisClassico = 
  [ Barril (9, 10) False
  , Barril (9, 16) False
  , Barril (9, 21) False
  ]

-- | Minhocas do mapa clássico.
-- Ambas na linha 9, com Terra por baixo (linha 10).
--
-- __Munições:__ Jetpack=50, Escavadora=30, Bazuca=15, Mina=10, Dinamite=10
minhocasClassico :: [Minhoca]
minhocasClassico = 
  [ Minhoca (Just (9, 8)) (Viva 100) 50 30 15 10 10    -- Verde (índice 0)
  , Minhoca (Just (9, 26)) (Viva 100) 50 30 15 10 10   -- Azul (índice 1)
  ]

--------------------------------------------------------------------------------
-- * Mapa 2: Montanhas

-- | Mapa com ilha central elevada.
--
-- __Característica especial:__
-- Ilha de Terra elevada nas colunas 15-19, linhas 8-9.
--
-- __Estratégia:__
-- A ilha central oferece vantagem de altura para disparos.
mapaMontanhas :: Mapa
mapaMontanhas =
  [ [Pedra | _ <- [1..34]] ] ++                                                 -- Linha 0: Teto
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..7] ] ++ -- Linhas 1-7: Ar
  [ [if c == 1 || c == 34 then Pedra
     else if c >= 15 && c <= 19 then Terra                                       -- Ilha elevada
     else Ar | c <- [1..34]] | _ <- [1..2] ] ++                                  -- Linhas 8-9: Ilha
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++ -- Linhas 10-17: Terra
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]    -- Linhas 18-19: Água

-- | Barris do mapa montanhas.
-- Inclui um barril na ilha elevada (linha 7).
barrisMontanhas :: [Objeto]
barrisMontanhas = 
  [ Barril (9, 10) False    -- Lado esquerdo
  , Barril (9, 24) False    -- Lado direito
  , Barril (7, 16) False    -- Na ilha elevada (em cima da Terra na linha 8)
  ]

-- | Minhocas do mapa montanhas.
-- Posicionadas nos lados, no chão plano.
--
-- __Munições:__ Jetpack=50, Escavadora=30, Bazuca=15, Mina=10, Dinamite=10
minhocasMontanhas :: [Minhoca]
minhocasMontanhas = 
  [ Minhoca (Just (9, 8)) (Viva 100) 50 30 15 10 10    -- Verde (esquerda)
  , Minhoca (Just (9, 26)) (Viva 100) 50 30 15 10 10   -- Azul (direita)
  ]

--------------------------------------------------------------------------------
-- * Mapa 3: Lago Central

-- | Mapa com lago estreito no centro.
--
-- __Característica especial:__
-- Lago de água nas colunas 16-18 (3 blocos de largura).
--
-- __Estratégia:__
-- O lago divide o mapa, forçando uso de armas de longo alcance
-- ou Jetpack para atravessar.
mapaLago :: Mapa
mapaLago =
  [ [Pedra | _ <- [1..34]] ] ++                                                 -- Linha 0: Teto
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++ -- Linhas 1-9: Ar
  [ [if c == 1 || c == 34 then Pedra
     else if c >= 16 && c <= 18 then Agua                                        -- Lago estreito
     else Terra | c <- [1..34]] | _ <- [1..8] ] ++                               -- Linhas 10-17: Terra+Lago
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]    -- Linhas 18-19: Água

-- | Barris do mapa lago.
-- Posicionados antes e depois do lago.
barrisLago :: [Objeto]
barrisLago = 
  [ Barril (9, 3) False    -- Longe do lago
  , Barril (9, 10) False    -- Perto do lago (esquerda)
  , Barril (9, 21) False    -- Perto do lago (direita)
  , Barril (9, 31) False    -- Longe do lago
  ]

-- | Minhocas do mapa lago.
-- Uma de cada lado do lago.
--
-- __Munições:__ Jetpack=50, Escavadora=30, Bazuca=15, Mina=10, Dinamite=10
minhocasLago :: [Minhoca]
minhocasLago = 
  [ Minhoca (Just (9, 8)) (Viva 100) 50 30 15 10 10    -- Verde (lado esquerdo)
  , Minhoca (Just (9, 25)) (Viva 100) 50 30 15 10 10   -- Azul (lado direito)
  ]

--------------------------------------------------------------------------------
-- * Mapa 4: Pedreira

-- | Mapa com pilares de pedra indestrutíveis.
--
-- __Característica especial:__
-- Pilares de Pedra nas colunas 10, 14, 20 e 24.
--
-- __Estratégia:__
-- Os pilares bloqueiam disparos diretos, exigindo ângulos diferentes
-- ou uso de explosões em área.
mapaPedreira :: Mapa
mapaPedreira =
  [ [Pedra | _ <- [1..34]] ] ++                                                 -- Linha 0: Teto
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++ -- Linhas 1-9: Ar
  [ [if c == 1 || c == 34 then Pedra
     else if c `elem` [10, 14, 20, 24] then Pedra                                -- Pilares
     else Terra | c <- [1..34]] | _ <- [1..8] ] ++                               -- Linhas 10-17: Terra+Pilares
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]    -- Linhas 18-19: Água

-- | Barris do mapa pedreira.
-- Posicionados entre os pilares para máximo impacto.
barrisPedreira :: [Objeto]
barrisPedreira = 
  [ Barril (9, 10) False     -- Antes do 1º pilar
  , Barril (9, 16) False    -- Entre pilares 2 e 3
  , Barril (9, 23) False    -- Entre pilares 3 e 4
    ]

-- | Minhocas do mapa pedreira.
-- Nas extremidades, protegidas pelos primeiros pilares.
--
-- __Munições:__ Jetpack=50, Escavadora=30, Bazuca=15, Mina=10, Dinamite=10
minhocasPedreira :: [Minhoca]
minhocasPedreira = 
  [ Minhoca (Just (9, 6)) (Viva 100) 50 30 15 10 10    -- Verde (extremo esquerdo)
  , Minhoca (Just (9, 28)) (Viva 100) 50 30 15 10 10   -- Azul (extremo direito)
  ]

--------------------------------------------------------------------------------
-- * Mapa 5: Ilhas Gémeas

-- | Mapa com duas ilhas elevadas laterais.
--
-- __Estrutura especial:__
--
--   * Linhas 1-5: Ar
--   * Linhas 6-8: Ilhas nas colunas 6-10 e 24-28
--   * Linhas 9-17: Terra completa com lago central
--   * Linhas 18-19: Água
--
-- __Posições das minhocas:__
--
--   * Verde: (5, 7) — na ilha esquerda
--   * Azul: (5, 27) — na ilha direita
--
-- __Estratégia:__
-- As minhocas começam em ilhas elevadas com vantagem de altura.
-- O lago central divide o terreno inferior.
mapaIlhas :: Mapa
mapaIlhas =
  [ [Pedra | _ <- [1..34]] ] ++                                                 -- Linha 0: Teto
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..5] ] ++ -- Linhas 1-5: Ar
  [ [if c == 1 || c == 34 then Pedra
     else if (c >= 6 && c <= 10) || (c >= 24 && c <= 28) then Terra              -- Ilhas laterais
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++                                  -- Linhas 6-8: Ilhas
  [ [if c == 1 || c == 34 then Pedra
     else if c >= 16 && c <= 18 then Agua                                        -- Lago central
     else Terra | c <- [1..34]] | _ <- [1..9] ] ++                               -- Linhas 9-17: Terra+Lago
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]    -- Linhas 18-19: Água

-- | Barris do mapa ilhas gémeas.
-- Posicionados nas ilhas e perto do lago.
barrisIlhas :: [Objeto]
barrisIlhas = 
  [ Barril (8, 12) False     -- Na ilha esquerda (ao lado da minhoca verde)
  , Barril (8, 21) False    -- Na ilha direita (ao lado da minhoca azul)
  ]

-- | Minhocas do mapa ilhas gémeas.
-- Cada uma na sua ilha elevada.
--
-- __Verificação de posição:__
--
--   * Minhoca Verde em (5, 7): Linha 5 é Ar, Linha 6 coluna 7 está em [6..10] = Terra ✓
--   * Minhoca Azul em (5, 27): Linha 5 é Ar, Linha 6 coluna 27 está em [24..28] = Terra ✓
--
-- __Munições:__ Jetpack=50, Escavadora=30, Bazuca=15, Mina=10, Dinamite=10
minhocasIlhas :: [Minhoca]
minhocasIlhas = 
  [ Minhoca (Just (5, 7)) (Viva 100) 50 30 15 10 10    -- Verde (ilha esquerda)
  , Minhoca (Just (5, 26)) (Viva 100) 50 30 15 10 10   -- Azul (ilha direita)
  ]

--------------------------------------------------------------------------------
-- * Estrutura e Seleção de Mapas

-- | Estrutura que agrupa todos os componentes de um mapa.
data MapaCompleto = MapaCompleto
  { mapaTerreno  :: Mapa       -- ^ Matriz de terrenos
  , mapaBarris   :: [Objeto]   -- ^ Lista de barris iniciais
  , mapaMinhocas :: [Minhoca]  -- ^ Lista de minhocas iniciais
  , mapaNome     :: String     -- ^ Nome do mapa para exibição
  }

-- | Lista com todos os mapas disponíveis no jogo.
todosOsMapas :: [MapaCompleto]
todosOsMapas =
  [ MapaCompleto mapaClassico barrisClassico minhocasClassico "Clássico"
  , MapaCompleto mapaMontanhas barrisMontanhas minhocasMontanhas "Montanhas"
  , MapaCompleto mapaLago barrisLago minhocasLago "Lago"
  , MapaCompleto mapaPedreira barrisPedreira minhocasPedreira "Pedreira"
  , MapaCompleto mapaIlhas barrisIlhas minhocasIlhas "Ilhas Gémeas"
  ]

-- | Seleciona um mapa pelo índice (com wrap-around).
--
-- >>> mapaNome (selecionarMapa 0)
-- "Clássico"
-- >>> mapaNome (selecionarMapa 5)
-- "Clássico"
selecionarMapa :: Int -> MapaCompleto
selecionarMapa n = todosOsMapas !! (abs n `mod` length todosOsMapas)

-- | Cria um Estado inicial a partir de um MapaCompleto.
criarEstadoDoMapa :: MapaCompleto -> Estado
criarEstadoDoMapa mapa = Estado
  { mapaEstado = mapaTerreno mapa
  , objetosEstado = mapaBarris mapa
  , minhocasEstado = mapaMinhocas mapa
  }


--------------------------------------------------------------------------------
-- * Modo Treino

-- | Cria um Estado de TREINO (só minhoca verde) a partir de um MapaCompleto.
--
-- No modo treino só existe uma minhoca para o jogador praticar.
criarEstadoTreino :: MapaCompleto -> Estado
criarEstadoTreino mapa = Estado
  { mapaEstado = mapaTerreno mapa
  , objetosEstado = mapaBarris mapa
  , minhocasEstado = [minhocaTreino]  -- Só 1 minhoca!
  }
  where
    -- Pega a primeira minhoca (verde) do mapa
    minhocaTreino = case mapaMinhocas mapa of
      (m:_) -> m  -- Usa a posição da primeira minhoca
      [] -> Minhoca (Just (9, 8)) (Viva 100) 50 30 15 10 10  -- Fallback