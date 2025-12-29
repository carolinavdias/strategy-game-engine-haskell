{-|
Module      : Mapas
Description : Coleção de mapas para o jogo (VERSÃO SIMPLES)
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

5 mapas diferentes que rodam em sequência - SEM dependências extras!
-}

module Mapas where

import Labs2025

--------------------------------------------------------------------------------
-- * COLEÇÃO DE MAPAS

-- | Mapa 1: Clássico
mapaClassico :: Mapa
mapaClassico = 
  [ [Pedra | _ <- [1..34]] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

barrisClassico :: [Objeto]
barrisClassico = 
  [ Barril (9, 8) False
  , Barril (9, 13) False
  , Barril (9, 17) False
  , Barril (9, 24) False
  ]

minhocasClassico :: [Minhoca]
minhocasClassico = 
  [ Minhoca (Just (8, 6)) (Viva 100) 2 3 5 2 3
  , Minhoca (Just (8, 26)) (Viva 100) 2 3 5 2 3
  ]

-- | Mapa 2: Montanhas
mapaMontanhas :: Mapa
mapaMontanhas =
  [ [Pedra | _ <- [1..34]] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..5] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if c >= 5 && c <= 12 then Terra 
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..2] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if c >= 22 && c <= 29 then Terra 
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..5] ] ++
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

barrisMontanhas :: [Objeto]
barrisMontanhas = 
  [ Barril (8, 8) False
  , Barril (8, 26) False
  , Barril (13, 17) False
  ]

minhocasMontanhas :: [Minhoca]
minhocasMontanhas = 
  [ Minhoca (Just (7, 7)) (Viva 100) 2 3 5 2 3
  , Minhoca (Just (7, 27)) (Viva 100) 2 3 5 2 3
  ]

-- | Mapa 3: Ilha Central
mapaIlha :: Mapa
mapaIlha =
  [ [Pedra | _ <- [1..34]] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..6] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if c >= 14 && c <= 20 then Terra 
     else Ar | c <- [1..34]] | _ <- [1..4] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if (c >= 5 && c <= 9) || (c >= 25 && c <= 29) then Terra 
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..5] ] ++
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

barrisIlha :: [Objeto]
barrisIlha = 
  [ Barril (8, 17) False
  , Barril (10, 7) False
  , Barril (10, 27) False
  , Barril (13, 17) False
  ]

minhocasIlha :: [Minhoca]
minhocasIlha = 
  [ Minhoca (Just (9, 6)) (Viva 100) 2 3 5 2 3
  , Minhoca (Just (9, 28)) (Viva 100) 2 3 5 2 3
  ]

-- | Mapa 4: Túneis
mapaTuneis :: Mapa
mapaTuneis =
  [ [Pedra | _ <- [1..34]] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..4] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if c `mod` 4 == 0 then Ar else Terra | c <- [1..34]] | _ <- [1..6] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if c `mod` 6 == 0 then Ar else Terra | c <- [1..34]] | _ <- [1..6] ] ++
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..2] ] ++
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

barrisTuneis :: [Objeto]
barrisTuneis = 
  [ Barril (6, 12) False
  , Barril (6, 20) False
  , Barril (10, 8) False
  , Barril (10, 26) False
  ]

minhocasTuneis :: [Minhoca]
minhocasTuneis = 
  [ Minhoca (Just (5, 6)) (Viva 100) 2 3 5 2 3
  , Minhoca (Just (5, 28)) (Viva 100) 2 3 5 2 3
  ]

-- | Mapa 5: Arena
mapaArena :: Mapa
mapaArena =
  [ [Pedra | _ <- [1..34]] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..8] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if (c >= 8 && c <= 12) || (c >= 22 && c <= 26) then Terra 
     else Ar | c <- [1..34]] ] ++
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..3] ] ++
  [ [if c == 1 || c == 34 then Pedra 
     else if c == 17 then Pedra else Terra | c <- [1..34]] | _ <- [1..5] ] ++
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

barrisArena :: [Objeto]
barrisArena = 
  [ Barril (8, 10) False
  , Barril (8, 24) False
  , Barril (13, 17) False
  ]

minhocasArena :: [Minhoca]
minhocasArena = 
  [ Minhoca (Just (7, 10)) (Viva 100) 2 3 5 2 3
  , Minhoca (Just (7, 24)) (Viva 100) 2 3 5 2 3
  ]

--------------------------------------------------------------------------------
-- * ESTRUTURA E SELEÇÃO

data MapaCompleto = MapaCompleto
  { mapaTerreno :: Mapa
  , mapaBarris :: [Objeto]
  , mapaMinhocas :: [Minhoca]
  , mapaNome :: String
  }

-- | TODOS os mapas disponíveis
todosOsMapas :: [MapaCompleto]
todosOsMapas =
  [ MapaCompleto mapaClassico barrisClassico minhocasClassico "Clássico"
  , MapaCompleto mapaMontanhas barrisMontanhas minhocasMontanhas "Montanhas"
  , MapaCompleto mapaIlha barrisIlha minhocasIlha "Ilha Central"
  , MapaCompleto mapaTuneis barrisTuneis minhocasTuneis "Túneis"
  , MapaCompleto mapaArena barrisArena minhocasArena "Arena"
  ]

-- | Seleciona mapa por índice (0-4)
selecionarMapa :: Int -> MapaCompleto
selecionarMapa n = todosOsMapas !! (abs n `mod` length todosOsMapas)

-- | Converte MapaCompleto em Estado do jogo
criarEstadoDoMapa :: MapaCompleto -> Estado
criarEstadoDoMapa mapa = Estado
  { mapaEstado = mapaTerreno mapa
  , objetosEstado = mapaBarris mapa
  , minhocasEstado = mapaMinhocas mapa
  }