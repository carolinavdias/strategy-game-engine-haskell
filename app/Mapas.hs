{-|
Module      : Mapas
Description : Coleção de mapas para o jogo 
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

5 mapas diferentes que rodam em sequência - POSIÇÕES CORRETAS!
-}

module Mapas where

import Labs2025

--------------------------------------------------------------------------------
-- * MAPA 1: CLÁSSICO (FLAT SIMPLES)

mapaClassico :: Mapa
mapaClassico = 
  [ [Pedra | _ <- [1..34]] ] ++                                                -- Linha 0
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++ -- Linhas 1-9: AR
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++ -- Linhas 10-17: TERRA
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]  -- Linhas 18-19: ÁGUA

barrisClassico :: [Objeto]
barrisClassico = 
  [ Barril (9, 10) False   -- Linha 9 = última de AR
  , Barril (9, 15) False
  , Barril (9, 20) False
  , Barril (9, 25) False
  ]

minhocasClassico :: [Minhoca]
minhocasClassico = 
  [ Minhoca (Just (9, 6)) (Viva 100) 2 3 5 2 3   -- Linha 9 = última de AR
  , Minhoca (Just (9, 28)) (Viva 100) 2 3 5 2 3
  ]

--------------------------------------------------------------------------------
-- * MAPA 2: MONTANHAS

mapaMontanhas :: Mapa
mapaMontanhas =
  [ [Pedra | _ <- [1..34]] ] ++                                                -- Linha 0
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..5] ] ++ -- Linhas 1-5: AR
  [ [if c == 1 || c == 34 then Pedra 
     else if c >= 5 && c <= 12 then Terra 
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++                                -- Linhas 6-8: Monte esquerdo
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..2] ] ++ -- Linhas 9-10: AR
  [ [if c == 1 || c == 34 then Pedra 
     else if c >= 22 && c <= 29 then Terra 
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++                                -- Linhas 11-13: Monte direito
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..4] ] ++ -- Linhas 14-17: TERRA
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]  -- Linhas 18-19: ÁGUA

barrisMontanhas :: [Objeto]
barrisMontanhas = 
  [ Barril (5, 8) False    -- Linha 5 = última de AR antes do monte esquerdo
  , Barril (5, 26) False   -- Linha 5 = última de AR antes do monte direito
  , Barril (10, 17) False  -- Linha 10 = AR entre montes
  ]

minhocasMontanhas :: [Minhoca]
minhocasMontanhas = 
  [ Minhoca (Just (5, 7)) (Viva 100) 2 3 5 2 3   -- Linha 5 = última de AR
  , Minhoca (Just (5, 27)) (Viva 100) 2 3 5 2 3
  ]

--------------------------------------------------------------------------------
-- * MAPA 3: ILHA CENTRAL

mapaIlha :: Mapa
mapaIlha =
  [ [Pedra | _ <- [1..34]] ] ++                                                -- Linha 0
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..6] ] ++ -- Linhas 1-6: AR
  [ [if c == 1 || c == 34 then Pedra 
     else if c >= 14 && c <= 20 then Terra 
     else Ar | c <- [1..34]] | _ <- [1..4] ] ++                                -- Linhas 7-10: Ilha central
  [ [if c == 1 || c == 34 then Pedra 
     else if (c >= 5 && c <= 9) || (c >= 25 && c <= 29) then Terra 
     else Ar | c <- [1..34]] | _ <- [1..3] ] ++                                -- Linhas 11-13: Plataformas laterais
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..4] ] ++ -- Linhas 14-17: TERRA
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]  -- Linhas 18-19: ÁGUA

barrisIlha :: [Objeto]
barrisIlha = 
  [ Barril (6, 17) False   -- Linha 6 = AR antes da ilha central
  , Barril (10, 7) False   -- Linha 10 = AR nas plataformas
  , Barril (10, 27) False
  ]

minhocasIlha :: [Minhoca]
minhocasIlha = 
  [ Minhoca (Just (10, 6)) (Viva 100) 2 3 5 2 3  -- Linha 10 = AR nas plataformas
  , Minhoca (Just (10, 28)) (Viva 100) 2 3 5 2 3
  ]

--------------------------------------------------------------------------------
-- * MAPA 4: TÚNEIS

mapaTuneis :: Mapa
mapaTuneis =
  [ [Pedra | _ <- [1..34]] ] ++                                                -- Linha 0
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..4] ] ++ -- Linhas 1-4: AR
  [ [if c == 1 || c == 34 then Pedra 
     else if c `mod` 4 == 0 then Ar else Terra | c <- [1..34]] | _ <- [1..6] ] ++ -- Linhas 5-10: Túneis padrão
  [ [if c == 1 || c == 34 then Pedra 
     else if c `mod` 6 == 0 then Ar else Terra | c <- [1..34]] | _ <- [1..5] ] ++ -- Linhas 11-15: Túneis padrão
  [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..2] ] ++ -- Linhas 16-17: TERRA
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]  -- Linhas 18-19: ÁGUA

barrisTuneis :: [Objeto]
barrisTuneis = 
  [ Barril (4, 12) False   -- Linha 4 = última de AR puro
  , Barril (4, 20) False
  , Barril (4, 8) False
  , Barril (4, 28) False
  ]

minhocasTuneis :: [Minhoca]
minhocasTuneis = 
  [ Minhoca (Just (4, 6)) (Viva 100) 2 3 5 2 3   -- Linha 4 = última de AR puro
  , Minhoca (Just (4, 28)) (Viva 100) 2 3 5 2 3
  ]

--------------------------------------------------------------------------------
-- * MAPA 5: ARENA

mapaArena :: Mapa
mapaArena =
  [ [Pedra | _ <- [1..34]] ] ++                                                -- Linha 0
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..8] ] ++ -- Linhas 1-8: AR
  [ [if c == 1 || c == 34 then Pedra 
     else if (c >= 8 && c <= 12) || (c >= 22 && c <= 26) then Terra 
     else Ar | c <- [1..34]] ] ++                                              -- Linha 9: Plataformas
  [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..3] ] ++ -- Linhas 10-12: AR
  [ [if c == 1 || c == 34 then Pedra 
     else if c == 17 then Pedra else Terra | c <- [1..34]] | _ <- [1..5] ] ++ -- Linhas 13-17: TERRA com pilar
  [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]  -- Linhas 18-19: ÁGUA

barrisArena :: [Objeto]
barrisArena = 
  [ Barril (8, 10) False   -- Linha 8 = AR antes das plataformas
  , Barril (8, 24) False
  , Barril (12, 17) False  -- Linha 12 = AR no meio
  ]

minhocasArena :: [Minhoca]
minhocasArena = 
  [ Minhoca (Just (8, 10)) (Viva 100) 2 3 5 2 3  -- Linha 8 = AR nas plataformas
  , Minhoca (Just (8, 24)) (Viva 100) 2 3 5 2 3
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