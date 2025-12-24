-- testes da tarefa4
module Main where

import Labs2025
import Tarefa4
import Magic

--------------------------------------------------------------------------------
-- * Definição de testes

-- | Testes do grupo para a Tarefa 4
testesTarefa4 :: [Estado]
testesTarefa4 = 
    [ testeDestruicaoTerra
    , testeComInimigos
    , testeMisto
    , testeMapaGrande
    ]

--------------------------------------------------------------------------------
-- * Mapas auxiliares

-- | Mapa simples para testes básicos
mapaSimples :: Mapa
mapaSimples = 
    [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Terra, Terra, Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Terra, Terra, Terra, Terra, Terra, Ar,    Ar,    Pedra]
    , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Ar,    Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- | Mapa maior com mais oportunidades de pontuação
mapaGrande :: Mapa
mapaGrande =
    [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Terra, Terra, Terra, Ar,    Ar,    Ar,    Ar,    Terra, Terra, Terra, Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Terra, Terra, Terra, Terra, Ar,    Ar,    Terra, Terra, Terra, Terra, Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Ar,    Pedra]
    , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Pedra]
    , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

--------------------------------------------------------------------------------
-- * Criação de minhocas

-- | Cria uma minhoca viva numa posição com armas padrão
minhocaViva :: Posicao -> Minhoca
minhocaViva pos = Minhoca
    { posicaoMinhoca = Just pos
    , vidaMinhoca = Viva 100
    , jetpackMinhoca = 2
    , escavadoraMinhoca = 3
    , bazucaMinhoca = 5
    , minaMinhoca = 2
    , dinamiteMinhoca = 3
    }

--------------------------------------------------------------------------------
-- * Estados de teste

-- | Teste 1: foco em destruição de Terra
-- Minhoca sozinha com muita Terra disponível para destruir
testeDestruicaoTerra :: Estado
testeDestruicaoTerra = Estado mapaSimples []
    [ minhocaViva (1, 5)
    ]

-- | Teste 2: cenário com inimigos para atacar
-- Uma minhoca nossa e duas inimigas
testeComInimigos :: Estado
testeComInimigos = Estado mapaSimples []
    [ minhocaViva (1, 1)
    , minhocaViva (1, 7)
    , minhocaViva (3, 5)
    ]

-- | Teste 3: cenário misto (Terra e inimigos)
-- Testa capacidade de equilibrar destruição e combate
testeMisto :: Estado
testeMisto = Estado mapaSimples []
    [ minhocaViva (1, 2)
    , minhocaViva (3, 7)
    ]

-- | Teste 4: mapa grande com muitas oportunidades
-- Testa otimização em mapas maiores
testeMapaGrande :: Estado
testeMapaGrande = Estado mapaGrande []
    [ minhocaViva (1, 7)
    , minhocaViva (2, 2)
    , minhocaViva (3, 12)
    ]

--------------------------------------------------------------------------------
-- * Execução dos testes

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4