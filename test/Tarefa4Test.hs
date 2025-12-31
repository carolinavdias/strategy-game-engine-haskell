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
    [ testeCombateBasico
    , testeCombate2v2
    , testeComBarris
    , testeDestruicaoTerra
    , testeMapaGrande
    , testeAguaProxima
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

-- | Mapa com água para testar evitar perigos
mapaComAgua :: Mapa
mapaComAgua =
    [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Agua,  Agua,  Terra, Terra, Agua,  Agua,  Ar,    Pedra]
    , [Pedra, Ar,    Terra, Terra, Terra, Terra, Terra, Ar,    Ar,    Pedra]
    , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Ar,    Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
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

-- | Cria uma minhoca com armas limitadas
minhocaSemArmas :: Posicao -> Minhoca
minhocaSemArmas pos = Minhoca
    { posicaoMinhoca = Just pos
    , vidaMinhoca = Viva 100
    , jetpackMinhoca = 0
    , escavadoraMinhoca = 0
    , bazucaMinhoca = 1
    , minaMinhoca = 0
    , dinamiteMinhoca = 0
    }

--------------------------------------------------------------------------------
-- * Estados de teste

-- | Teste 1: Combate básico 1v1
-- Minhoca 0 (BOT) vs Minhoca 1 (INIMIGO)
testeCombateBasico :: Estado
testeCombateBasico = Estado mapaSimples []
    [ minhocaViva (1, 2)  -- índice 0 (par) - BOT
    , minhocaViva (1, 7)  -- índice 1 (ímpar) - INIMIGO
    ]

-- | Teste 2: Combate 2v2
-- Minhocas 0,2 (BOT) vs Minhocas 1,3 (INIMIGOS)
testeCombate2v2 :: Estado
testeCombate2v2 = Estado mapaSimples []
    [ minhocaViva (1, 2)  -- índice 0 (par) - BOT
    , minhocaViva (3, 7)  -- índice 1 (ímpar) - INIMIGO
    , minhocaViva (1, 8)  -- índice 2 (par) - BOT (aliado)
    , minhocaViva (3, 2)  -- índice 3 (ímpar) - INIMIGO
    ]

-- | Teste 3: Cenário com barris (teste de segurança)
-- Bot deve evitar explodir barris próximos de si mesmo
testeComBarris :: Estado
testeComBarris = Estado mapaSimples 
    [ Barril (2, 4) False
    , Barril (3, 6) False
    ]
    [ minhocaViva (2, 2)  -- índice 0 (par) - BOT perto de barril
    , minhocaViva (2, 7)  -- índice 1 (ímpar) - INIMIGO também perto
    ]

-- | Teste 4: Destruição de terra
-- Bot deve focar em destruir terra quando não há pressão de combate
testeDestruicaoTerra :: Estado
testeDestruicaoTerra = Estado mapaSimples []
    [ minhocaViva (1, 5)  -- índice 0 (par) - BOT no centro do mapa
    , minhocaSemArmas (4, 8)  -- índice 1 (ímpar) - INIMIGO longe e fraco
    ]

-- | Teste 5: Mapa grande com múltiplas oportunidades
-- Testa capacidade de otimização em espaços maiores
testeMapaGrande :: Estado
testeMapaGrande = Estado mapaGrande []
    [ minhocaViva (1, 7)   -- índice 0 (par) - BOT
    , minhocaViva (2, 2)   -- índice 1 (ímpar) - INIMIGO
    , minhocaViva (3, 12)  -- índice 2 (par) - BOT (aliado)
    , minhocaViva (4, 8)   -- índice 3 (ímpar) - INIMIGO
    ]

-- | Teste 6: Mapa com água próxima
-- Bot deve evitar cair em água
testeAguaProxima :: Estado
testeAguaProxima = Estado mapaComAgua []
    [ minhocaViva (1, 4)  -- índice 0 (par) - BOT cercado por água
    , minhocaViva (3, 7)  -- índice 1 (ímpar) - INIMIGO em posição segura
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