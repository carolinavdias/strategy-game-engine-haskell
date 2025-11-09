import Labs2025
import Tarefa3
import Magic



-- * HELPER FUNCTIONS

-- | Mapa simples para testes (5x5)
mapaSimples :: Mapa
mapaSimples = 
    [ [Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Terra, Ar,    Pedra]
    , [Pedra, Terra, Terra, Terra, Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- | Mapa com água
mapaComAgua :: Mapa
mapaComAgua = 
    [ [Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Agua,  Terra, Ar,    Pedra]
    , [Pedra, Agua,  Terra, Terra, Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- | Minhoca viva padrão
minhocaViva :: Posicao -> Minhoca
minhocaViva pos = Minhoca
    { posicaoMinhoca = Just pos
    , vidaMinhoca = Viva 100
    , jetpackMinhoca = 1
    , escavadoraMinhoca = 1
    , bazucaMinhoca = 1
    , minaMinhoca = 1
    , dinamiteMinhoca = 1
    }

-- | Minhoca morta
minhocaMorta :: Maybe Posicao -> Minhoca
minhocaMorta pos = Minhoca
    { posicaoMinhoca = pos
    , vidaMinhoca = Morta
    , jetpackMinhoca = 0
    , escavadoraMinhoca = 0
    , bazucaMinhoca = 0
    , minaMinhoca = 0
    , dinamiteMinhoca = 0
    }

-- * TESTES DA TAREFA 3

testesTarefa3 :: [Estado]
testesTarefa3 = 
    [ 
    -- MINHOCAS (5 testes)
    
    -- Teste 1: Minhoca no ar cai
      Estado mapaSimples [] [minhocaViva (1, 1)]
    
    -- Teste 2: Minhoca no chão não cai
    , Estado mapaSimples [] [minhocaViva (3, 1)]
    
    -- Teste 3: Minhoca viva cai na água e morre
    , Estado mapaComAgua [] [minhocaViva (1, 1)]
    
    -- Teste 4: Minhoca morta cai na água
    , Estado mapaComAgua [] [minhocaMorta (Just (1, 1))]
    
    -- Teste 5: Minhoca sem posição
    , Estado mapaSimples [] [minhocaMorta Nothing]
    
    -- BARRIL (5 testes)
    
    -- Teste 6: Barril no chão mantém-se
    , Estado mapaSimples [Barril (3, 2) False] []
    
    -- Teste 7: Barril no ar ativa-se
    , Estado mapaSimples [Barril (1, 1) False] []
    
    -- Teste 8: Barril na água ativa-se
    , Estado mapaComAgua [Barril (2, 1) False] []
    
    -- Teste 9: Barril ativado explode
    , Estado mapaSimples [Barril (2, 2) True] []
    
    -- Teste 10: Barril explode e ativa outro barril próximo
    , Estado mapaSimples 
        [ Barril (2, 2) True
        , Barril (2, 3) False
        ] []
    
    -- BAZUCA (5 testes)
    
    -- Teste 11: Bazuca avança
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 5) 0] []
    
    -- Teste 12: Bazuca com tempo 0 explode
    , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 0) 0] []
    
    -- Teste 13: Bazuca em terra explode
    , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 3) 0] []
    
    -- Teste 14: Bazuca decrementa tempo
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 3) 0] []
    
    -- Teste 15: Bazuca sem tempo
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca Nothing 0] []
    
    -- MINA (5 testes)
    
    -- Teste 16: Mina sem tempo sem inimigos
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [minhocaViva (1, 1)]
    
    -- Teste 17: Mina sem tempo com inimigo ativa
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (1, 1)
        , minhocaViva (3, 3)
        ]
    
    -- Teste 18: Mina com tempo decrementa
    , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 2) 0] []
    
    -- Teste 19: Mina com tempo 0 explode
    , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 0) 0] []
    
    -- Teste 20: Mina no ar com Sul cai
    , Estado mapaSimples [Disparo (1, 1) Sul Mina Nothing 0] []
    
    -- DINAMITE (5 testes)
    
    -- Teste 21: Dinamite tempo 0 explode
    , Estado mapaSimples [Disparo (2, 2) Este Dinamite (Just 0) 0] []
    
    -- Teste 22: Dinamite sem tempo mantém-se
    , Estado mapaSimples [Disparo (1, 1) Este Dinamite Nothing 0] []
    
    -- Teste 23: Dinamite no chão para
    , Estado mapaSimples [Disparo (3, 2) Este Dinamite (Just 3) 0] []
    
    -- Teste 24: Dinamite parábola Este->Sudeste
    , Estado mapaSimples [Disparo (1, 1) Este Dinamite (Just 5) 0] []
    
    -- Teste 25: Dinamite Norte cai vertical
    , Estado mapaSimples [Disparo (1, 2) Norte Dinamite (Just 5) 0] []
    
    -- EXPLOSÕES (5 testes)
    
    -- Teste 26: Explosão mata minhoca centro
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 2)]
    
    -- Teste 27: Explosão reduz vida
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 3)]
    
    -- Teste 28: Explosão múltiplas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Este Bazuca (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        ]
    
    -- Teste 29: Mina explode (diâmetro 3)
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Mina (Just 0) 0] 
        [minhocaViva (2, 2)]
    
    -- Teste 30: Explosão não mata morta
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaMorta (Just (2, 2))]
    ]

-- * MAIN

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = do
    let teste = testesTarefa3 !! 9
    putStrLn "MAPA:"
    print (mapaEstado teste)
    putStrLn "\nOBJETOS:"
    print (objetosEstado teste)
    runFeedback =<< dataTarefa3