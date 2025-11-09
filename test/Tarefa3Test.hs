import Labs2025
import Tarefa3
import Magic

-- * MAPAS AUXILIARES

-- | Mapa simples 5x5
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

-- | Mapa pequeno para teste de saída
mapaPequeno :: Mapa
mapaPequeno = 
    [ [Ar, Ar, Ar]
    , [Ar, Ar, Ar]
    ]


-- * HELPERS

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

-- | Minhoca com vida específica
minhocaComVida :: Posicao -> Int -> Minhoca
minhocaComVida pos vida = Minhoca
    { posicaoMinhoca = Just pos
    , vidaMinhoca = Viva vida
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
    -- ============================================
    -- TESTES DE MINHOCAS (10 testes)
    -- ============================================
    
    -- Teste 1: Minhoca no ar cai uma posição
      Estado mapaSimples [] [minhocaViva (1, 1)]
    
    -- Teste 2: Minhoca no chão não cai
    , Estado mapaSimples [] [minhocaViva (3, 1)]
    
    -- Teste 3: Minhoca viva cai na água e morre
    , Estado mapaComAgua [] [minhocaViva (1, 1)]
    
    -- Teste 4: Minhoca morta cai na água (mantém posição e estado)
    , Estado mapaComAgua [] [minhocaMorta (Just (1, 1))]
    
    -- Teste 5: Minhoca sem posição não é afetada
    , Estado mapaSimples [] [minhocaMorta Nothing]
    
    -- Teste 6: Minhoca cai fora do mapa (perde posição e morre)
    , Estado mapaPequeno [] [minhocaViva (1, 1)]
    
    -- Teste 7: Minhoca morta na água não morre novamente
    , Estado mapaComAgua [] [minhocaMorta (Just (2, 1))]
    
    -- Teste 8: Minhoca viva sem posição não é afetada
    , Estado mapaSimples [] [Minhoca Nothing (Viva 50) 0 0 0 0 0]
    
    -- Teste 9: Múltiplas minhocas com comportamentos diferentes
    , Estado mapaComAgua [] 
        [ minhocaViva (1, 1)      -- cai no ar
        , minhocaViva (3, 2)      -- no chão
        , minhocaMorta (Just (1, 2)) -- morta no ar
        ]
    
    -- Teste 10: Minhoca cai múltiplas posições até água
    , Estado mapaComAgua [] [minhocaViva (0, 1)]
    
    -- ============================================
    -- TESTES DE BARRIL (10 testes)
    -- ============================================
    
    -- Teste 11: Barril no chão mantém-se estável
    , Estado mapaSimples [Barril (3, 2) False] []
    
    -- Teste 12: Barril no ar ativa-se
    , Estado mapaSimples [Barril (1, 1) False] []
    
    -- Teste 13: Barril na água ativa-se
    , Estado mapaComAgua [Barril (2, 1) False] []
    
    -- Teste 14: Barril ativado explode
    , Estado mapaSimples [Barril (2, 2) True] []
    
    -- Teste 15: Barril explode e ativa outro barril próximo
    , Estado mapaSimples 
        [ Barril (2, 2) True
        , Barril (2, 3) False
        ] []
    
    -- Teste 16: Barril explode e mata minhoca no centro
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 2)]
    
    -- Teste 17: Barril explode e reduz vida de minhoca próxima
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 3)]
    
    -- Teste 18: Barril não afeta minhoca morta
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaMorta (Just (2, 2))]
    
    -- Teste 19: Múltiplos barris em cascata
    , Estado mapaSimples
        [ Barril (2, 2) True
        , Barril (2, 4) False
        , Barril (3, 3) False
        ] [minhocaViva (1, 1)]
    
    -- Teste 20: Barril atingido por explosão ativa
    , Estado mapaSimples
        [ Barril (2, 2) False
        , Disparo (2, 4) Oeste Bazuca (Just 0) 0
        ] []
    
    -- ============================================
    -- TESTES DE BAZUCA (10 testes)
    -- ============================================
    
    -- Teste 21: Bazuca avança uma posição
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 5) 0] []
    
    -- Teste 22: Bazuca com tempo 0 explode
    , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 0) 0] []
    
    -- Teste 23: Bazuca bate em terra e explode
    , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 3) 0] []
    
    -- Teste 24: Bazuca decrementa tempo (mantém o mesmo)
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 3) 0] []
    
    -- Teste 25: Bazuca sem tempo continua avançando
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca Nothing 0] []
    
    -- Teste 26: Bazuca sai do mapa (retorna danos vazios)
    , Estado mapaPequeno [Disparo (0, 2) Este Bazuca (Just 2) 0] []
    
    -- Teste 27: Bazuca explode e afeta múltiplas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Bazuca (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        , minhocaViva (3, 2)
        ]
    
    -- Teste 28: Bazuca avança Oeste
    , Estado mapaSimples [Disparo (1, 3) Oeste Bazuca (Just 4) 0] []
    
    -- Teste 29: Bazuca avança Sul
    , Estado mapaSimples [Disparo (1, 2) Sul Bazuca (Just 3) 0] []
    
    -- Teste 30: Bazuca avança Norte
    , Estado mapaSimples [Disparo (3, 2) Norte Bazuca (Just 2) 0] []
    
    -- ============================================
    -- TESTES DE MINA (15 testes)
    -- ============================================
    
    -- Teste 31: Mina sem tempo, sem inimigos próximos
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [minhocaViva (1, 1)]
    
    -- Teste 32: Mina sem tempo, com inimigo próximo ativa
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (1, 1)
        , minhocaViva (3, 3)
        ]
    
    -- Teste 33: Mina com tempo decrementa
    , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 2) 0] []
    
    -- Teste 34: Mina com tempo 0 explode
    , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 0) 0] []
    
    -- Teste 35: Mina no ar com Sul cai
    , Estado mapaSimples [Disparo (1, 1) Sul Mina Nothing 0] []
    
    -- Teste 36: Mina no chão aponta Norte
    , Estado mapaSimples [Disparo (3, 2) Sul Mina (Just 3) 0] []
    
    -- Teste 37: Mina explode e mata minhoca no centro (diâmetro 3)
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Mina (Just 0) 0] 
        [minhocaViva (2, 2)]
    
    -- Teste 38: Mina explode com múltiplas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Mina (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        , minhocaViva (3, 3)
        ]
    
    -- Teste 39: Mina não ativa se inimigo longe
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (3, 2)  -- dono
        , minhocaViva (0, 0)  -- longe
        ]
    
    -- Teste 40: Mina cai sobre Terra
    , Estado mapaSimples [Disparo (2, 1) Sul Mina (Just 2) 0] []
    
    -- Teste 41: Mina cai sobre Pedra
    , Estado mapaSimples [Disparo (0, 0) Sul Mina Nothing 0] []
    
    -- Teste 42: Mina sai do mapa
    , Estado mapaPequeno [Disparo (1, 1) Sul Mina (Just 1) 0] []
    
    -- Teste 43: Mina ativa e depois explode
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (1, 1)
        , minhocaViva (4, 2)
        ]
    
    -- Teste 44: Mina com tempo 1
    , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 1) 0] []
    
    -- Teste 45: Mina no ar caindo
    , Estado mapaSimples [Disparo (1, 2) Sul Mina (Just 3) 0] []
    
    -- ============================================
    -- TESTES DE DINAMITE (15 testes)
    -- ============================================
    
    -- Teste 46: Dinamite com tempo 0 explode
    , Estado mapaSimples [Disparo (2, 2) Este Dinamite (Just 0) 0] []
    
    -- Teste 47: Dinamite sem tempo mantém-se
    , Estado mapaSimples [Disparo (1, 1) Este Dinamite Nothing 0] []
    
    -- Teste 48: Dinamite no chão para (aponta Norte)
    , Estado mapaSimples [Disparo (3, 2) Este Dinamite (Just 3) 0] []
    
    -- Teste 49: Dinamite parábola Este -> Sudeste
    , Estado mapaSimples [Disparo (1, 1) Este Dinamite (Just 5) 0] []
    
    -- Teste 50: Dinamite Norte cai vertical
    , Estado mapaSimples [Disparo (1, 2) Norte Dinamite (Just 5) 0] []
    
    -- Teste 51: Dinamite Sul cai vertical
    , Estado mapaSimples [Disparo (1, 2) Sul Dinamite (Just 4) 0] []
    
    -- Teste 52: Dinamite Oeste -> Sudoeste
    , Estado mapaSimples [Disparo (1, 3) Oeste Dinamite (Just 3) 0] []
    
    -- Teste 53: Dinamite Nordeste -> Este
    , Estado mapaSimples [Disparo (1, 1) Nordeste Dinamite (Just 4) 0] []
    
    -- Teste 54: Dinamite Sudeste -> Este -> Sul
    , Estado mapaSimples [Disparo (1, 1) Sudeste Dinamite (Just 3) 0] []
    
    -- Teste 55: Dinamite Noroeste -> Oeste
    , Estado mapaSimples [Disparo (1, 3) Noroeste Dinamite (Just 2) 0] []
    
    -- Teste 56: Dinamite Sudoeste -> Oeste -> Sul
    , Estado mapaSimples [Disparo (1, 3) Sudoeste Dinamite (Just 3) 0] []
    
    -- Teste 57: Dinamite explode (diâmetro 7) grande área
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Dinamite (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (3, 3)
        , minhocaViva (1, 1)
        ]
    
    -- Teste 58: Dinamite sai do mapa
    , Estado mapaPequeno [Disparo (0, 2) Este Dinamite (Just 1) 0] []
    
    -- Teste 59: Dinamite no chão sobre Terra
    , Estado mapaSimples [Disparo (3, 1) Sul Dinamite (Just 2) 0] []
    
    -- Teste 60: Dinamite no chão sobre Pedra
    , Estado mapaSimples [Disparo (4, 2) Este Dinamite (Just 1) 0] []
    
    -- ============================================
    -- TESTES DE EXPLOSÕES E DANOS (10 testes)
    -- ============================================
    
    -- Teste 61: Explosão mata minhoca no centro
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 2)]
    
    -- Teste 62: Explosão reduz vida (não mata)
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 4)]
    
    -- Teste 63: Explosão afeta múltiplas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Este Bazuca (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        , minhocaViva (3, 2)
        ]
    
    -- Teste 64: Explosão não afeta minhoca morta
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaMorta (Just (2, 2))]
    
    -- Teste 65: Explosão afeta terreno (destrói Terra)
    , Estado mapaSimples [Barril (2, 2) True] []
    
    -- Teste 66: Explosão em posição cardeal
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [ minhocaViva (2, 3)  -- cardeal
        , minhocaViva (3, 3)  -- diagonal
        ]
    
    -- Teste 67: Explosão em posição diagonal
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (3, 3)]
    
    -- Teste 68: Minhoca com vida reduzida a exatamente 0
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaComVida (2, 2) 50]
    
    -- Teste 69: Dano zero não afeta minhoca
    , Estado mapaSimples 
        [Disparo (0, 0) Norte Bazuca (Just 0) 0] 
        [minhocaViva (4, 4)]
    
    -- Teste 70: Explosão afeta apenas objetos próximos
    , Estado mapaSimples
        [ Barril (2, 2) True
        , Barril (4, 4) False  -- longe
        ] []
    ]

-- * MAIN

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3