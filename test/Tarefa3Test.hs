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

-- | Mapa com água 5x5
mapaComAgua :: Mapa
mapaComAgua = 
    [ [Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar, Ar, Ar, Pedra]
    , [Pedra, Agua, Terra, Ar, Pedra]
    , [Pedra, Agua, Terra, Terra, Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- | Mapa pequeno para teste de saída 2x3
mapaPequeno :: Mapa
mapaPequeno = 
    [ [Ar, Ar, Ar]
    , [Ar, Ar, Ar]
    ]

-- | Mapa alto para testar jetpack 8x5
mapaAlto :: Mapa
mapaAlto =
    [ [Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar, Ar, Ar, Pedra]
    , [Pedra, Ar, Ar, Ar, Pedra]
    , [Pedra, Ar, Ar, Ar, Pedra]
    , [Pedra, Ar, Ar, Ar, Pedra]
    , [Pedra, Ar, Ar, Ar, Pedra]
    , [Pedra, Terra, Terra, Terra, Pedra]
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
    -- testes Minhoca

    -- T1: minhoca no ar cai uma posição
      Estado mapaSimples 
      [] [minhocaViva (1, 1)]
    
    -- T2: Minhoca no chão não cai
    , Estado mapaSimples
     [] [minhocaViva (3, 1)]
    
    -- T3: Minhoca viva cai na água e morre
    , Estado mapaComAgua 
    [] [minhocaViva (1, 1)]
    
    -- T4: minhoca morta cai na água (mantém pos e estado)
    , Estado mapaComAgua 
    [] 
    [minhocaMorta (Just (1, 1))]
    
    -- T5: Minhoca sem pos não é afetada
    , Estado mapaSimples 
    [] 
    [minhocaMorta Nothing]
    
    -- T6: minhoca cai fora do mapa (perde pos e morre)
    , Estado mapaPequeno 
    [] [minhocaViva (1, 1)]
    
    -- T7: Minhoca morta na água não morre novamente
    , Estado mapaComAgua 
    [] 
    [minhocaMorta (Just (2, 1))]
    
    -- T8: Minhoca viva sem posição não é afetada
    , Estado mapaSimples 
    [] [Minhoca Nothing (Viva 50) 0 0 0 0 0]
    
    -- T9: minhocas com comportamentos dif
    , Estado mapaComAgua [] 
        [ minhocaViva (1, 1)      -- cai no ar
        , minhocaViva (3, 2)      -- no chão
        , minhocaMorta (Just (1, 2)) -- morta no ar
        ]
    
    -- T10: Minhoca cai muitas pos até água
    , Estado mapaComAgua 
    [] [minhocaViva (0, 1)]
    
    -- teste barril
    
    -- T11: barril no chão mantém-se estável
    , Estado mapaSimples 
    [Barril (3, 2) False] []
    
    -- T12: Barril no ar ativa-se
    , Estado mapaSimples 
    [Barril (1, 1) False] 
    []
    
    -- T13: barril na água ativa-se
    , Estado mapaComAgua 
    [Barril (2, 1) False] []
    
    -- T14: barril ativado explode
    , Estado mapaSimples 
    [Barril (2, 2) True] []
    
    -- T15: Barril explode e ativa outro barril próximo
    , Estado mapaSimples 
        [ Barril (2, 2) True
       
        , Barril (2, 3) False
        ] []
    
    -- T16: barril explode e mata minhoca no centro
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 2)]
    
    -- T17: Barril explode e reduz vida de minhoca próxima
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        
        [minhocaViva (2, 3)]
    
    -- T18: Barril não afeta minhoca morta
    , Estado mapaSimples 
        [Barril (2, 2) True] 
       
        [minhocaMorta (Just (2, 2))]
    
    -- T19: Múltiplos barris em cascata
    , Estado mapaSimples
        [ Barril (2, 2) True
        , Barril (2, 4) False
        , Barril (3, 3) False
        
        ] [minhocaViva (1, 1)]
    
    -- T20: Barril atingido por explosão ativa
    , Estado mapaSimples
        [ Barril (2, 2) False
        , Disparo (2, 4) Oeste Bazuca (Just 0) 0
        ] []
    
    -- Teste Bazuca
    
    -- T21: Bazuca avança uma pos
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 5) 0] []
    
    -- T22: bazuca temp= 0 explode
    , Estado mapaSimples 
    [Disparo (2, 2) Este Bazuca (Just 0) 0] []
    
    -- T23: Bazuca bate em terra e explode
    , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 3) 0] []
    
    -- T24: bazuca decrementa temp 
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 3) 0] 
    []
    
    -- T25: bazuca sem temp continua andar
    , Estado mapaSimples 
    [Disparo (1, 1) Este Bazuca Nothing 0] []
    
    -- T26: Bazuca sai  mapa (return danos vazios)
    , Estado mapaPequeno
     [Disparo (0, 2) Este Bazuca (Just 2) 0] []
    
    -- T27 bazuca explode e afeta muitas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Bazuca (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        , minhocaViva (3, 2)
        ]
    
    -- T28: Bazuca avanca Oeste
    , Estado mapaSimples [Disparo (1, 3) Oeste Bazuca (Just 4) 0] 
    []
    
    -- T29: bazuca avanca Sul
    , Estado mapaSimples 
    [Disparo (1, 2) Sul Bazuca (Just 3) 0] []
    
    -- T30 bazuca avanca Norte
    , Estado mapaSimples [Disparo (3, 2) Norte Bazuca (Just 2) 0] []
    
    -- testes mina

    -- T31: mina sem temp, sem inimigos próximos
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [minhocaViva (1, 1)]
    
    -- T32: mina sem temp, com inimigo próximo ativa
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (1, 1)
        , minhocaViva (3, 3)
        ]
    
    -- T33 Mina  temp decrementa
    , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 2) 0] []
    
    -- T34: mina  temp 0 explode
    , Estado mapaSimples
     [Disparo (3, 2) Norte Mina (Just 0) 0] []
    
    -- T35: Mina ar com Sul cai
    , Estado mapaSimples [Disparo (1, 1) Sul Mina Nothing 0] 
    []
    
    -- T36 mina  chão aponta Norte
    , Estado mapaSimples [Disparo (3, 2) Sul Mina (Just 3) 0] []
    
    -- T37: mina explode e mata minhoca no centro (d= 3)
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Mina (Just 0) 0] 
        [minhocaViva (2, 2)]
    
    -- T38: mina explode com muitas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Mina (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        , minhocaViva (3, 3)
        ]
    
    -- T39: mina n ativa se inimigo longe
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (3, 2)  -- ^dono
        , minhocaViva (0, 0)  -- ^longe
        ]
    
    -- T40: mina cai sobre Terra
    , Estado mapaSimples 
    [Disparo (2, 1) Sul Mina (Just 2) 0] []
    
    -- T41: Mina cai sobre Pedra
    , Estado mapaSimples [Disparo (0, 0) Sul Mina Nothing 0] 
    []
    
    -- T42: mina sai mapa
    , Estado mapaPequeno [Disparo (1, 1) Sul Mina (Just 1) 0] []
    
    -- T43: mina ativa e depois explode
    , Estado mapaSimples 
        [Disparo (3, 2) Norte Mina Nothing 0] 
        [ minhocaViva (1, 1)
        , minhocaViva (4, 2)
        ]
    
    -- T44: mina tempo 1
    , Estado mapaSimples
     [Disparo (3, 2) Norte Mina (Just 1) 0] []
    
    -- T45 mina ar caindo
    , Estado mapaSimples 
    [Disparo (1, 2) Sul Mina (Just 3) 0] []
    
    -- teste dinamite
    
    -- T46: Dinamite temp=  0 explode
    , Estado mapaSimples
     [Disparo (2, 2) Este Dinamite (Just 0) 0] []
    
    -- T47: dinamite sem temp mantém-se
    , Estado mapaSimples 
    [Disparo (1, 1) Este Dinamite Nothing 0] []
    
    -- T48 Dinamite chão para (aponta Norte)
    , Estado mapaSimples 
    [Disparo (3, 2) Este Dinamite (Just 3) 0] []
    
    -- T49: dinamite parábola Este -> Sudeste
    , Estado mapaSimples 
    [Disparo (1, 1) Este Dinamite (Just 5) 0] []
    
    -- T50: Dinamite Norte cai vertical
    , Estado mapaSimples 
    [Disparo (1, 2) Norte Dinamite (Just 5) 0] []
    
    -- T51 dinamite Sul cai vertical
    , Estado mapaSimples 
    [Disparo (1, 2) Sul Dinamite (Just 4) 0] []
    
    -- T52: Dinamite Oeste -> Sudoeste
    , Estado mapaSimples 
    [Disparo (1, 3) Oeste Dinamite (Just 3) 0] []
    
    -- T53: dinamite Nordeste -> Este
    , Estado mapaSimples 
    [Disparo (1, 1) Nordeste Dinamite (Just 4) 0] []
    
    -- T54: Dinamite Sudeste -> Este -> Sul
    , Estado mapaSimples 
    [Disparo (1, 1) Sudeste Dinamite (Just 3) 0] []
    
    -- T55: dinamite Noroeste -> Oeste
    , Estado mapaSimples 
    [Disparo (1, 3) Noroeste Dinamite (Just 2) 0] []
    
    -- T56: Dinamite Sudoeste -> Oeste -> Sul
    , Estado mapaSimples 
    [Disparo (1, 3) Sudoeste Dinamite (Just 3) 0] []
    
    -- T57 dinamite explode (d= 7) grande área
    , Estado mapaSimples 
        [Disparo (2, 2) Norte Dinamite (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (3, 3)
        , minhocaViva (1, 1)
        ]
    
    -- T58: dinamite sai mapa
    , Estado mapaPequeno 
    [Disparo (0, 2) Este Dinamite (Just 1) 0] []
    
    -- T59: Dinamite no chão sobre Terra
    , Estado mapaSimples [Disparo (3, 1) Sul Dinamite (Just 2) 0] []
    
    -- T60: dinamite no chão sobre Pedra
    , Estado mapaSimples
     [Disparo (4, 2) Este Dinamite (Just 1) 0] []
    
    -- teste para explosao e dano
    
    -- T61 Explosão mata minhoca no centro
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 2)]
    
    -- T62: Explosão reduz vida (n mata)
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (2, 4)]
    
    -- T63: Explosão afeta muitas minhocas
    , Estado mapaSimples 
        [Disparo (2, 2) Este Bazuca (Just 0) 0] 
        [ minhocaViva (2, 2)
        , minhocaViva (2, 3)
        , minhocaViva (3, 2)
        ]
    
    -- T64 explosão n afeta minhoca morta
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaMorta (Just (2, 2))]
    
    -- T65: explosão afeta terreno 
    , Estado mapaSimples [Barril (2, 2) True] []
    
    -- T66: Explosão em pos cardeal
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [ minhocaViva (2, 3)  -- cardeal
        , minhocaViva (3, 3)  -- diagonal
        ]
    
    -- T67: Explosão em pos diagonal
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaViva (3, 3)]
    
    -- T68 minhoca com vida =  0
    , Estado mapaSimples 
        [Barril (2, 2) True] 
        [minhocaComVida (2, 2) 50]
    
    -- T69: Dano zero n afeta minhoca
    , Estado mapaSimples 
        [Disparo (0, 0) Norte Bazuca (Just 0) 0] 
        [minhocaViva (4, 4)]
    
    -- T70 explosão afeta apenas objetos próx
    , Estado mapaSimples
        [ Barril (2, 2) True
        , Barril (4, 4) False  -- longe
        ] []

    -- T71 jetpack
    , Estado mapaAlto 
        [Disparo (2, 2) Este Jetpack Nothing 0] 
        [minhocaViva (2, 2)]

    -- T72 Escavadora avança em Ar
    , Estado mapaSimples 
        [Disparo (1, 1) Este Escavadora Nothing 0] 
        [minhocaViva (3, 3)]
    ]

-- * MAIN

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3