--ficheiro de testes tarefa 1 :)
import Labs2025
import Tarefa1
import Magic

-- *FUNÇÕES AUXILIARES

-- > mapaSimples (3X3)
mapaSimples :: Mapa --Usado em testes que requerem mais espaço para múltiplos objetos
mapaSimples = 
    [ [Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Pedra]
    , [Pedra, Pedra, Pedra]
    ]

-- > mapa4x4
mapa4x4 :: Mapa --Usado em testes que requerem mais espaço para múltiplos objetos.
mapa4x4 = 
    [ [Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Ar,    Pedra]
    , [Pedra, Pedra, Pedra, Pedra]
    ]

-- mapa simples com uma célula de água (usado para testar minhocas na água)
mapaComAgua :: Mapa
mapaComAgua =
    [ [Ar, Ar, Ar]
    , [Ar, Agua, Ar]
    , [Ar, Terra, Ar]
    ]

-- >> minhocaViva (1, 1)
-- Minhoca {posicaoMinhoca = Just (1,1), vidaMinhoca = Viva 100, ...}
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

-- >> minhoca morta
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


-- >>>> TESTES DA TAREFA 1! <<<<

testesTarefa1 :: [Estado]
testesTarefa1 = 
    [ 
    ---------------------------------------------------------------------------------
    -- MAPA (5 testes)
    ---------------------------------------------------------------------------------
    -- T1: mapa vazio
      Estado [] [] []
    
    -- T2: linhas com tamanhos diferentes
    , Estado 
        [ [Terra, Terra, Terra]
        , [Terra, Ar]
        , [Terra, Terra, Terra]
        ] [] []
    
    -- T3: mapa válido simples
    , Estado mapaSimples [] []
    
    -- T4: mapa com água
    , Estado mapaComAgua [] []
    
    -- T5: mapa com linha vazia
    , Estado [[]] [] []
    
    ---------------------------------------------------------------------------------
    -- BARRIL (8 testes)
    ---------------------------------------------------------------------------------
    -- T6: baril posição inválida (fora)
    , Estado mapaSimples [Barril (10, 10) False] []
    
    -- T7: barril sobre Terra (opaco)
    , Estado mapaSimples [Barril (0, 0) False] []
    
    -- T8: barril sobre minhoca
    , Estado mapaSimples [Barril (1, 1) False] [minhocaViva (1, 1)]
    
    -- T9: barris duplicados
    , Estado mapaSimples [Barril (1, 1) False, Barril (1, 1) True] []
    
    -- T10: barril válido em Ar
    , Estado mapaSimples [Barril (1, 1) False] []
    
    -- T11: múltiplos barris válidos
    , Estado mapa4x4 [Barril (1, 1) False, Barril (2, 2) True] []
    
    -- T12: barril em posição negativa (linha)
    , Estado mapaSimples [Barril (-1, 1) False] []
    
    -- T13: barril em posição negativa (coluna)
    , Estado mapaSimples [Barril (1, -1) False] []
    
    ---------------------------------------------------------------------------------
    -- DISPAROS- JETPACK/ESCAVADORA (6 testes)
    ---------------------------------------------------------------------------------
    -- T14: jetpack válido
    , Estado mapaSimples [Disparo (1, 1) Norte Jetpack Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 5 0 0 0 0]
    
    -- T15: jetpack com tempo (inválido)
    , Estado mapaSimples [Disparo (1, 1) Este Jetpack (Just 3) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 10 0 0 0 0]
    
    -- T16: jetpack sem munições
    , Estado mapaSimples [Disparo (1, 1) Sul Jetpack Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 0]
    
    -- T17: escavadora válida
    , Estado mapaSimples [Disparo (1, 1) Norte Escavadora Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 5 0 0 0]
    
    -- T18: escavadora com tempo (inválido)
    , Estado mapaSimples [Disparo (1, 1) Oeste Escavadora (Just 2) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 10 0 0 0]
    
    -- T19: escavadora sem munições
    , Estado mapaSimples [Disparo (1, 1) Este Escavadora Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 0]
    
    ---------------------------------------------------------------------------------
    -- BAZUCA (12 testes)
    ---------------------------------------------------------------------------------
    -- T20: bazuca sem tempo (válido)
    , Estado mapaSimples [Disparo (1, 1) Norte Bazuca Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 5 0 0]
    
    -- T21: bazuca com tempo (inválido)
    , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 5) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 3 0 0]
    
    -- T22: bazuca perfurando norte (vem de sul)
    , Estado
        [ [Terra, Terra, Terra]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ] [Disparo (0, 1) Sul Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T23: bazuca perfurando sul (vem de norte)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Terra, Ar]
        ] [Disparo (2, 1) Norte Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T24: bazuca perfurando este (vem de oeste)
    , Estado
        [ [Terra, Ar, Ar]
        , [Terra, Ar, Ar]
        ] [Disparo (0, 0) Oeste Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T25: bazuca perfurando oeste (vem de este)
    , Estado
        [ [Ar, Ar, Terra]
        , [Ar, Ar, Terra]
        ] [Disparo (0, 2) Este Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T26: bazuca perfurando nordeste (vem de sudoeste)
    , Estado
        [ [Ar, Ar, Terra]
        , [Ar, Ar, Ar]
        , [Terra, Ar, Ar]
        ] [Disparo (0, 2) Sudoeste Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T27: bazuca perfurando noroeste (vem de sudeste)
    , Estado
        [ [Terra, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Terra]
        ] [Disparo (0, 0) Sudeste Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T28: bazuca perfurando sudeste (vem de noroeste)
    , Estado
        [ [Terra, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Terra]
        ] [Disparo (2, 2) Noroeste Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T29: bazuca perfurando sudoeste (vem de nordeste)
    , Estado
        [ [Ar, Ar, Terra]
        , [Ar, Ar, Ar]
        , [Terra, Ar, Ar]
        ] [Disparo (2, 0) Nordeste Bazuca Nothing 0]
        [minhocaViva (1, 1)]
    
    -- T30: bazuca perfurando subsolo (inválido)
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        ] [Disparo (1, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (2, 1)) (Viva 100) 0 0 2 0 0]
    
    -- T31: duas bazucas com o mesmo dono
    , Estado mapa4x4
        [ Disparo (1, 1) Norte Bazuca Nothing 0
        , Disparo (1, 2) Sul Bazuca Nothing 0
        ] [Minhoca (Just (2, 1)) (Viva 100) 0 0 3 0 0]
    
    ---------------------------------------------------------------------------------
    -- MINA (5 testes)
    ---------------------------------------------------------------------------------
    -- T32: mina sem tempo (válido)
    , Estado mapaSimples [Disparo (1, 1) Norte Mina Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 5 0]
    
    -- T33: mina com tempo 0 (válido)
    , Estado mapaSimples [Disparo (1, 1) Sul Mina (Just 0) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 2 0]
    
    -- T34: mina com tempo 1 (válido)
    , Estado mapaSimples [Disparo (1, 1) Este Mina (Just 1) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 2 0]
    
    -- T35: mina com tempo 2 (válido)
    , Estado mapaSimples [Disparo (1, 1) Oeste Mina (Just 2) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 1 0]
    
    -- T36: mina com tempo 3 (inválido)
    , Estado mapaSimples [Disparo (1, 1) Nordeste Mina (Just 3) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 1 0]
    
    ---------------------------------------------------------------------------------
    -- DINAMITE (7 testes)
    ---------------------------------------------------------------------------------
    -- T37: dinamite sem tempo (inválido)
    , Estado mapaSimples [Disparo (1, 1) Norte Dinamite Nothing 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 5]
    
    -- T38: dinamite com tempo 0 (válido)
    , Estado mapaSimples [Disparo (1, 1) Sul Dinamite (Just 0) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
    
    -- T39: dinamite com tempo 1 (válido)
    , Estado mapaSimples [Disparo (1, 1) Este Dinamite (Just 1) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
    
    -- T40: dinamite com tempo 2 (válido)
    , Estado mapaSimples [Disparo (1, 1) Oeste Dinamite (Just 2) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
    
    -- T41: dinamite com tempo 3 (válido)
    , Estado mapaSimples [Disparo (1, 1) Noroeste Dinamite (Just 3) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
    
    -- T42: dinamite com tempo 4 (válido)
    , Estado mapaSimples [Disparo (1, 1) Sudeste Dinamite (Just 4) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
    
    -- T43: dinamite com tempo 5 (inválido)
    , Estado mapaSimples [Disparo (1, 1) Sudoeste Dinamite (Just 5) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 1]
    
    ---------------------------------------------------------------------------------
    -- dono disparo (3 testes)
    ---------------------------------------------------------------------------------
    -- T44: dono negativo
    , Estado mapaSimples [Disparo (1, 1) Norte Bazuca Nothing (-1)]
        [minhocaViva (1, 1)]
    
    -- T45: dono maior que o tamanho da lista
    , Estado mapaSimples [Disparo (1, 1) Sul Mina Nothing 5]
        [minhocaViva (1, 1), minhocaViva (0, 1)]
    
    -- T46: lista de minhocas vazia
    , Estado mapaSimples [Disparo (1, 1) Este Dinamite (Just 2) 0] []
    
    ---------------------------------------------------------------------------------
    -- disparos duplicados (3 testes)
    ---------------------------------------------------------------------------------
    -- T47: duas minas com o mesmo dono
    , Estado mapa4x4
        [ Disparo (1, 1) Este Mina (Just 1) 0
        , Disparo (2, 2) Oeste Mina Nothing 0
        ] [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 3 0]
    
    -- T48: duas dinamites com o mesmo dono
    , Estado mapa4x4
        [ Disparo (1, 1) Norte Dinamite (Just 2) 0
        , Disparo (2, 2) Sul Dinamite (Just 3) 0
        ] [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 3]
    
    -- T49: disparos válidos com donos diferentes
    , Estado mapa4x4
        [ Disparo (1, 1) Norte Bazuca Nothing 0
        , Disparo (2, 2) Sul Mina (Just 1) 1
        ] [ Minhoca (Just (1, 2)) (Viva 100) 0 0 1 0 0
          , Minhoca (Just (2, 1)) (Viva 100) 0 0 0 1 0
          ]
    
    ---------------------------------------------------------------------------------
    -- minhoca posição (7 testes)
    ---------------------------------------------------------------------------------
    -- T50: posição com linha negativa
    , Estado mapaSimples [] [Minhoca (Just (-1, 1)) (Viva 100) 0 0 0 0 0]
    
    -- T51: posição com coluna negativa
    , Estado mapaSimples [] [Minhoca (Just (1, -1)) (Viva 100) 0 0 0 0 0]
    
    -- T52: posição fora do mapa (linha)
    , Estado mapaSimples [] [Minhoca (Just (10, 1)) (Viva 100) 0 0 0 0 0]
    
    -- T53: posição fora do mapa (coluna)
    , Estado mapaSimples [] [Minhoca (Just (1, 10)) (Viva 100) 0 0 0 0 0]
    
    -- T54: minhoca sobre terra
    , Estado mapaSimples [] [minhocaViva (0, 0)]
    
    -- T55: minhoca sobre pedra
    , Estado mapaSimples [] [minhocaViva (2, 2)]
    
    -- T56: minhoca sobre barril
    , Estado mapaSimples [Barril (1, 1) False] [minhocaViva (1, 1)]
    
    ---------------------------------------------------------------------------------
    -- minhoca duplicada (2 testes)
    ---------------------------------------------------------------------------------
    -- T57: duas minhocas na mesma posição
    , Estado mapaSimples [] 
        [ minhocaViva (1, 1)
        , Minhoca (Just (1, 1)) (Viva 50) 0 0 0 0 0
        ]
    
    -- T58: três minhocas, duas duplicadas
    , Estado mapa4x4 []
        [ minhocaViva (1, 1)
        , minhocaViva (2, 2)
        , minhocaViva (1, 1)
        ]
    
    ---------------------------------------------------------------------------------
    -- minhoca água (2 testes)
    ---------------------------------------------------------------------------------
    -- T59: minhoca viva em água (inválido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Agua,  Pedra]
        , [Pedra, Pedra, Pedra]
        ] [] [minhocaViva (1, 1)]
    
    -- T60: minhoca morta em água (válido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Agua,  Pedra]
        , [Pedra, Pedra, Pedra]
        ] [] [minhocaMorta (Just (1, 1))]
    
    ---------------------------------------------------------------------------------
    -- minhoca estado (10 testes)
    ---------------------------------------------------------------------------------
    -- T61: sem posição mas morta (válido)
    , Estado mapaSimples [] [minhocaMorta Nothing]
    
    -- T62: sem posição mas viva (inválido)
    , Estado mapaSimples [] [Minhoca Nothing (Viva 100) 0 0 0 0 0]
    
    -- T63: vida -10 (inválido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva (-10)) 0 0 0 0 0]
    
    -- T64: vida -1 (inválido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva (-1)) 0 0 0 0 0]
    
    -- T65: vida 0 (válido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 0) 0 0 0 0 0]
    
    -- T66: vida 1 (válido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 1) 0 0 0 0 0]
    
    -- T67: vida 50 (válido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 50) 1 1 1 1 1]
    
    -- T68: vida 99 (válido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 99) 0 0 0 0 0]
    
    -- T69: vida 100 (válido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0]
    
    -- T70: vida 101 (inválido)
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 101) 0 0 0 0 0]
    
    ---------------------------------------------------------------------------------
    -- minhoca munição (5 testes)
    ---------------------------------------------------------------------------------
    -- T71: jetpack negativo
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 100) (-1) 0 0 0 0]
    
    -- T72: escavadora negativa
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 100) 0 (-1) 0 0 0]
    
    -- T73: bazuca negativa
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 100) 0 0 (-1) 0 0]
    
    -- T74: mina negativa
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 (-1) 0]
    
    -- T75: dinamite negativa
    , Estado mapaSimples [] [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 (-1)]

    ---------------------------------------------------------------------------------
    -- ESTADOS VÁLIDOS (5 testes)
    ---------------------------------------------------------------------------------
    -- T76: estado válido mínimo
    , Estado mapaSimples [] [minhocaViva (1, 1)]
    
    -- T77: estado com barril e minhoca
    , Estado mapaSimples [Barril (1, 2) False] 
        [minhocaViva (1, 1)]
    
    -- T78: estado com disparo válido
    , Estado mapa4x4 [Disparo (1, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (2, 1)) (Viva 100) 0 0 3 0 0]
    
    -- T79: estado com múltiplas minhocas
    , Estado mapa4x4 []
        [ Minhoca (Just (1, 1)) (Viva 100) 5 5 5 5 5
        , Minhoca (Just (2, 2)) (Viva 80) 3 3 3 3 3
        , minhocaMorta Nothing
        ]
    
    -- T80: estado complexo válido
    , Estado
        [ [Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Agua, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
        [ Barril (1, 2) False
        , Barril (3, 2) True
        , Disparo (1, 1) Este Bazuca Nothing 0
        , Disparo (3, 3) Norte Dinamite (Just 4) 1
        ]
        [ Minhoca (Just (1, 3)) (Viva 100) 5 4 3 2 1
        , Minhoca (Just (3, 1)) (Viva 80) 3 3 3 3 3
        , minhocaMorta (Just (2, 2))
        ]
    ]

--------------------------------------------------------------------------------- - - ;)
-- *MAIN

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1