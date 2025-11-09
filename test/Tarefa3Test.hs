--ficheiro test/Tarefa3Test.hs ;)
import Labs2025
import Tarefa3
import Magic



-- *FUNÇÕES AUXILIARES PARA TESTES

-- > mapa simples para testes (5x5)
mapaSimples :: Mapa
mapaSimples = 
    [ [Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Ar,    Terra, Ar,    Pedra]
    , [Pedra, Terra, Terra, Terra, Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- > mapa com água
mapaComAgua :: Mapa
mapaComAgua = 
    [ [Pedra, Pedra, Pedra, Pedra, Pedra]
    , [Pedra, Ar,    Ar,    Ar,    Pedra]
    , [Pedra, Agua,  Terra, Ar,    Pedra]
    , [Pedra, Agua,  Terra, Terra, Pedra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- >> minhoca viva padrão
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

-- >>>> TESTES DA TAREFA 3! <<<<

testesTarefa3 :: [Estado]
testesTarefa3 = 
    [ 
    ----------------------------------------------------------------------
    -- MINHOCAS
    ----------------------------------------------------------------------

    -- t1: minhoca no ar deve cair
    Estado mapaSimples [] [minhocaViva (1, 1)]

    -- t2: minhoca no chão mantém-se
  , Estado mapaSimples [] [minhocaViva (3, 1)]

    -- t3: minhoca viva cai na água e morre
  , Estado mapaComAgua [] [minhocaViva (1, 1)]

    -- t4: minhoca morta cai na água (mantém-se morta)
  , Estado mapaComAgua [] [minhocaMorta (Just (1, 1))]

    -- t5: minhoca sem posição (não deve alterar o estado)
  , Estado mapaSimples [] [minhocaMorta Nothing]

  ----------------------------------------------------------------------
  -- BARRIS
  ----------------------------------------------------------------------

    -- t6: barril no chão mantém-se
  , Estado mapaSimples [Barril (3, 2) False] []

    -- t7: barril no ar ativa-se (começa a cair)
  , Estado mapaSimples [Barril (1, 1) False] []

    -- t8: barril na água ativa-se
  , Estado mapaComAgua [Barril (2, 1) False] []

    -- t9: barril ativado explode
  , Estado mapaSimples [Barril (2, 2) True] []

    -- t10: explosão de barril ativa outro barril próximo
  , Estado mapaSimples
      [ Barril (2, 2) True
      , Barril (2, 3) False
      ] []

  ----------------------------------------------------------------------
  -- BAZUCA
  ----------------------------------------------------------------------

    -- t11: bazuca avança na direção este
  , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 5) 0] []

    -- t12: bazuca com tempo 0 explode
  , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 0) 0] []

    -- t13: bazuca colide com terra e explode
  , Estado mapaSimples [Disparo (2, 2) Este Bazuca (Just 3) 0] []

    -- t14: bazuca com tempo > 0 decrementa o contador
  , Estado mapaSimples [Disparo (1, 1) Este Bazuca (Just 3) 0] []

    -- t15: bazuca sem tempo definido (não faz nada)
  , Estado mapaSimples [Disparo (1, 1) Este Bazuca Nothing 0] []

  ----------------------------------------------------------------------
  -- MINAS
  ----------------------------------------------------------------------

    -- t16: mina sem tempo e sem inimigos (fica inativa)
  , Estado mapaSimples
      [Disparo (3, 2) Norte Mina Nothing 0]
      [minhocaViva (1, 1)]

    -- t17: mina sem tempo mas com inimigo próximo (ativa)
  , Estado mapaSimples
      [Disparo (3, 2) Norte Mina Nothing 0]
      [ minhocaViva (1, 1)
      , minhocaViva (3, 3)
      ]

    -- t18: mina com tempo > 0 diminui o contador
  , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 2) 0] []

    -- t19: mina com tempo 0 explode
  , Estado mapaSimples [Disparo (3, 2) Norte Mina (Just 0) 0] []

    -- t20: mina no ar cai para sul
  , Estado mapaSimples [Disparo (1, 1) Sul Mina Nothing 0] []

  ----------------------------------------------------------------------
  -- DINAMITE
  ----------------------------------------------------------------------

    -- t21: dinamite com tempo 0 explode
  , Estado mapaSimples [Disparo (2, 2) Este Dinamite (Just 0) 0] []

    -- t22: dinamite sem tempo (mantém-se no mesmo sítio)
  , Estado mapaSimples [Disparo (1, 1) Este Dinamite Nothing 0] []

    -- t23: dinamite no chão (não se move)
  , Estado mapaSimples [Disparo (3, 2) Este Dinamite (Just 3) 0] []

    -- t24: dinamite segue trajetória parabólica (Este → Sudeste)
  , Estado mapaSimples [Disparo (1, 1) Este Dinamite (Just 5) 0] []

    -- t25: dinamite lançada para norte cai verticalmente
  , Estado mapaSimples [Disparo (1, 2) Norte Dinamite (Just 5) 0] []

  ----------------------------------------------------------------------
  -- EXPLOSÕES
  ----------------------------------------------------------------------

    -- t26: explosão mata minhoca no centro
  , Estado mapaSimples
      [Barril (2, 2) True]
      [minhocaViva (2, 2)]

    -- t27: explosão reduz vida de minhoca próxima
  , Estado mapaSimples
      [Barril (2, 2) True]
      [minhocaViva (2, 3)]

    -- t28: explosão com múltiplas minhocas próximas
  , Estado mapaSimples
      [Disparo (2, 2) Este Bazuca (Just 0) 0]
      [ minhocaViva (2, 2)
      , minhocaViva (2, 3)
      ]

    -- t29: mina explode (diâmetro de 3 casas)
  , Estado mapaSimples
      [Disparo (2, 2) Norte Mina (Just 0) 0]
      [minhocaViva (2, 2)]

    -- t30: explosão não afeta minhoca já morta
  , Estado mapaSimples
      [Barril (2, 2) True]
      [minhocaMorta (Just (2, 2))]
  ]

--------------------------------------------------------------------------------
-- * MAIN

dataTarefa3 :: IO TaskData
dataTarefa3 = do
  let ins = testesTarefa3
  outs <- mapM (runTest . avancaEstado) ins
  return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3