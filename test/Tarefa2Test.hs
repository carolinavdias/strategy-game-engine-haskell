--ficheiro test/Tarefa2Test.hs :)
import Labs2025
import Tarefa2
import Magic

-- *FUNÇÕES AUXILIARES

-- > mapa simples 2x5
mapaBasico :: Mapa
mapaBasico = 
    [ [Ar, Ar, Ar, Ar, Ar]
    , [Terra, Terra, Terra, Terra, Terra]
    ]

-- > mapa com 3 linhas
mapaTresLinhas :: Mapa
mapaTresLinhas = 
    [ [Ar, Ar, Ar, Ar, Ar]
    , [Ar, Ar, Ar, Ar, Ar]
    , [Terra, Terra, Terra, Terra, Terra]
    ]

-- > mapa com água
mapaComAgua :: Mapa
mapaComAgua = 
    [ [Ar, Ar, Agua, Ar, Ar]
    , [Terra, Terra, Terra, Terra, Terra]
    ]

-- > mapa com terra no meio
mapaComTerra :: Mapa
mapaComTerra = 
    [ [Ar, Ar, Terra, Ar, Ar]
    , [Terra, Terra, Terra, Terra, Terra]
    ]

-- > mapa com terra à direita
mapaComTerraLado :: Mapa
mapaComTerraLado = 
    [ [Ar, Ar, Ar, Ar, Ar]
    , [Ar, Terra, Terra, Ar, Ar]
    , [Terra, Terra, Terra, Terra, Terra]
    ]

-- >> minhoca viva padrão
minhocaViva :: Posicao -> Minhoca
minhocaViva pos = Minhoca (Just pos) (Viva 100) 1 1 1 1 1

-- >> minhoca sem munição específica
minhocaSemJetpack :: Posicao -> Minhoca
minhocaSemJetpack pos = Minhoca (Just pos) (Viva 100) 0 1 1 1 1

minhocaSemEscavadora :: Posicao -> Minhoca
minhocaSemEscavadora pos = Minhoca (Just pos) (Viva 100) 1 0 1 1 1

minhocaSemBazuca :: Posicao -> Minhoca
minhocaSemBazuca pos = Minhoca (Just pos) (Viva 100) 1 1 0 1 1

minhocaSemMina :: Posicao -> Minhoca
minhocaSemMina pos = Minhoca (Just pos) (Viva 100) 1 1 1 0 1

minhocaSemDinamite :: Posicao -> Minhoca
minhocaSemDinamite pos = Minhoca (Just pos) (Viva 100) 1 1 1 1 0

-- >> minhoca morta
minhocaMorta :: Maybe Posicao -> Minhoca
minhocaMorta pos = Minhoca pos Morta 1 1 1 1 1

-- >>>> TESTES DA TAREFA 2! <<<<
testesTarefa2 :: [(NumMinhoca, Jogada, Estado)]
testesTarefa2 = 
    [
    ---------------------------------------------------------------------------------
    -- MOVIMENTOS BÁSICOS (7 testes)
    ---------------------------------------------------------------------------------
    -- T1: move Este
      (0, Move Este, Estado mapaBasico [] [minhocaViva (0, 2)])
    
    -- T2: move Oeste
    , (0, Move Oeste, Estado mapaBasico [] [minhocaViva (0, 2)])
    
    -- T3: move Sul (no chão, não deve mover)
    , (0, Move Sul, Estado mapaBasico [] [minhocaViva (0, 2)])
    
    -- T4: move Sudeste
    , (0, Move Sudeste, Estado mapaTresLinhas [] [minhocaViva (1, 1)])
    
    -- T5: move Sudoeste
    , (0, Move Sudoeste, Estado mapaTresLinhas [] [minhocaViva (1, 3)])
    
    -- T6: move Sul (posição inválida)
    , (0, Move Sul, Estado mapaTresLinhas [] [minhocaViva (6, 6)])
    
    -- T7: move Norte (sem posição)
    , (0, Move Norte, Estado mapaTresLinhas [] [minhocaMorta Nothing])
    
    ---------------------------------------------------------------------------------
    -- SALTOS (4 testes)
    ---------------------------------------------------------------------------------
    -- T8: salto Norte
    , (0, Move Norte, Estado mapaTresLinhas [] [minhocaViva (1, 2)])
    
    -- T9: salto Nordeste
    , (0, Move Nordeste, Estado mapaTresLinhas [] [minhocaViva (1, 1)])
    
    -- T10: salto Noroeste
    , (0, Move Noroeste, Estado mapaTresLinhas [] [minhocaViva (1, 3)])
    
    -- T11: salto no ar (não deve funcionar)
    , (0, Move Norte, Estado 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ] [] [minhocaViva (1, 2)])
    
    ---------------------------------------------------------------------------------
    -- MOVIMENTOS INVÁLIDOS (8 testes)
    ---------------------------------------------------------------------------------
    -- T12: move para Terra
    , (0, Move Este, Estado mapaComTerra [] [minhocaViva (0, 1)])
    
    -- T13: move fora do mapa (Oeste)
    , (0, Move Oeste, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T14: move fora do mapa (Este)
    , (0, Move Este, Estado mapaBasico [] [minhocaViva (0, 4)])
    
    -- T15: move fora do mapa (Norte)
    , (0, Move Norte, Estado mapaBasico [] [minhocaViva (0, 2)])
    
    -- T16: move para Água
    , (0, Move Este, Estado mapaComAgua [] [minhocaViva (0, 1)])
    
    -- T17: move com Barril no caminho
    , (0, Move Este, Estado mapaBasico [Barril (0, 2) False] [minhocaViva (0, 1)])
    
    -- T18: move com outra minhoca
    , (0, Move Este, Estado mapaBasico [] [minhocaViva (0, 1), minhocaViva (0, 2)])
    
    -- T19: move Sul em Água
    , (0, Move Sul, Estado mapaComAgua [] [minhocaViva (0, 2)])
    
    ---------------------------------------------------------------------------------
    -- DISPAROS BÁSICOS (13 testes)
    ---------------------------------------------------------------------------------
    -- T20: Jetpack Norte
    , (0, Dispara Jetpack Norte, Estado mapaTresLinhas [] [minhocaViva (1, 2)])
    
    -- T21: Jetpack bloqueado
    , (0, Dispara Jetpack Norte, Estado mapaComTerra [] [minhocaViva (1, 2)])
    
    -- T22: Escavadora em Terra
    , (0, Dispara Escavadora Este, Estado mapaComTerraLado [] [minhocaViva (1, 0)])
    
    -- T23: Escavadora em Ar
    , (0, Dispara Escavadora Este, Estado mapaBasico [] [minhocaViva (0, 1)])
    
    -- T24: Escavadora em Água
    , (0, Dispara Escavadora Este, Estado mapaComAgua [] [minhocaViva (0, 1)])
    
    -- T25: Escavadora fora do mapa
    , (0, Dispara Escavadora Oeste, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T26: Escavadora com minhoca no destino
    , (0, Dispara Escavadora Este, Estado mapaComTerraLado [] [minhocaViva (1, 0), minhocaViva (1, 1)])
    
    -- T27: Bazuca
    , (0, Dispara Bazuca Este, Estado mapaBasico [] [minhocaViva (0, 1)])
    
    -- T28: Mina Sul
    , (0, Dispara Mina Sul, Estado mapaTresLinhas [] [minhocaViva (0, 2)])
    
    -- T29: Dinamite Este
    , (0, Dispara Dinamite Este, Estado mapaBasico [] [minhocaViva (0, 1)])
    
    -- T30: Mina em posição ocupada
    , (0, Dispara Mina Este, Estado mapaBasico [Barril (0, 2) False] [minhocaViva (0, 1)])
    
    -- T31: Dinamite em posição ocupada
    , (0, Dispara Dinamite Este, Estado mapaBasico [Barril (0, 2) False] [minhocaViva (0, 1)])
    
    -- T32: Jetpack bloqueado sem posição
    , (0, Dispara Jetpack Norte, Estado mapaComTerra [] [minhocaMorta Nothing])
    
     ---------------------------------------------------------------------------------
    -- SEM MUNIÇÃO (5 testes)
    ---------------------------------------------------------------------------------
    -- T33: Jetpack sem munição
    , (0, Dispara Jetpack Norte, Estado mapaBasico [] [minhocaSemJetpack (0, 2)])
    
    -- T34: Escavadora sem munição
    , (0, Dispara Escavadora Este, Estado mapaBasico [] [minhocaSemEscavadora (0, 1)])
    
    -- T35: Bazuca sem munição
    , (0, Dispara Bazuca Este, Estado mapaBasico [] [minhocaSemBazuca (0, 1)])
    
    -- T36: Mina sem munição
    , (0, Dispara Mina Sul, Estado mapaBasico [] [minhocaSemMina (0, 2)])
    
    -- T37: Dinamite sem munição
    , (0, Dispara Dinamite Este, Estado mapaBasico [] [minhocaSemDinamite (0, 1)])
    
    ---------------------------------------------------------------------------------
    -- DISPAROS DUPLICADOS (3 testes)
    ---------------------------------------------------------------------------------
    -- T38: Bazuca duplicada
    , (0, Dispara Bazuca Este, Estado mapaBasico [Disparo (0, 3) Este Bazuca Nothing 0] [minhocaViva (0, 1)])
    
    -- T39: Mina duplicada
    , (0, Dispara Mina Sul, Estado mapaTresLinhas [Disparo (1, 3) Sul Mina Nothing 0] [minhocaViva (0, 2)])
    
    -- T40: Dinamite duplicada
    , (0, Dispara Dinamite Este, Estado mapaBasico [Disparo (0, 3) Este Dinamite (Just 4) 0] [minhocaViva (0, 1)])
    
    ---------------------------------------------------------------------------------
    -- MINHOCAS MORTAS (2 testes)
    ---------------------------------------------------------------------------------
    -- T41: Move com minhoca morta
    , (0, Move Este, Estado mapaBasico [] [minhocaMorta (Just (0, 1))])
    
    -- T42: Disparo com minhoca morta
    , (0, Dispara Bazuca Este, Estado mapaBasico [] [minhocaMorta (Just (0, 1))])
    
    ---------------------------------------------------------------------------------
    -- CASOS EXTREMOS (5 testes)
    ---------------------------------------------------------------------------------
    -- T43: Múltiplas minhocas (joga segunda)
    , (1, Move Este, Estado mapaBasico [] [minhocaViva (0, 1), minhocaViva (0, 3)])
    
    -- T44: Disparo fora do mapa
    , (0, Dispara Bazuca Oeste, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T45: Minhoca sem posição
    , (0, Move Este, Estado mapaBasico [] [minhocaMorta Nothing])
    
    -- T46: Índice negativo
    , (-1, Move Este, Estado mapaBasico [] [minhocaViva (0, 1)])
    
    -- T47: Índice muito grande
    , (5, Move Este, Estado mapaBasico [] [minhocaViva (0, 1)])
    
    ---------------------------------------------------------------------------------
    -- COBERTURA ADICIONAL (10 testes)
    ---------------------------------------------------------------------------------
    -- T48: Move fora Sul
    , (0, Move Sul, Estado mapaBasico [] [minhocaViva (1, 2)])
    
    -- T49: Mina com minhoca
    , (0, Dispara Mina Este, Estado mapaBasico [] [minhocaViva (0, 1), minhocaViva (0, 2)])
    
    -- T50: Dinamite com minhoca
    , (0, Dispara Dinamite Este, Estado mapaBasico [] [minhocaViva (0, 1), minhocaViva (0, 2)])
    
    -- T51: Mina em Terra
    , (0, Dispara Mina Este, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T52: Dinamite em Terra
    , (0, Dispara Dinamite Este, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T53: Mina fora do mapa
    , (0, Dispara Mina Oeste, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T54: Dinamite fora do mapa
    , (0, Dispara Dinamite Oeste, Estado mapaBasico [] [minhocaViva (0, 0)])
    
    -- T55: Escavadora Ar com minhoca
    , (0, Dispara Escavadora Este, Estado mapaBasico [] [minhocaViva (0, 1), minhocaViva (0, 2)])
    
    -- T56: Move para Água Sul
    , (0, Move Sul, Estado 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Agua, Agua, Agua, Agua, Agua]
        , [Terra, Terra, Terra, Terra, Terra]
        ] [] [minhocaViva (0, 2)])
    
    -- T57: Escavadora em terreno específico
    , (0, Dispara Escavadora Este, Estado mapaBasico [] [minhocaViva (0, 1)])
    ]

------------------------------------------------------------------------------- - ;-)
-- *MAIN

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i, j, e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2