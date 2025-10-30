module Main where

import Labs2025
import Tarefa1
import Magic

-- ========== TESTES PARA VALIDAÇÃO DE ESTADOS ==========

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = 
    [ -- TESTES DE MAPA
      estadoMapaVazio
    , estadoMapaInvalido
    , estadoMapaValido
    
      -- TESTES DE OBJETOS
    , estadoBarrilValido
    , estadoBarrilPosicaoInvalida
    , estadoBarrilSobreTerreno
    , estadoBarrilSobreMinhoca
    , estadoBarrilSobreBarril
    
    , estadoDisparoJetpackInvalido
    , estadoDisparoEscavadoraInvalido
    , estadoBazucaValida
    , estadoBazucaComTempo
    , estadoBazucaPerfurando
    , estadoBazucaPerfurandoInvalido
    , estadoMinaSemTempo
    , estadoMinaComTempo
    , estadoMinaTempoInvalido
    , estadoDinamiteSemTempo
    , estadoDinamiteComTempo
    , estadoDinamiteTempoInvalido
    , estadoDisparoDonoInvalido
    , estadoDisparosDuplicados
    
      -- TESTES DE MINHOCAS
    , estadoMinhocaValida
    , estadoMinhocaSemPosicao
    , estadoMinhocaPosicaoInvalida
    , estadoMinhocaSobreTerreno
    , estadoMinhocaSobreBarril
    , estadoMinhocasSobrepostas
    , estadoMinhocaEmAguaViva
    , estadoMinhocaEmAguaMorta
    , estadoMinhocaSemPosicaoViva
    , estadoMinhocaVidaInvalida
    , estadoMinhocaMunicaoNegativa
    
      -- TESTES COMPLEXOS
    , estadoComplexoValido
    , estadoJogoReal
    ]

-- ========== TESTES DE MAPA ==========

-- Teste 1: Mapa vazio (inválido)
estadoMapaVazio :: Estado
estadoMapaVazio = Estado
    { mapaEstado = []
    , objetosEstado = []
    , minhocasEstado = []
    }

-- Teste 2: Mapa com linhas de tamanhos diferentes (inválido)
estadoMapaInvalido :: Estado
estadoMapaInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra, Pedra]  -- linha maior
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = []
    }

-- Teste 3: Mapa válido simples
estadoMapaValido :: Estado
estadoMapaValido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = []
    }

-- ========== TESTES DE BARRIS ==========

-- Teste 4: Barril em posição válida e livre
estadoBarrilValido :: Estado
estadoBarrilValido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Barril (1, 1) False]
    , minhocasEstado = []
    }

-- Teste 5: Barril em posição fora do mapa (inválido)
estadoBarrilPosicaoInvalida :: Estado
estadoBarrilPosicaoInvalida = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Barril (5, 5) False]
    , minhocasEstado = []
    }

-- Teste 6: Barril sobre terreno opaco (inválido)
estadoBarrilSobreTerreno :: Estado
estadoBarrilSobreTerreno = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Barril (0, 0) False]  -- sobre Terra
    , minhocasEstado = []
    }

-- Teste 7: Barril na mesma posição que minhoca (inválido)
estadoBarrilSobreMinhoca :: Estado
estadoBarrilSobreMinhoca = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Barril (1, 1) False]
    , minhocasEstado = 
        [ Minhoca
            { posicaoMinhoca = Just (1, 1)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 0
            , escavadoraMinhoca = 0
            , bazucaMinhoca = 0
            , minaMinhoca = 0
            , dinamiteMinhoca = 0
            }
        ]
    }

-- Teste 8: Dois barris na mesma posição (inválido)
estadoBarrilSobreBarril :: Estado
estadoBarrilSobreBarril = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (1, 1) False
        , Barril (1, 1) True
        ]
    , minhocasEstado = []
    }

-- ========== TESTES DE DISPAROS ==========

-- Teste 9: Disparo de Jetpack (inválido)
estadoDisparoJetpackInvalido :: Estado
estadoDisparoJetpackInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Jetpack Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 5 0 0 0 0 ]
    }

-- Teste 10: Disparo de Escavadora (inválido)
estadoDisparoEscavadoraInvalido :: Estado
estadoDisparoEscavadoraInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Escavadora Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 5 0 0 0 ]
    }

-- Teste 11: Disparo de Bazuca sem tempo (válido)
estadoBazucaValida :: Estado
estadoBazucaValida = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Bazuca Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 5 0 0 ]
    }

-- Teste 12: Disparo de Bazuca com tempo (inválido)
estadoBazucaComTempo :: Estado
estadoBazucaComTempo = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Bazuca (Just 3) 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 5 0 0 ]
    }

-- Teste 13: Bazuca perfurando superfície (válido)
estadoBazucaPerfurando :: Estado
estadoBazucaPerfurando = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (0, 1) Sul Bazuca Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 5 0 0 ]
    }

-- Teste 14: Bazuca perfurando subsolo (inválido)
estadoBazucaPerfurandoInvalido :: Estado
estadoBazucaPerfurandoInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Sul Bazuca Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (2, 1)) (Viva 100) 0 0 5 0 0 ]
    }

-- Teste 15: Mina sem tempo (válido)
estadoMinaSemTempo :: Estado
estadoMinaSemTempo = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Mina Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 0 5 0 ]
    }

-- Teste 16: Mina com tempo válido (válido)
estadoMinaComTempo :: Estado
estadoMinaComTempo = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Mina (Just 2) 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 0 5 0 ]
    }

-- Teste 17: Mina com tempo inválido (inválido)
estadoMinaTempoInvalido :: Estado
estadoMinaTempoInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Mina (Just 5) 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 0 5 0 ]
    }

-- Teste 18: Dinamite sem tempo (inválido)
estadoDinamiteSemTempo :: Estado
estadoDinamiteSemTempo = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Dinamite Nothing 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 5 ]
    }

-- Teste 19: Dinamite com tempo válido (válido)
estadoDinamiteComTempo :: Estado
estadoDinamiteComTempo = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Dinamite (Just 4) 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 5 ]
    }

-- Teste 20: Dinamite com tempo inválido (inválido)
estadoDinamiteTempoInvalido :: Estado
estadoDinamiteTempoInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Dinamite (Just 7) 0]
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 5 ]
    }

-- Teste 21: Disparo com dono inválido (inválido)
estadoDisparoDonoInvalido :: Estado
estadoDisparoDonoInvalido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Disparo (1, 1) Norte Bazuca Nothing 5]  -- dono 5 não existe
    , minhocasEstado = 
        [ Minhoca (Just (1, 2)) (Viva 100) 0 0 5 0 0 ]
    }

-- Teste 22: Dois disparos do mesmo tipo pelo mesmo dono (inválido)
estadoDisparosDuplicados :: Estado
estadoDisparosDuplicados = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra, Terra]
        , [Terra, Ar, Ar, Terra]
        , [Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (1, 1) Norte Bazuca Nothing 0
        , Disparo (1, 2) Sul Bazuca Nothing 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (2, 1)) (Viva 100) 0 0 5 0 0 ]
    }

-- ========== TESTES DE MINHOCAS ==========

-- Teste 23: Minhoca válida
estadoMinhocaValida :: Estado
estadoMinhocaValida = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 5 3 2 1 4 ]
    }

-- Teste 24: Minhoca sem posição e morta (válido)
estadoMinhocaSemPosicao :: Estado
estadoMinhocaSemPosicao = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca Nothing Morta 0 0 0 0 0 ]
    }

-- Teste 25: Minhoca em posição inválida (inválido)
estadoMinhocaPosicaoInvalida :: Estado
estadoMinhocaPosicaoInvalida = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (10, 10)) (Viva 100) 0 0 0 0 0 ]
    }

-- Teste 26: Minhoca sobre terreno opaco (inválido)
estadoMinhocaSobreTerreno :: Estado
estadoMinhocaSobreTerreno = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0 ]
    }

-- Teste 27: Minhoca sobre barril (inválido)
estadoMinhocaSobreBarril :: Estado
estadoMinhocaSobreBarril = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = [Barril (1, 1) False]
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0 ]
    }

-- Teste 28: Duas minhocas na mesma posição (inválido)
estadoMinhocasSobrepostas :: Estado
estadoMinhocasSobrepostas = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 50) 0 0 0 0 0
        ]
    }

-- Teste 29: Minhoca viva em água (inválido)
estadoMinhocaEmAguaViva :: Estado
estadoMinhocaEmAguaViva = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Agua, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0 ]
    }

-- Teste 30: Minhoca morta em água (válido)
estadoMinhocaEmAguaMorta :: Estado
estadoMinhocaEmAguaMorta = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Agua, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) Morta 0 0 0 0 0 ]
    }

-- Teste 31: Minhoca sem posição mas viva (inválido)
estadoMinhocaSemPosicaoViva :: Estado
estadoMinhocaSemPosicaoViva = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca Nothing (Viva 50) 0 0 0 0 0 ]
    }

-- Teste 32: Minhoca com vida > 100 (inválido)
estadoMinhocaVidaInvalida :: Estado
estadoMinhocaVidaInvalida = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 150) 0 0 0 0 0 ]
    }

-- Teste 33: Minhoca com munição negativa (inválido)
estadoMinhocaMunicaoNegativa :: Estado
estadoMinhocaMunicaoNegativa = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) (-1) 0 0 0 0 ]
    }

-- ========== TESTES COMPLEXOS ==========

-- Teste 34: Estado complexo válido
estadoComplexoValido :: Estado
estadoComplexoValido = Estado
    { mapaEstado = 
        [ [Terra, Terra, Terra, Terra, Terra]
        , [Terra, Ar, Ar, Ar, Terra]
        , [Terra, Ar, Agua, Ar, Terra]
        , [Terra, Ar, Ar, Ar, Terra]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (1, 2) False
        , Disparo (3, 2) Norte Bazuca Nothing 0
        , Disparo (1, 3) Este Dinamite (Just 3) 1
        ]
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 5 3 2 1 4
        , Minhoca (Just (3, 3)) (Viva 75) 3 2 1 0 2
        , Minhoca (Just (2, 2)) Morta 0 0 0 0 0
        ]
    }

-- Teste 35: Jogo realista
estadoJogoReal :: Estado
estadoJogoReal = Estado
    { mapaEstado = 
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Terra, Terra, Terra, Ar, Pedra]
        , [Pedra, Ar, Terra, Ar, Terra, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , objetosEstado = 
        [ Barril (2, 3) False
        , Barril (4, 4) True
        ]
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 10 5 3 2 1
        , Minhoca (Just (1, 5)) (Viva 85) 8 4 2 1 1
        , Minhoca (Just (4, 2)) (Viva 60) 5 3 1 0 0
        ]
    }

-- ========== MAIN ==========

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1