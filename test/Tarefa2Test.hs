module Main where

import Labs2025
import Tarefa2
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 2
testesTarefa2 :: [(NumMinhoca,Jogada,Estado)]
testesTarefa2 = 
    -- Testes de movimentação básica
    testesMovimentoBasico ++
    -- Testes de movimentação com saltos
    testesMovimentoSaltos ++
    -- Testes de movimentação inválida
    testesMovimentoInvalido ++
    -- Testes de disparos básicos
    testesDisparosBasicos ++
    -- Testes de disparos sem munição
    testesDisparosSemMunicao ++
    -- Testes de disparos duplicados
    testesDisparosDuplicados ++
    -- Testes com minhocas mortas
    testesMinhocasMortas ++
    -- Testes de casos extremos
    testesCasosExtremos

-- Para facilitar debug inicial, também definimos testes simples
testesSimples :: [(NumMinhoca,Jogada,Estado)]
testesSimples = 
    [ (0, Move Este, estadoSimples1)
    ]

estadoSimples1 :: Estado
estadoSimples1 = Estado
    { mapaEstado = [[Ar,Ar,Ar]
                   ,[Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes de Movimentação Básica

testesMovimentoBasico :: [(NumMinhoca,Jogada,Estado)]
testesMovimentoBasico = 
    [ -- Movimento simples para Este
      (0, Move Este, estadoMovimentoBasico1)
    , -- Movimento simples para Oeste
      (0, Move Oeste, estadoMovimentoBasico2)
    , -- Movimento simples para Sul (impossível, minhoca no chão)
      (0, Move Sul, estadoMovimentoBasico3)
    , -- Movimento para Sudeste
      (0, Move Sudeste, estadoMovimentoBasico4)
    , -- Movimento para Sudoeste
      (0, Move Sudoeste, estadoMovimentoBasico5)
    ]

-- Minhoca no chão, pode mover para os lados
estadoMovimentoBasico1 :: Estado
estadoMovimentoBasico1 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoBasico2 :: Estado
estadoMovimentoBasico2 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoBasico3 :: Estado
estadoMovimentoBasico3 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoBasico4 :: Estado
estadoMovimentoBasico4 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoBasico5 :: Estado
estadoMovimentoBasico5 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,3)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes de Movimentação com Saltos

testesMovimentoSaltos :: [(NumMinhoca,Jogada,Estado)]
testesMovimentoSaltos = 
    [ -- Salto para Norte
      (0, Move Norte, estadoSalto1)
    , -- Salto para Nordeste
      (0, Move Nordeste, estadoSalto2)
    , -- Salto para Noroeste
      (0, Move Noroeste, estadoSalto3)
    , -- Tentativa de salto no ar (deve falhar)
      (0, Move Norte, estadoSaltoNoAr)
    ]

estadoSalto1 :: Estado
estadoSalto1 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

estadoSalto2 :: Estado
estadoSalto2 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1]
    }

estadoSalto3 :: Estado
estadoSalto3 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,3)) (Viva 100) 1 1 1 1 1]
    }

estadoSaltoNoAr :: Estado
estadoSaltoNoAr = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes de Movimentação Inválida

testesMovimentoInvalido :: [(NumMinhoca,Jogada,Estado)]
testesMovimentoInvalido = 
    [ -- Movimento para terreno opaco (Terra)
      (0, Move Este, estadoMovimentoParaTerra)
    , -- Movimento para fora do mapa (morte)
      (0, Move Oeste, estadoMovimentoForaMapa)
    , -- Movimento para água (morte)
      (0, Move Este, estadoMovimentoParaAgua)
    , -- Movimento com obstáculo (barril)
      (0, Move Este, estadoMovimentoComBarril)
    , -- Movimento com obstáculo (outra minhoca)
      (0, Move Este, estadoMovimentoComMinhoca)
    ]

estadoMovimentoParaTerra :: Estado
estadoMovimentoParaTerra = Estado
    { mapaEstado = [[Ar,Ar,Terra,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoForaMapa :: Estado
estadoMovimentoForaMapa = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoParaAgua :: Estado
estadoMovimentoParaAgua = Estado
    { mapaEstado = [[Ar,Ar,Agua,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoComBarril :: Estado
estadoMovimentoComBarril = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Barril (0,2) False]
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoComMinhoca :: Estado
estadoMovimentoComMinhoca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1
                       ,Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes de Disparos Básicos

testesDisparosBasicos :: [(NumMinhoca,Jogada,Estado)]
testesDisparosBasicos = 
    [ -- Disparo de Jetpack
      (0, Dispara Jetpack Norte, estadoDisparoJetpack)
    , -- Disparo de Escavadora
      (0, Dispara Escavadora Este, estadoDisparoEscavadora)
    , -- Disparo de Bazuca
      (0, Dispara Bazuca Este, estadoDisparoBazuca)
    , -- Disparo de Mina em posição livre
      (0, Dispara Mina Sul, estadoDisparoMina)
    , -- Disparo de Dinamite em posição livre
      (0, Dispara Dinamite Este, estadoDisparoDinamite)
    , -- Disparo de Mina em posição ocupada (fica na posição atual)
      (0, Dispara Mina Este, estadoDisparoMinaOcupada)
    , -- Disparo de Dinamite em posição ocupada (fica na posição atual)
      (0, Dispara Dinamite Este, estadoDisparoDinamiteOcupada)
    ]

estadoDisparoJetpack :: Estado
estadoDisparoJetpack = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoEscavadora :: Estado
estadoDisparoEscavadora = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoBazuca :: Estado
estadoDisparoBazuca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoMina :: Estado
estadoDisparoMina = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoDinamite :: Estado
estadoDisparoDinamite = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoMinaOcupada :: Estado
estadoDisparoMinaOcupada = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Barril (0,2) False]
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoDinamiteOcupada :: Estado
estadoDisparoDinamiteOcupada = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Barril (0,2) False]
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes de Disparos sem Munição

testesDisparosSemMunicao :: [(NumMinhoca,Jogada,Estado)]
testesDisparosSemMunicao = 
    [ -- Disparo de Jetpack sem munição
      (0, Dispara Jetpack Norte, estadoSemMunicaoJetpack)
    , -- Disparo de Escavadora sem munição
      (0, Dispara Escavadora Este, estadoSemMunicaoEscavadora)
    , -- Disparo de Bazuca sem munição
      (0, Dispara Bazuca Este, estadoSemMunicaoBazuca)
    , -- Disparo de Mina sem munição
      (0, Dispara Mina Sul, estadoSemMunicaoMina)
    , -- Disparo de Dinamite sem munição
      (0, Dispara Dinamite Este, estadoSemMunicaoDinamite)
    ]

estadoSemMunicaoJetpack :: Estado
estadoSemMunicaoJetpack = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 0 1 1 1 1]
    }

estadoSemMunicaoEscavadora :: Estado
estadoSemMunicaoEscavadora = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 0 1 1 1]
    }

estadoSemMunicaoBazuca :: Estado
estadoSemMunicaoBazuca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 0 1 1]
    }

estadoSemMunicaoMina :: Estado
estadoSemMunicaoMina = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 0 1]
    }

estadoSemMunicaoDinamite :: Estado
estadoSemMunicaoDinamite = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 0]
    }

-- * Testes de Disparos Duplicados

testesDisparosDuplicados :: [(NumMinhoca,Jogada,Estado)]
testesDisparosDuplicados = 
    [ -- Disparo de Bazuca quando já existe um ativo
      (0, Dispara Bazuca Este, estadoDisparoBazucaDuplicado)
    , -- Disparo de Mina quando já existe uma ativa
      (0, Dispara Mina Sul, estadoDisparoMinaDuplicada)
    , -- Disparo de Dinamite quando já existe uma ativa
      (0, Dispara Dinamite Este, estadoDisparoDinamiteDuplicada)
    ]

estadoDisparoBazucaDuplicado :: Estado
estadoDisparoBazucaDuplicado = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Disparo (0,3) Este Bazuca Nothing 0]
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoMinaDuplicada :: Estado
estadoDisparoMinaDuplicada = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Disparo (1,3) Sul Mina Nothing 0]
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoDinamiteDuplicada :: Estado
estadoDisparoDinamiteDuplicada = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Disparo (0,3) Este Dinamite (Just 4) 0]
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes com Minhocas Mortas

testesMinhocasMortas :: [(NumMinhoca,Jogada,Estado)]
testesMinhocasMortas = 
    [ -- Movimento com minhoca morta
      (0, Move Este, estadoMinhocaMortaMovimento)
    , -- Disparo com minhoca morta
      (0, Dispara Bazuca Este, estadoMinhocaMortaDisparo)
    ]

estadoMinhocaMortaMovimento :: Estado
estadoMinhocaMortaMovimento = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) Morta 1 1 1 1 1]
    }

estadoMinhocaMortaDisparo :: Estado
estadoMinhocaMortaDisparo = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) Morta 1 1 1 1 1]
    }

-- * Testes de Casos Extremos

testesCasosExtremos :: [(NumMinhoca,Jogada,Estado)]
testesCasosExtremos = 
    [ -- Escavadora em terreno que não é Terra
      (0, Dispara Escavadora Este, estadoEscavadoraEmAr)
    , -- Jetpack para posição ocupada
      (0, Dispara Jetpack Norte, estadoJetpackOcupado)
    , -- Múltiplas minhocas, jogada da segunda
      (1, Move Este, estadoMultiplasMinhocas)
    , -- Disparo fora do mapa (deve ser eliminado)
      (0, Dispara Bazuca Oeste, estadoDisparoForaMapa)
    , -- Minhoca sem posição (não deve fazer nada)
      (0, Move Este, estadoMinhocaSemPosicao)
    ]

estadoEscavadoraEmAr :: Estado
estadoEscavadoraEmAr = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoJetpackOcupado :: Estado
estadoJetpackOcupado = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = [Barril (0,2) False]
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMultiplasMinhocas :: Estado
estadoMultiplasMinhocas = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1
                       ,Minhoca (Just (0,3)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoForaMapa :: Estado
estadoDisparoForaMapa = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoMinhocaSemPosicao :: Estado
estadoMinhocaSemPosicao = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca Nothing Morta 1 1 1 1 1]
    }

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2