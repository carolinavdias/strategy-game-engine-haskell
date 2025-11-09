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
    testesCasosExtremos ++
    -- Testes para cobrir condições específicas
    testesCoberturaMelhorada

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
    , 
      (0, Move Sul , estadoMovimentoBasico6)
    , 
      (0, Move Norte , estadoMovimentoBasico7)
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

estadoMovimentoBasico6 :: Estado
estadoMovimentoBasico6 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (6,6)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoBasico7 :: Estado
estadoMovimentoBasico7 = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Nothing) (Viva 100) 1 1 1 1 1]
    }


-- * Testes de Movimentação com Saltos

testesMovimentoSaltos :: [(NumMinhoca,Jogada,Estado)]
testesMovimentoSaltos = 
    [ -- Salto para Norte (minhoca no chão pode saltar)
      (0, Move Norte, estadoSalto1)
    , -- Salto para Nordeste (minhoca no chão)
      (0, Move Nordeste, estadoSalto2)
    , -- Salto para Noroeste (minhoca no chão)
      (0, Move Noroeste, estadoSalto3)
    , -- Tentativa de salto no ar (deve falhar - minhoca já está no ar)
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
    , -- Movimento para fora do mapa pelo oeste (morte)
      (0, Move Oeste, estadoMovimentoForaMapaOeste)
    , -- Movimento para fora do mapa pelo leste
      (0, Move Este, estadoMovimentoForaMapaLeste)
    , -- Movimento para fora do mapa pelo norte
      (0, Move Norte, estadoMovimentoForaMapaNorte)
    , -- Movimento para água (morte)
      (0, Move Este, estadoMovimentoParaAgua)
    , -- Movimento com obstáculo (barril)
      (0, Move Este, estadoMovimentoComBarril)
    , -- Movimento com obstáculo (outra minhoca)
      (0, Move Este, estadoMovimentoComMinhoca)
    , 
      (0, Move Sul, estadoMovimentoEmAgua)
    ]

estadoMovimentoParaTerra :: Estado
estadoMovimentoParaTerra = Estado
    { mapaEstado = [[Ar,Ar,Terra,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoForaMapaOeste :: Estado
estadoMovimentoForaMapaOeste = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoForaMapaLeste :: Estado
estadoMovimentoForaMapaLeste = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,4)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoForaMapaNorte :: Estado
estadoMovimentoForaMapaNorte = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
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

estadoMovimentoEmAgua :: Estado
estadoMovimentoEmAgua = Estado
    { mapaEstado = [[Ar,Ar,Agua,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes de Disparos Básicos

testesDisparosBasicos :: [(NumMinhoca,Jogada,Estado)]
testesDisparosBasicos = 
    [ -- Disparo de Jetpack (posição livre)
      (0, Dispara Jetpack Norte, estadoDisparoJetpack)
    , -- Disparo de Jetpack (posição ocupada - fica no lugar)
      (0, Dispara Jetpack Norte, estadoDisparoJetpackBloqueado)
    , -- Disparo de Escavadora em Terra
      (0, Dispara Escavadora Este, estadoDisparoEscavadora)
    , -- Disparo de Escavadora em Ar
      (0, Dispara Escavadora Este, estadoDisparoEscavadoraEmAr)
    , -- Disparo de Escavadora em Água
      (0, Dispara Escavadora Este, estadoDisparoEscavadoraEmAgua)
    , -- Disparo de Escavadora fora do mapa
      (0, Dispara Escavadora Oeste, estadoDisparoEscavadoraForaMapa)
    , -- Disparo de Escavadora em Terra com minhoca no destino
      (0, Dispara Escavadora Este, estadoDisparoEscavadoraComMinhoca)
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
    , 
      (0, Dispara Jetpack Norte, estadoDisparoJetpackBloqueado2)
    ]

estadoDisparoJetpack :: Estado
estadoDisparoJetpack = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoJetpackBloqueado :: Estado
estadoDisparoJetpackBloqueado = Estado
    { mapaEstado = [[Ar,Ar,Terra,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoEscavadora :: Estado
estadoDisparoEscavadora = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Terra,Terra,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoEscavadoraEmAr :: Estado
estadoDisparoEscavadoraEmAr = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoEscavadoraEmAgua :: Estado
estadoDisparoEscavadoraEmAgua = Estado
    { mapaEstado = [[Ar,Ar,Agua,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoEscavadoraForaMapa :: Estado
estadoDisparoEscavadoraForaMapa = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoDisparoEscavadoraComMinhoca :: Estado
estadoDisparoEscavadoraComMinhoca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Ar,Terra,Terra,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1
                       ,Minhoca (Just (1,1)) (Viva 100) 1 1 1 1 1]
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

estadoDisparoJetpackBloqueado2 :: Estado
estadoDisparoJetpackBloqueado2 = Estado
    { mapaEstado = [[Ar,Ar,Terra,Ar,Ar]
                   ,[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Nothing) (Viva 100) 1 1 1 1 1]
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
    [ -- Múltiplas minhocas, jogada da segunda
      (1, Move Este, estadoMultiplasMinhocas)
    , -- Disparo fora do mapa (não deve adicionar ao estado)
      (0, Dispara Bazuca Oeste, estadoDisparoForaMapa)
    , -- Minhoca sem posição (não deve fazer nada)
      (0, Move Este, estadoMinhocaSemPosicao)
    , -- Índice de minhoca inválido (negativo)
      (-1, Move Este, estadoIndiceInvalido)
    , -- Índice de minhoca inválido (muito grande)
      (5, Move Este, estadoIndiceInvalidoGrande)
    ]

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

estadoIndiceInvalido :: Estado
estadoIndiceInvalido = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

estadoIndiceInvalidoGrande :: Estado
estadoIndiceInvalidoGrande = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

-- * Testes para melhorar cobertura

testesCoberturaMelhorada :: [(NumMinhoca,Jogada,Estado)]
testesCoberturaMelhorada =
    [ -- Movimento para fora do mapa pelo sul
      (0, Move Sul, estadoForaMapaSul)
    , -- Mina em posição com minhoca
      (0, Dispara Mina Este, estadoMinaComMinhoca)
    , -- Dinamite em posição com minhoca
      (0, Dispara Dinamite Este, estadoDinamiteComMinhoca)
    , -- Mina em posição com terreno Terra
      (0, Dispara Mina Este, estadoMinaEmTerra)
    , -- Dinamite em posição com terreno Terra
      (0, Dispara Dinamite Este, estadoDinamiteEmTerra)
    , -- Mina fora do mapa
      (0, Dispara Mina Oeste, estadoMinaForaMapa)
    , -- Dinamite fora do mapa
      (0, Dispara Dinamite Oeste, estadoDinamiteForaMapa)
    , -- Escavadora com destino Ar mas minhoca no caminho
      (0, Dispara Escavadora Este, estadoEscavadoraArComMinhoca)
    , -- Movimento para água (l >= 0, dentro do mapa, terreno = Agua)
      (0, Move Sul, estadoMovimentoParaAguaSul)
    , -- Disparo de Escavadora para destino com terreno diferente de Terra/Ar/Agua
      (0, Dispara Escavadora Este, estadoEscavadoraTerreno)
    ]

estadoForaMapaSul :: Estado
estadoForaMapaSul = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMinaComMinhoca :: Estado
estadoMinaComMinhoca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1
                       ,Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoDinamiteComMinhoca :: Estado
estadoDinamiteComMinhoca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1
                       ,Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMinaEmTerra :: Estado
estadoMinaEmTerra = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoDinamiteEmTerra :: Estado
estadoDinamiteEmTerra = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoMinaForaMapa :: Estado
estadoMinaForaMapa = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoDinamiteForaMapa :: Estado
estadoDinamiteForaMapa = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

estadoEscavadoraArComMinhoca :: Estado
estadoEscavadoraArComMinhoca = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1
                       ,Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoMovimentoParaAguaSul :: Estado
estadoMovimentoParaAguaSul = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Agua,Agua,Agua,Agua,Agua]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

estadoEscavadoraTerreno :: Estado
estadoEscavadoraTerreno = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar]
                   ,[Terra,Terra,Terra,Terra,Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2