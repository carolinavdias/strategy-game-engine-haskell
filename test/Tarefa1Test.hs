module Main where

import Labs2025
import Tarefa1
import Magic

-- ========== TESTES PARA VALIDAÇÃO DE ESTADOS ==========

testesTarefa1 :: [Estado]
testesTarefa1 = 
    testesCoberturaTotalAdicional ++
    testesPosiçãoValidaComNothing ++
    testesDisparoPosiçãoNothing ++
    testesTodasDirecoesOpostas ++
    testesObjetosIguaisFalso ++
    testesValidaTempoDisparoFalso ++
    testesRemoverDuplicadosComX ++
    testesMapaVazio ++
    testesMapaInvalido ++
    testesBarrilPosicaoInvalida ++
    testesBarrilSobreOpaco ++
    testesBarrilSobreMinhoca ++
    testesBarrilDuplicado ++
    testesBarrilValido ++
    testesDisparoJetpack ++
    testesDisparoEscavadora ++
    testesBazucaTempo ++
    testesBazucaPerfuracao ++
    testesMinaTempo ++
    testesDinamiteTempo ++
    testesDisparoDonoInvalido ++
    testesDisparosDuplicados ++
    testesMinhocaPosicaoInvalida ++
    testesMinhocaSobreOpaco ++
    testesMinhocaSobreBarril ++
    testesMinhocasDuplicadas ++
    testesMinhocaAgua ++
    testesMinhocaSemPosicao ++
    testesMinhocaVida ++
    testesMinhocaMunicao ++
    testesEstadosValidos

-- ========== TESTES DE MAPA VAZIO (5 testes) ==========

testesMapaVazio :: [Estado]
testesMapaVazio = 
    [ -- 1. Mapa completamente vazio
      Estado [] [] []
      
      -- 2. Mapa vazio com objetos
    , Estado [] [Barril (0,0) False] []
      
      -- 3. Mapa vazio com minhocas
    , Estado [] [] [Minhoca Nothing Morta 0 0 0 0 0]
      
      -- 4. Mapa vazio com objetos e minhocas
    , Estado [] [Disparo (0,0) Norte Bazuca Nothing 0] 
        [Minhoca (Just (0,0)) (Viva 100) 0 0 1 0 0]
      
      -- 5. Lista de listas vazias
    , Estado [[]] [] []
    ]

-- ========== TESTES DE MAPA INVÁLIDO (5 testes) ==========

testesMapaInvalido :: [Estado]
testesMapaInvalido = 
    [ -- 1. Linhas com tamanhos diferentes
      Estado 
        [ [Terra, Terra, Terra]
        , [Terra, Ar]  -- linha menor
        , [Terra, Terra, Terra]
        ] [] []
      
      -- 2. Primeira linha maior que as outras
    , Estado
        [ [Terra, Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ] [] []
      
      -- 3. Última linha diferente
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra]
        ] [] []
      
      -- 4. Linha do meio com tamanho diferente
    , Estado
        [ [Agua, Agua]
        , [Agua, Agua, Agua, Agua]
        , [Agua, Agua]
        ] [] []
      
      -- 5. Múltiplas linhas irregulares
    , Estado
        [ [Terra, Terra]
        , [Ar]
        , [Pedra, Pedra, Pedra]
        , [Agua]
        ] [] []
    ]

-- ========== TESTES ESPECÍFICOS PARA COBERTURA ==========

-- Testes para posicaoValidaELivreNoMapa com Nothing -> False
testesPosiçãoValidaComNothing :: [Estado]
testesPosiçãoValidaComNothing = 
    [ -- Teste que força encontraPosicaoMatriz a retornar Nothing
      Estado
        [ [Terra, Terra]
        , [Terra, Terra]
        ]
        [Barril (10, 10) False]  -- Posição fora do mapa -> Nothing
        []
    ]

-- Testes para validaPosicaoDisparo com Nothing -> False
testesDisparoPosiçãoNothing :: [Estado]
testesDisparoPosiçãoNothing = 
    [ -- Disparo em posição que retorna Nothing
      Estado
        [ [Ar, Ar]
        , [Ar, Ar]
        ]
        [Disparo (10, 10) Norte Bazuca Nothing 0]  -- Posição fora -> Nothing
        [Minhoca (Just (0, 0)) (Viva 100) 0 0 1 0 0]
    ]

-- Testes para direcaoOposta com todas as direções
testesTodasDirecoesOpostas :: [Estado]
testesTodasDirecoesOpostas = 
    [ -- Norte -> Sul
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (0, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Este -> Oeste
    , Estado
        [ [Terra, Ar, Terra]
        , [Terra, Ar, Terra]
        ]
        [Disparo (0, 1) Oeste Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Nordeste -> Sudoeste
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (0, 1) Sudoeste Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Noroeste -> Sudeste
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (0, 1) Sudeste Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
    ]

-- Testes para objetosIguais retornar False
testesObjetosIguaisFalso :: [Estado]
testesObjetosIguaisFalso = 
    [ -- Dois barris diferentes na mesma posição (False)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 1) False, Barril (1, 1) True]  -- Mesmo pos, explode diferente
        []
      
      -- Dois disparos IGUAIS (pos1==pos2 && dir1==dir2 && tipo1==tipo2 && tempo1==tempo2 && dono1==dono2)
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Bazuca Nothing 0
        , Disparo (0, 1) Norte Bazuca Nothing 0  -- Tudo igual -> True (mas é inválido por serem duplicados)
        ]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 2 0 0]
      
      -- Barril e Disparo (tipos diferentes -> False)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [ Barril (0, 1) False
        , Disparo (1, 1) Norte Mina Nothing 0
        ]
        [Minhoca (Just (0, 0)) (Viva 100) 0 0 0 1 0]
    ]

-- Testes para validaTempoDisparo retornar False
testesValidaTempoDisparoFalso :: [Estado]
testesValidaTempoDisparoFalso = 
    [ -- Bazuca com tempo (False)
      Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Bazuca (Just 3) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Mina com tempo > 2 (False)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Mina (Just 5) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 1 0]
      
      -- Dinamite sem tempo (False)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Sul Dinamite Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 1]
      
      -- Dinamite com tempo > 4 (False)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Oeste Dinamite (Just 10) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 1]
    
      -- Validar _ _ = False (qualquer outro caso)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Jetpack (Just 1) 0]  -- Jetpack com tempo -> False
        [Minhoca (Just (1, 1)) (Viva 100) 1 0 0 0 0]
    ]

-- Testes para removerDuplicados usar x no filter
testesRemoverDuplicadosComX :: [Estado]
testesRemoverDuplicadosComX = 
    [ -- Disparos duplicados que usam a função com x
      Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 0) Norte Bazuca Nothing 0
        , Disparo (0, 1) Norte Bazuca Nothing 0  -- Mesmo tipo e dono
        , Disparo (0, 2) Norte Bazuca Nothing 0  -- Mesmo tipo e dono
        ]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 3 0 0]
      
      -- Múltiplos tipos duplicados
    , Estado
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 0) Norte Mina Nothing 0
        , Disparo (0, 1) Sul Mina (Just 1) 0
        , Disparo (1, 0) Este Dinamite (Just 2) 1
        , Disparo (1, 1) Oeste Dinamite (Just 3) 1
        ]
        [ Minhoca (Just (0, 4)) (Viva 100) 0 0 0 2 0
        , Minhoca (Just (1, 4)) (Viva 100) 0 0 0 0 2
        ]
    
      -- Teste específico para cobrir x no caso recursivo
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [ Disparo (0, 0) Norte Bazuca Nothing 0
        , Disparo (0, 1) Norte Mina Nothing 0
        , Disparo (0, 2) Norte Bazuca Nothing 0  -- Duplicado de Bazuca com dono 0
        ]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 2 1 0]
    ]

-- ========== TESTES DE BARRIL EM POSIÇÃO INVÁLIDA (5 testes) ==========

testesBarrilPosicaoInvalida :: [Estado]
testesBarrilPosicaoInvalida = 
    [ -- 1. Barril fora do mapa (linha negativa)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (-1, 1) False]
        []
      
      -- 2. Barril fora do mapa (coluna negativa)
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, -1) False]
        []
      
      -- 3. Barril fora do mapa (linha muito grande)
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (10, 1) False]
        []
      
      -- 4. Barril fora do mapa (coluna muito grande)
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 10) False]
        []
      
      -- 5. Barril completamente fora
    , Estado
        [ [Pedra, Pedra]
        , [Pedra, Pedra]
        ]
        [Barril (5, 5) True]
        []
    ]

-- ========== TESTES DE BARRIL SOBRE TERRENO OPACO (5 testes) ==========

testesBarrilSobreOpaco :: [Estado]
testesBarrilSobreOpaco = 
    [ -- 1. Barril sobre Terra
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (0, 0) False]
        []
      
      -- 2. Barril sobre Pedra
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [Barril (2, 2) False]
        []
      
      -- 3. Barril sobre Terra no centro
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Terra, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (1, 1) True]
        []
      
      -- 4. Barril sobre Pedra na borda
    , Estado
        [ [Pedra, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (0, 0) False]
        []
      
      -- 5. Múltiplos barris, um sobre Terra
    , Estado
        [ [Terra, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Barril (0, 0) False, Barril (1, 2) False]
        []
    ]

-- ========== TESTES DE BARRIL SOBRE MINHOCA (5 testes) ==========

testesBarrilSobreMinhoca :: [Estado]
testesBarrilSobreMinhoca = 
    [ -- 1. Barril e minhoca na mesma posição
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 1) False]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 2. Barril explodindo sobre minhoca
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [Barril (1, 1) True]
        [Minhoca (Just (1, 1)) (Viva 50) 1 1 1 1 1]
      
      -- 3. Múltiplas minhocas, uma sob barril
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (1, 1) False]
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        ]
      
      -- 4. Barril sobre minhoca morta
    , Estado
        [ [Agua, Agua, Agua]
        , [Agua, Agua, Agua]
        ]
        [Barril (1, 1) False]
        [Minhoca (Just (1, 1)) Morta 0 0 0 0 0]
      
      -- 5. Múltiplos barris, um sobre minhoca
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Barril (0, 0) False, Barril (1, 2) False]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 0]
    ]

-- ========== TESTES DE BARRIS DUPLICADOS (5 testes) ==========

testesBarrilDuplicado :: [Estado]
testesBarrilDuplicado = 
    [ -- 1. Dois barris na mesma posição
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 1) False, Barril (1, 1) False]
        []
      
      -- 2. Barril normal e explodindo na mesma posição
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (0, 1) False, Barril (0, 1) True]
        []
      
      -- 3. Três barris na mesma posição
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [ Barril (1, 1) False
        , Barril (1, 1) True
        , Barril (1, 1) False
        ]
        []
      
      -- 4. Barris em posições diferentes e duplicados
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Barril (0, 0) False
        , Barril (1, 1) False
        , Barril (0, 0) True
        ]
        []
      
      -- 5. Múltiplos pares de barris duplicados
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [ Barril (0, 0) False, Barril (0, 0) False
        , Barril (2, 2) False, Barril (2, 2) True
        ]
        []
    ]

-- ========== TESTES DE BARRIS VÁLIDOS (5 testes) ==========

testesBarrilValido :: [Estado]
testesBarrilValido = 
    [ -- 1. Barril simples em Ar
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 1) False]
        []
      
      -- 2. Barril explodindo em Ar
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [Barril (1, 1) True]
        []
      
      -- 3. Múltiplos barris em posições diferentes
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Barril (0, 1) False
        , Barril (1, 2) True
        , Barril (2, 0) False
        ]
        []
      
      -- 4. Barril em Água
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Agua, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 1) False]
        []
      
      -- 5. Barril com minhoca próxima (não sobreposta)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (0, 0) False]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0]
    ]

-- ========== TESTES DE DISPARO JETPACK (5 testes) ==========

testesDisparoJetpack :: [Estado]
testesDisparoJetpack = 
    [ -- 1. Disparo de Jetpack
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (1, 1) Norte Jetpack Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 5 0 0 0 0]
      
      -- 2. Jetpack com tempo
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Jetpack (Just 3) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 10 0 0 0 0]
      
      -- 3. Jetpack em todas as direções
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [ Disparo (1, 1) Norte Jetpack Nothing 0
        , Disparo (1, 1) Sul Jetpack Nothing 0
        ]
        [Minhoca (Just (0, 0)) (Viva 100) 20 0 0 0 0]
      
      -- 4. Jetpack sem munições (incoerente)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Disparo (1, 1) Oeste Jetpack Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 0]
      
      -- 5. Múltiplos Jetpacks
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Jetpack Nothing 0
        , Disparo (1, 2) Sul Jetpack Nothing 1
        ]
        [ Minhoca (Just (0, 0)) (Viva 100) 5 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 3 0 0 0 0
        ]
    ]

-- ========== TESTES DE DISPARO ESCAVADORA (5 testes) ==========

testesDisparoEscavadora :: [Estado]
testesDisparoEscavadora = 
    [ -- 1. Disparo de Escavadora
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (1, 1) Norte Escavadora Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 5 0 0 0]
      
      -- 2. Escavadora com tempo
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Escavadora (Just 2) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 10 0 0 0]
      
      -- 3. Escavadora em diagonal
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (1, 1) Nordeste Escavadora Nothing 0]
        [Minhoca (Just (2, 0)) (Viva 100) 0 3 0 0 0]
      
      -- 4. Escavadora sem munições
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Disparo (1, 1) Oeste Escavadora Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 0]
      
      -- 5. Múltiplas Escavadoras
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Escavadora Nothing 0
        , Disparo (1, 2) Sul Escavadora Nothing 1
        ]
        [ Minhoca (Just (0, 0)) (Viva 100) 0 5 0 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 0 3 0 0 0
        ]
    ]

-- ========== TESTES DE BAZUCA TEMPO (5 testes) ==========

testesBazucaTempo :: [Estado]
testesBazucaTempo = 
    [ -- 1. Bazuca sem tempo (válido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (1, 1) Norte Bazuca Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 5 0 0]
      
      -- 2. Bazuca com tempo 0 (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Bazuca (Just 0) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 10 0 0]
      
      -- 3. Bazuca com tempo 5 (inválido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Disparo (1, 1) Sul Bazuca (Just 5) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 3 0 0]
      
      -- 4. Bazuca com tempo negativo (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (1, 1) Oeste Bazuca (Just (-1)) 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 2 0 0]
      
      -- 5. Múltiplas Bazucas com tempo (inválido)
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Bazuca (Just 1) 0
        , Disparo (1, 2) Sul Bazuca (Just 3) 1
        ]
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 5 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 0 0 3 0 0
        ]
    ]

-- ========== TESTES DE BAZUCA PERFURAÇÃO (5 testes) ==========

testesBazucaPerfuracao :: [Estado]
testesBazucaPerfuracao = 
    [ -- 1. Bazuca perfurando Terra na superfície (válido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (0, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 5 0 0]
      
      -- 2. Bazuca perfurando Pedra na superfície (válido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [Disparo (0, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 3 0 0]
      
      -- 3. Bazuca perfurando subsolo (inválido)
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        ]
        [Disparo (1, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (2, 1)) (Viva 100) 0 0 2 0 0]
      
      -- 4. Bazuca em Ar normal (válido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (1, 1) Norte Bazuca Nothing 0]
        [Minhoca (Just (2, 1)) (Viva 100) 0 0 4 0 0]
      
      -- 5. Bazuca perfurando horizontal (válido)
    , Estado
        [ [Terra, Terra, Ar, Ar]
        , [Terra, Terra, Ar, Ar]
        ]
        [Disparo (0, 1) Este Bazuca Nothing 0]
        [Minhoca (Just (1, 0)) (Viva 100) 0 0 1 0 0]
    ]

-- ========== TESTES DE MINA TEMPO (5 testes) ==========

testesMinaTempo :: [Estado]
testesMinaTempo = 
    [ -- 1. Mina sem tempo (válido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (1, 1) Norte Mina Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 5 0]
      
      -- 2. Mina com tempo 0 (válido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Mina (Just 0) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 3 0]
      
      -- 3. Mina com tempo 2 (válido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Disparo (1, 1) Sul Mina (Just 2) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 2 0]
      
      -- 4. Mina com tempo 3 (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (1, 1) Oeste Mina (Just 3) 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 1 0]
      
      -- 5. Mina com tempo negativo (inválido)
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Mina (Just (-1)) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 4 0]
    ]

-- ========== TESTES DE DINAMITE TEMPO (5 testes) ==========

testesDinamiteTempo :: [Estado]
testesDinamiteTempo = 
    [ -- 1. Dinamite sem tempo (inválido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (1, 1) Norte Dinamite Nothing 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 5]
      
      -- 2. Dinamite com tempo 0 (válido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Dinamite (Just 0) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 3]
      
      -- 3. Dinamite com tempo 4 (válido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Disparo (1, 1) Sul Dinamite (Just 4) 0]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
      
      -- 4. Dinamite com tempo 5 (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (1, 1) Oeste Dinamite (Just 5) 0]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 1]
      
      -- 5. Dinamite com tempo negativo (inválido)
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Dinamite (Just (-2)) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 4]
    ]

-- ========== TESTES DE DISPARO DONO INVÁLIDO (5 testes) ==========

testesDisparoDonoInvalido :: [Estado]
testesDisparoDonoInvalido = 
    [ -- 1. Dono negativo
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (1, 1) Norte Bazuca Nothing (-1)]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 5 0 0]
      
      -- 2. Dono maior que lista de minhocas
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Mina Nothing 5]
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 3 0
        , Minhoca (Just (0, 0)) (Viva 100) 0 0 0 2 0
        ]
      
      -- 3. Dono igual ao tamanho da lista
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Disparo (1, 1) Sul Dinamite (Just 3) 3]
        [ Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2
        , Minhoca (Just (1, 2)) (Viva 100) 0 0 0 0 1
        , Minhoca (Just (0, 2)) (Viva 100) 0 0 0 0 1
        ]
      
      -- 4. Dono muito grande
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (1, 1) Oeste Bazuca Nothing 100]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 1 0 0]
      
      -- 5. Lista de minhocas vazia
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Mina Nothing 0]
        []
    ]

-- ========== TESTES DE DISPAROS DUPLICADOS (5 testes) ==========

testesDisparosDuplicados :: [Estado]
testesDisparosDuplicados = 
    [ -- 1. Duas Bazucas do mesmo dono
      Estado
        [ [Terra, Terra, Terra, Terra]
        , [Terra, Ar, Ar, Terra]
        , [Terra, Terra, Terra, Terra]
        ]
        [ Disparo (1, 1) Norte Bazuca Nothing 0
        , Disparo (1, 2) Sul Bazuca Nothing 0
        ]
        [Minhoca (Just (2, 1)) (Viva 100) 0 0 5 0 0]
      
      -- 2. Duas Minas do mesmo dono
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Este Mina (Just 1) 0
        , Disparo (1, 2) Oeste Mina Nothing 0
        ]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 3 0]
      
      -- 3. Duas Dinamites do mesmo dono
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [ Disparo (1, 1) Sul Dinamite (Just 2) 0
        , Disparo (2, 1) Norte Dinamite (Just 4) 0
        ]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 2]
      
      -- 4. Múltiplos tipos duplicados
    , Estado
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Bazuca Nothing 0
        , Disparo (0, 2) Sul Bazuca Nothing 0
        , Disparo (1, 1) Este Mina Nothing 1
        , Disparo (1, 2) Oeste Mina (Just 1) 1
        ]
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 3 2 0
        , Minhoca (Just (1, 0)) (Viva 100) 0 0 2 3 0
        ]
      
      -- 5. Três disparos do mesmo tipo e dono
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Bazuca Nothing 0
        , Disparo (1, 1) Sul Bazuca Nothing 0
        , Disparo (2, 1) Este Bazuca Nothing 0
        ]
        [Minhoca (Just (1, 2)) (Viva 100) 0 0 5 0 0]
    ]

-- ========== TESTES DE MINHOCA POSIÇÃO INVÁLIDA (5 testes) ==========

testesMinhocaPosicaoInvalida :: [Estado]
testesMinhocaPosicaoInvalida = 
    [ -- 1. Linha negativa
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (-1, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 2. Coluna negativa
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (1, -1)) (Viva 50) 1 1 1 1 1]
      
      -- 3. Linha fora do mapa
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        []
        [Minhoca (Just (10, 1)) (Viva 75) 2 2 2 2 2]
      
      -- 4. Coluna fora do mapa
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (1, 10)) (Viva 100) 0 0 0 0 0]
      
      -- 5. Ambas coordenadas fora
    , Estado
        [ [Terra, Terra]
        , [Terra, Terra]
        ]
        []
        [Minhoca (Just (5, 5)) (Viva 100) 5 5 5 5 5]
    ]

-- ========== TESTES DE MINHOCA SOBRE OPACO (5 testes) ==========

testesMinhocaSobreOpaco :: [Estado]
testesMinhocaSobreOpaco = 
    [ -- 1. Minhoca sobre Terra
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0]
      
      -- 2. Minhoca sobre Pedra
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        []
        [Minhoca (Just (2, 2)) (Viva 50) 1 1 1 1 1]
      
      -- 3. Minhoca sobre Terra no centro
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Terra, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 75) 2 2 2 2 2]
      
      -- 4. Múltiplas minhocas, uma sobre opaco
    , Estado
        [ [Terra, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        ]
      
      -- 5. Minhoca sobre Pedra na borda
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Pedra]
        ]
        []
        [Minhoca (Just (1, 3)) (Viva 100) 5 5 5 5 5]
    ]

-- ========== TESTES DE MINHOCA SOBRE BARRIL (5 testes) ==========

testesMinhocaSobreBarril :: [Estado]
testesMinhocaSobreBarril = 
    [ -- 1. Minhoca e barril na mesma posição
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Barril (1, 1) False]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 2. Minhoca sobre barril explodindo
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (0, 1) True]
        [Minhoca (Just (0, 1)) (Viva 50) 1 1 1 1 1]
      
      -- 3. Múltiplas minhocas, uma sobre barril
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        [Barril (1, 1) False]
        [ Minhoca (Just (2, 1)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        ]
      
      -- 4. Minhoca morta sobre barril
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Barril (1, 1) False]
        [Minhoca (Just (1, 1)) Morta 0 0 0 0 0]
      
      -- 5. Múltiplos barris, minhoca sobre um
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Barril (0, 0) False, Barril (1, 2) False]
        [Minhoca (Just (0, 0)) (Viva 100) 5 5 5 5 5]
    ]

-- ========== TESTES DE MINHOCAS DUPLICADAS (5 testes) ==========

testesMinhocasDuplicadas :: [Estado]
testesMinhocasDuplicadas = 
    [ -- 1. Duas minhocas na mesma posição
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 50) 0 0 0 0 0
        ]
      
      -- 2. Três minhocas na mesma posição
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [ Minhoca (Just (0, 1)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (0, 1)) (Viva 75) 2 2 2 2 2
        , Minhoca (Just (0, 1)) (Viva 50) 3 3 3 3 3
        ]
      
      -- 3. Minhoca viva e morta na mesma posição
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        []
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) Morta 0 0 0 0 0
        ]
      
      -- 4. Minhocas em posições diferentes e duplicadas
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        []
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (0, 0)) (Viva 50) 0 0 0 0 0
        ]
      
      -- 5. Múltiplos pares duplicados
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [ Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (0, 0)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (2, 2)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (2, 2)) (Viva 100) 0 0 0 0 0
        ]
    ]

-- ========== TESTES DE MINHOCA EM ÁGUA (5 testes) ==========

testesMinhocaAgua :: [Estado]
testesMinhocaAgua = 
    [ -- 1. Minhoca viva em Água (inválido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Agua, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 2. Minhoca morta em Água (válido)
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Agua, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (1, 1)) Morta 0 0 0 0 0]
      
      -- 3. Minhoca viva em Água com vida baixa
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Agua, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 1) 0 0 0 0 0]
      
      -- 4. Múltiplas minhocas, uma viva em Água
    , Estado
        [ [Ar, Agua, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [ Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0
        , Minhoca (Just (0, 1)) (Viva 50) 0 0 0 0 0
        ]
      
      -- 5. Minhoca morta em Água com munições
    , Estado
        [ [Agua, Agua]
        , [Agua, Agua]
        ]
        []
        [Minhoca (Just (0, 0)) Morta 5 5 5 5 5]
    ]

-- ========== TESTES DE MINHOCA SEM POSIÇÃO (5 testes) ==========

testesMinhocaSemPosicao :: [Estado]
testesMinhocaSemPosicao = 
    [ -- 1. Minhoca sem posição e morta (válido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca Nothing Morta 0 0 0 0 0]
      
      -- 2. Minhoca sem posição e viva (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca Nothing (Viva 100) 0 0 0 0 0]
      
      -- 3. Minhoca sem posição, viva com vida baixa
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        []
        [Minhoca Nothing (Viva 1) 0 0 0 0 0]
      
      -- 4. Múltiplas minhocas sem posição
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [ Minhoca Nothing Morta 0 0 0 0 0
        , Minhoca Nothing Morta 0 0 0 0 0
        ]
      
      -- 5. Minhoca sem posição com munições e viva
    , Estado
        [ [Terra, Terra]
        , [Terra, Terra]
        ]
        []
        [Minhoca Nothing (Viva 50) 5 5 5 5 5]
    ]

-- ========== TESTES DE VIDA DE MINHOCA (5 testes) ==========

testesMinhocaVida :: [Estado]
testesMinhocaVida = 
    [ -- 1. Vida = 101 (inválido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 101) 0 0 0 0 0]
      
      -- 2. Vida = -1 (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (0, 1)) (Viva (-1)) 1 1 1 1 1]
      
      -- 3. Vida = 0 (válido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 0) 2 2 2 2 2]
      
      -- 4. Vida = 100 (válido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 5. Vida = 1000 (inválido)
    , Estado
        [ [Terra, Terra]
        , [Terra, Ar]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 1000) 5 5 5 5 5]
    ]

-- ========== TESTES DE MUNIÇÃO DE MINHOCA (5 testes) ==========

testesMinhocaMunicao :: [Estado]
testesMinhocaMunicao = 
    [ -- 1. Jetpack negativo (inválido)
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) (-1) 0 0 0 0]
      
      -- 2. Escavadora negativa (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (0, 1)) (Viva 100) 0 (-5) 0 0 0]
      
      -- 3. Bazuca negativa (inválido)
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 (-10) 0 0]
      
      -- 4. Mina negativa (inválido)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 (-2) 0]
      
      -- 5. Dinamite negativa (inválido)
    , Estado
        [ [Terra, Terra]
        , [Terra, Ar]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 (-1)]
    ]

-- ========== TESTES DE ESTADOS VÁLIDOS COMPLEXOS (10 testes) ==========

testesEstadosValidos :: [Estado]
testesEstadosValidos = 
    [ -- 1. Estado válido simples
      Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        []
        [Minhoca (Just (1, 1)) (Viva 100) 5 5 5 5 5]
      
      -- 2. Estado com barril válido
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [Barril (1, 1) False]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 3. Estado com disparo de Bazuca
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 3 0 0]
      
      -- 4. Estado com Mina e Dinamite
    , Estado
        [ [Terra, Terra, Terra, Terra]
        , [Terra, Ar, Ar, Terra]
        , [Terra, Terra, Terra, Terra]
        ]
        [ Disparo (1, 1) Este Mina (Just 2) 0
        , Disparo (1, 2) Oeste Dinamite (Just 3) 1
        ]
        [ Minhoca (Just (0, 1)) (Viva 100) 0 0 0 5 0
        , Minhoca (Just (0, 2)) (Viva 75) 0 0 0 0 3
        ]
      
      -- 5. Estado complexo com tudo
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
        , Minhoca (Just (2, 2)) Morta 0 0 0 0 0
        ]
      
      -- 6. Estado com múltiplas minhocas vivas
    , Estado
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        ]
        []
        [ Minhoca (Just (0, 0)) (Viva 100) 10 10 10 10 10
        , Minhoca (Just (1, 1)) (Viva 90) 8 8 8 8 8
        , Minhoca (Just (2, 2)) (Viva 80) 6 6 6 6 6
        , Minhoca (Just (0, 4)) (Viva 70) 4 4 4 4 4
        ]
      
      -- 7. Estado com múltiplos barris
    , Estado
        [ [Terra, Terra, Terra, Terra]
        , [Terra, Ar, Ar, Terra]
        , [Terra, Ar, Ar, Terra]
        , [Terra, Terra, Terra, Terra]
        ]
        [ Barril (1, 1) False
        , Barril (1, 2) False
        , Barril (2, 1) True
        , Barril (2, 2) False
        ]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 0 0]
      
      -- 8. Estado com Bazuca perfurando
    , Estado
        [ [Terra, Terra, Terra]
        , [Terra, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
        [Disparo (0, 1) Sul Bazuca Nothing 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 5 0 0]
      
      -- 9. Estado com minhoca morta sem posição
    , Estado
        [ [Pedra, Pedra, Pedra]
        , [Pedra, Ar, Pedra]
        , [Pedra, Pedra, Pedra]
        ]
        [Barril (1, 1) False]
        [ Minhoca (Just (0, 1)) (Viva 100) 5 5 5 5 5
        , Minhoca Nothing Morta 0 0 0 0 0
        ]
      
      -- 10. Jogo realista completo
    , Estado
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Terra, Terra, Terra, Ar, Pedra]
        , [Pedra, Ar, Terra, Ar, Terra, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
        [ Barril (2, 3) False
        , Barril (4, 4) True
        , Disparo (1, 2) Sul Bazuca Nothing 0
        , Disparo (4, 2) Norte Mina (Just 1) 1
        ]
        [ Minhoca (Just (1, 1)) (Viva 100) 10 5 3 2 1
        , Minhoca (Just (1, 5)) (Viva 85) 8 4 2 1 1
        , Minhoca (Just (4, 5)) (Viva 60) 5 3 1 0 0
        , Minhoca Nothing Morta 0 0 0 0 0
        ]
    ]



testesPosicaoNothingFalse :: [Estado]
testesPosicaoNothingFalse = 
    [ -- Barril em posição COMPLETAMENTE inválida (fora dos limites)
      Estado
        [ [Ar, Ar]
        , [Ar, Ar]
        ]
        [Barril (1, 1) False]  -- Muito fora -> Nothing em encontraPosicaoMatriz
        []
    ]

-- Imagem 1: validaPosicaoDisparo Bazuca - linha "Nothing -> False"  
testesBazucaPosicaoNothing :: [Estado]
testesBazucaPosicaoNothing = 
    [ -- Disparo Bazuca completamente fora do mapa
      Estado
        [ [Ar, Ar]
        , [Ar, Ar]
        ]
        [Disparo (1, 1) Norte Bazuca Nothing 0]  -- Fora -> Nothing
        [Minhoca (Just (0, 0)) (Viva 100) 0 0 1 0 0]
    ]

testesDirecaoOpostaCompleta :: [Estado]
testesDirecaoOpostaCompleta = 
    [ -- Forçar Norte -> Sul (bazuca perfurando de Norte, verifica Sul)
      Estado
        [ [Terra, Terra, Terra]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Sul Bazuca Nothing 0]  -- Em Terra, vem de Norte
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]

    , Estado
        [ [Terra, Terra, Terra]
        , [Ar, Ar, Ar]
        , [Ar, Terra, Ar]
        ]
        [Disparo (2, 1) Norte Bazuca Nothing 0]  
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Forçar Este -> Oeste
    , Estado
        [ [Terra, Ar, Ar]
        , [Terra, Ar, Ar]
        ]
        [Disparo (0, 0) Oeste Bazuca Nothing 0]  -- Em Terra, vem de Este
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Forçar Nordeste -> Sudoeste
    , Estado
        [ [Ar, Ar, Terra]
        , [Ar, Ar, Ar]
        , [Terra, Ar, Ar]
        ]
        [Disparo (0, 2) Sudoeste Bazuca Nothing 0]  -- Em Terra, vem de Nordeste
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Forçar Noroeste -> Sudeste
    , Estado
        [ [Terra, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Ar, Ar, Terra]
        ]
        [Disparo (0, 0) Sudeste Bazuca Nothing 0]  -- Em Terra, vem de Noroeste
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Forçar Sudoeste -> Nordeste
    , Estado
        [ [Ar, Ar, Terra]
        , [Ar, Ar, Ar]
        , [Terra, Ar, Ar]
        ]
        [Disparo (0, 2) Noroeste Bazuca Nothing 0]  
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]

    , Estado
        [ [Ar, Ar, Terra]
        , [Ar, Ar, Ar]
        , [Terra, Ar, Ar]
        ]
        [Disparo (2, 0) Nordeste Bazuca Nothing 0]  -- Em Terra, vem de Sudoeste
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]

    ]

-- IMAGEM 2: objetosIguais - forçar os ramos True e False específicos
testesObjetosIguaisRamos :: [Estado]
testesObjetosIguaisRamos = 
    [ -- Dois Barris EXATAMENTE iguais: pos1 == pos2 && e1 == e2 -> True
      Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [ Barril (1, 1) True
        , Barril (1, 1) True  -- Mesma posição, mesmo explode -> True
        ]
        []
      
      -- Dois Disparos EXATAMENTE iguais (todas as condições True)
    , Estado
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 1) Norte Bazuca Nothing 0
        , Disparo (0, 1) Norte Bazuca Nothing 0  -- Tudo igual -> True
        ]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 2 0 0]
      
      -- Diferentes tipos de objetos: _ _ = False
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [ Barril (0, 0) False
        , Disparo (1, 1) Norte Mina Nothing 0  -- Barril vs Disparo -> False
        ]
        [Minhoca (Just (0, 1)) (Viva 100) 0 0 0 1 0]
    ]

-- IMAGEM 2: validaTempoDisparo - cobrir TODAS as linhas amarelas
testesValidaTempoDisparoCompleto :: [Estado]
testesValidaTempoDisparoCompleto = 
    [ -- Bazuca (Just _) = False
      Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Bazuca (Just 1) 0]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 1 0 0]
      
      -- Mina (Just t) com t válido (>= 0 && <= 2) -> True
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Este Mina (Just 1) 0]  -- 1 está no intervalo -> True
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 1 0]
      
      -- Dinamite (Just t) com t válido (>= 0 && <= 4) -> True
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Sul Dinamite (Just 2) 0]  -- 2 está no intervalo -> True
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 1]
      
      -- Dinamite Nothing = False
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Oeste Dinamite Nothing 0]  -- Sem tempo -> False
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 0 0 1]
      
      -- _ _ = False (outro tipo com tempo)
    , Estado
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
        [Disparo (0, 1) Norte Jetpack Nothing 0]  -- Jetpack não deve ter tempo -> False
        [Minhoca (Just (1, 1)) (Viva 100) 1 0 0 0 0]
    ]

-- IMAGEM 2: removerDuplicados - forçar o caso recursivo com 'x'
-- removerDuplicados (x:xs) = x : removerDuplicados (filter (/= x) xs)
testesRemoverDuplicadosRecursivo :: [Estado]
testesRemoverDuplicadosRecursivo = 
    [ -- Lista com duplicados que força o filter (/= x)
      Estado
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 0) Norte Bazuca Nothing 0      -- x (mantido)
        , Disparo (0, 0) Norte Bazuca Nothing 0      -- duplicado (removido por filter)
        , Disparo (0, 2) Sul Mina (Just 1) 0        -- diferente (mantido)
        , Disparo (0, 0) Norte Bazuca Nothing 0      -- duplicado (removido)
        , Disparo (0, 4) Este Dinamite (Just 3) 0   -- diferente (mantido)
        ]
        [Minhoca (Just (1, 1)) (Viva 100) 0 0 3 1 1]
      
      -- Outro caso forçando múltiplas chamadas recursivas
    , Estado
        [ [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar, Ar]
        ]
        [ Disparo (0, 0) Norte Mina Nothing 0        -- x1
        , Disparo (0, 1) Norte Mina (Just 2) 0       -- dup x1
        , Disparo (0, 2) Sul Dinamite (Just 1) 1     -- x2 (diferente)
        , Disparo (0, 3) Norte Mina Nothing 0        -- dup x1
        , Disparo (0, 4) Sul Dinamite (Just 2) 1     -- dup x2
        , Disparo (0, 5) Oeste Bazuca Nothing 0      -- x3 (diferente)
        ]
        [ Minhoca (Just (1, 0)) (Viva 100) 0 0 1 3 0
        , Minhoca (Just (1, 5)) (Viva 100) 0 0 0 0 2
        ]
    ]

-- Lista completa de testes adicionais para cobertura
testesCoberturaTotalAdicional :: [Estado]
testesCoberturaTotalAdicional = 
    testesPosicaoNothingFalse ++
    testesBazucaPosicaoNothing ++
    testesDirecaoOpostaCompleta ++
    testesObjetosIguaisRamos ++
    testesValidaTempoDisparoCompleto ++
    testesRemoverDuplicadosRecursivo

-- ========== MAIN ==========

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1