module Main where

import Labs2025
import Tarefa3
import Magic

--------------------------------------------------------------------------------
-- Mapas de apoio (médios, estilo do enunciado)
--------------------------------------------------------------------------------

-- | Mapa 6x10 do enunciado.
mapaEnunciado :: Mapa
mapaEnunciado =
  [ [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
  ]

-- | Mapa 6x10 com chão ao centro e poça à direita.
mapaChaoCentroAgua :: Mapa
mapaChaoCentroAgua =
  [ [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Agua,Agua]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Pedra,Agua,Agua]
  ]

-- | Mapa 7x9 com “mesa” e um bloco de pedra no meio.
mapaMesa :: Mapa
mapaMesa =
  [ [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Terra,Terra,Terra,Ar,Ar,Ar]
  , [Ar,Ar,Ar,Terra,Pedra,Terra,Ar,Ar,Ar]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Minhoca viva a 100 de vida, sem munições (não interessam na T3).
mkViva :: Posicao -> Minhoca
mkViva p = Minhoca (Just p) (Viva 100) 0 0 0 0 0

--------------------------------------------------------------------------------
-- Exemplos do enunciado
--------------------------------------------------------------------------------

-- | Exemplo 'e' do enunciado (não entra na lista de testes).
exemplo_e :: Estado
exemplo_e =
  let o1 = Barril (3,0) False
      o2 = Disparo (3,3) Oeste Bazuca Nothing 0
      m1 = mkViva (3,4)
      m2 = mkViva (2,0)
  in Estado mapaEnunciado [o1,o2] [m1,m2]

-- | Exemplo 'e'' (input correto para o teste 0): inclui o barril.
exemplo_e' :: Estado
exemplo_e' =
  let o1 = Barril (3,0) False
      o2 = Disparo (3,3) Oeste Bazuca Nothing 0
      m1 = mkViva (3,4)
      m2' = mkViva (1,1)
  in Estado mapaEnunciado [o1,o2] [m1,m2']

--------------------------------------------------------------------------------
-- Testes (cada um é um Estado de entrada)
--------------------------------------------------------------------------------

-- | Teste 1: Minhoca sobre Ar cai 1 bloco (gravidade simples).
testeQuedaSimples :: Estado
testeQuedaSimples =
  Estado mapaChaoCentroAgua [] [ mkViva (2,5) ]

testeSemQueda :: Estado
testeSemQueda =
  Estado mapaChaoCentroAgua [] [ mkViva (4,1) ]

testeMinhocaSemPosicao :: Estado
testeMinhocaSemPosicao =
  Estado mapaChaoCentroAgua [] [ Minhoca Nothing (Viva 100) 0 0 0 0 0 ]

testeMinhocaMorta :: Estado
testeMinhocaMorta =
  Estado mapaChaoCentroAgua [] [ Minhoca (Just (4,8)) Morta 0 0 0 0 0 ]


-- | Teste 2: Minhoca cai na Água → morre mas fica com a posição nova.
testeQuedaParaAgua :: Estado
testeQuedaParaAgua =
  Estado mapaChaoCentroAgua [] [ mkViva (3,8) ]


-- | Teste: Minhoca morta cai na água (deve manter posição e continuar morta)
testeMinhocaMortaCaiAgua :: Estado
testeMinhocaMortaCaiAgua =
  Estado mapaChaoCentroAgua 
    [] 
    [ Minhoca (Just (3,8)) Morta 0 0 0 0 0 ]


-- | Teste: Minhoca morta na água não morre de novo
testeMinhocaMortaNaAgua :: Estado
testeMinhocaMortaNaAgua =
  Estado mapaChaoCentroAgua
    []
    [ Minhoca (Just (4,8)) Morta 0 0 0 0 0 ]

-- | Teste: Minhoca viva com posição Nothing
testeMinhocaVivaPosiçãoNada :: Estado
testeMinhocaVivaPosiçãoNada =
  Estado mapaChaoCentroAgua
    []
    [ Minhoca Nothing (Viva 50) 0 0 0 0 0 ]

-- | Teste 3: Minhoca cai fora do mapa → perde posição e morre.
testeQuedaForaMapa :: Estado
testeQuedaForaMapa =
  let m = [ [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
          , [Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar] ]
  in Estado m [] [ mkViva (1,5) ]

-- | Teste 4 (corrigido): Barril suspenso no ar explode neste tick.
testeBarrilSuspensoNaoExplode :: Estado
testeBarrilSuspensoNaoExplode =
  Estado mapaChaoCentroAgua
    [ Barril (1,1) True ]
    [ mkViva (4,0) ]

-- | Teste 5: Barril já prestes a explodir → aplica danos (mapa e minhoca).
testeBarrilDanoTerrenoMinhoca :: Estado
testeBarrilDanoTerrenoMinhoca =
  Estado mapaEnunciado
    [ Barril (3,5) True ]
    [ mkViva (3,7) ]

-- | Teste: Barril em posição válida mas não deve cair (não está no ar/água)
testeBarrilNaoDeveCair :: Estado
testeBarrilNaoDeveCair =
  Estado mapaEnunciado
    [ Barril (4,1) False ]
    [ mkViva (3,3) ]

-- | Teste: Barril em posição não livre no mapa (deve explodir)
testeBarrilPosicaoNaoLivre :: Estado
testeBarrilPosicaoNaoLivre =
  Estado mapaEnunciado
    [ Barril (4,0) False ]  -- Barril sobre Terra
    []

-- | Teste 6 (corrigido): Bazuca parada; minhoca cai 1 bloco.
testeBazucaParadaMinhocaCai :: Estado
testeBazucaParadaMinhocaCai =
  Estado mapaEnunciado
    [ Disparo (2,7) Oeste Bazuca Nothing 0 ]
    [ mkViva (4,4) ]

-- | Teste 7: Bazuca bate em Terra e explode.
testeBazucaBateExplode :: Estado
testeBazucaBateExplode =
  Estado mapaMesa
    [ Disparo (3,6) Oeste Bazuca Nothing 0 ]
    [ mkViva (2,6) ]

-- | Teste: Bazuca sai do mapa (retorna lista vazia de danos)
testeBazucaSaiMapa :: Estado
testeBazucaSaiMapa =
  let m = [ [Ar,Ar,Ar]
          , [Ar,Ar,Ar] ]
  in Estado m
    [ Disparo (0,2) Este Bazuca Nothing 0 ]
    [ mkViva (1,0) ]

testeBazucaNumaBazuca :: Estado
testeBazucaNumaBazuca = 
  Estado mapaMesa
    [ Disparo (3,6) Oeste Bazuca Nothing 0 
    , Disparo (3,6) Oeste Bazuca Nothing 0
    ]
    [ mkViva (2,6) ]

-- | Teste 8 (corrigido): Mina ativa por inimigo na área.
testeMinaAtivaPorInimigo :: Estado
testeMinaAtivaPorInimigo =
  Estado mapaChaoCentroAgua
    [ Disparo (2,7) Norte Mina Nothing 0 ]
    [ mkViva (3,4)   -- dono
    , mkViva (3,6)   -- inimiga
    ]

-- | Teste 9 (corrigido): Mina em queda (tempo Just 2) desce e vira Norte; tempo-1.
testeMinaCaiEContinuaDesativada :: Estado
testeMinaCaiEContinuaDesativada =
  Estado mapaChaoCentroAgua
    [ Disparo (2,5) Sul Mina (Just 2) 0 ]
    [ mkViva (5,0) ]

-- | Teste 10: Mina com tempo 0 explode (diâmetro 3).
testeMinaTempo0Explode :: Estado
testeMinaTempo0Explode =
  Estado mapaChaoCentroAgua
    [ Disparo (4,2) Norte Mina (Just 0) 0 ]
    [ mkViva (4,3) ]

-- | Teste: Mina sem tempo (Nothing) mas sem inimigos próximos - não ativa
testeMinaSemInimigosPerto :: Estado
testeMinaSemInimigosPerto =
  Estado mapaChaoCentroAgua
    [ Disparo (4,2) Norte Mina Nothing 0 ]
    [ mkViva (4,2)   -- dono na mesma posição
    , mkViva (1,1)   -- longe
    ]

-- | Teste: Mina nova posição inválida - mantém posição anterior
testeMinaPosicaoInvalida :: Estado
testeMinaPosicaoInvalida =
  let m = [ [Ar,Ar]
          , [Terra,Terra] ]
  in Estado m
    [ Disparo (1,1) Sul Mina (Just 1) 0 ]
    [ mkViva (0,0) ]

-- | Teste 11: Dinamite em parábola (no ar) – movimento Este → Sudeste.
testeDinamiteParabola :: Estado
testeDinamiteParabola =
  Estado mapaMesa
    [ Disparo (1,7) Este Dinamite (Just 3) 0 ]
    [ mkViva (2,7) ]

-- | Teste 12 : Dinamite no chão com tempo 1.
testeDinamiteNoChaoFicaNorte :: Estado
testeDinamiteNoChaoFicaNorte =
  Estado mapaChaoCentroAgua
    [ Disparo (6,3) Norte Dinamite (Just 1) 0 ]
    [ mkViva (3,3) ]

-- | Teste: Dinamite no chão (Terra) - deve parar e apontar Norte
testeDinamiteParaNoChaoTerra :: Estado
testeDinamiteParaNoChaoTerra =
  Estado mapaEnunciado
    [ Disparo (4,0) Sul Dinamite (Just 2) 0 ]
    [ mkViva (2,2) ]

-- | Teste: Dinamite no chão (Pedra) - deve parar e apontar Norte
testeDinamiteParaNoChaoPedra :: Estado
testeDinamiteParaNoChaoPedra =
  Estado mapaEnunciado
    [ Disparo (5,5) Oeste Dinamite (Just 1) 0 ]
    [ mkViva (3,3) ]



-- | Teste: Dinamite direção Sudeste
testeDinamiteDirecaoSudeste :: Estado
testeDinamiteDirecaoSudeste =
  Estado mapaMesa
    [ Disparo (2,5) Sudeste Dinamite (Just 3) 0 ]
    [ mkViva (5,5) ]

-- | Teste: Dinamite direção Sudoeste
testeDinamiteDirecaoSudoeste :: Estado
testeDinamiteDirecaoSudoeste =
  Estado mapaMesa
    [ Disparo (2,3) Sudoeste Dinamite (Just 3) 0 ]
    [ mkViva (5,1) ]

-- | Teste: Dinamite direção Norte (gravidade aplica)
testeDinamiteDirecaoNorte :: Estado
testeDinamiteDirecaoNorte =
  Estado mapaMesa
    [ Disparo (2,4) Norte Dinamite (Just 2) 0 ]
    [ mkViva (5,4) ]


-- | Teste: Dinamite direção Sul 
testeDinamiteDirecaoSul :: Estado
testeDinamiteDirecaoSul =
  Estado mapaMesa
    [ Disparo (2,4) Sul Dinamite (Just 2) 0 ]
    [ mkViva (5,7) ]

-- | Teste: Dinamite direção Nordeste
testeDinamiteDirecaoNordeste :: Estado
testeDinamiteDirecaoNordeste =
  Estado mapaMesa
    [ Disparo (2,3) Nordeste Dinamite (Just 2) 0 ]
    [ mkViva (5,6) ]



-- | Teste: Dinamite direção Noroeste
testeDinamiteDirecaoNoroeste :: Estado
testeDinamiteDirecaoNoroeste =
  Estado mapaMesa
    [ Disparo (2,5) Noroeste Dinamite (Just 2) 0 ]
    [ mkViva (5,2) ]

-- | Teste: Dinamite sai do mapa (retorna lista vazia)
testeDinamiteSaiMapa :: Estado
testeDinamiteSaiMapa =
  let m = [ [Ar,Ar,Ar]
          , [Ar,Ar,Ar] ]
  in Estado m
    [ Disparo (0,2) Este Dinamite (Just 1) 0 ]
    [ mkViva (1,0) ]

testeDinamiteRecenteLancada :: Estado
testeDinamiteRecenteLancada =
  Estado mapaMesa
    [ Disparo (2,4) Este Dinamite Nothing 0 ]
    [ mkViva (5,4) ]

-- | Teste 13 (corrigido): Explosão de bazuca afeta duas minhocas.
testeBazucaExplodeAfetaMinhocas :: Estado
testeBazucaExplodeAfetaMinhocas =
  Estado mapaChaoCentroAgua
    [ Disparo (5,6) Norte Bazuca Nothing 0 ]
    [ mkViva (3,6)
    , mkViva (4,6)
    ]

-- | Teste 14: Explosão de dinamite (diâmetro 9) grande área.
testeDinamiteExplodeGrandeArea :: Estado
testeDinamiteExplodeGrandeArea =
  Estado mapaChaoCentroAgua
    [ Disparo (5,8) Norte Dinamite (Just 0) 0 ]
    [ mkViva (3,7)
    , mkViva (4,8)
    ]

-- | Teste 16: Nada muda – minhocas estáveis em chão, sem objetos.
testeNadaMuda :: Estado
testeNadaMuda =
  Estado mapaMesa
    []
    [ mkViva (5,1), mkViva (5,7) ]

-- | Teste 17 (corrigido): Mina no chão; minhoca na posição correta.
testeMinaNoChaoApontaNorteCorrigido :: Estado
testeMinaNoChaoApontaNorteCorrigido =
  Estado mapaMesa
    [ Disparo (5,5) Norte Mina Nothing 0 ]
    [ mkViva (2,3) ]

-- | Teste 18: Sem “minhoca extra” – cenário simples com 1 minhoca.
testeSemExtraMinhoca :: Estado
testeSemExtraMinhoca =
  Estado mapaChaoCentroAgua
    []
    [ mkViva (3,5) ]


-- | Teste: Minhoca com dano zero ou negativo - não deve alterar
testeMinhocaDanoZero :: Estado
testeMinhocaDanoZero =
  Estado mapaEnunciado
    [ Disparo (5,9) Norte Bazuca Nothing 0 ]
    [ mkViva (3,3) ]  -- longe da explosão

-- | Teste: Objeto que não é Barril/Disparo (deve retornar Left obj)
testeOutroObjeto :: Estado
testeOutroObjeto =
  Estado mapaChaoCentroAgua
    []
    [ mkViva (4,4) ]

-- | Teste: Explosão com raio ímpar para testar cálculo de dano
testeExplosaoRaioImpar :: Estado
testeExplosaoRaioImpar =
  Estado mapaEnunciado
    [ Disparo (4,5) Norte Mina (Just 0) 0 ]  -- Mina raio 3
    [ mkViva (4,6)   -- cardeal
    , mkViva (5,6)   -- diagonal
    ]

-- | Teste: Minhoca recebe dano mas não morre
testeMinhocaRecebeManoDanoNaoMorre :: Estado
testeMinhocaRecebeManoDanoNaoMorre =
  Estado mapaEnunciado
    [ Disparo (3,3) Norte Bazuca Nothing 0 ]
    [ Minhoca (Just (3,5)) (Viva 100) 0 0 0 0 0 ]  -- Longe, dano pequeno

-- | Teste: Posição diagonal na explosão
testeExplosaoDiagonal :: Estado
testeExplosaoDiagonal =
  Estado mapaEnunciado
    [ Barril (4,4) True ]
    [ mkViva (5,5)   -- diagonal ao barril
    , mkViva (4,5)   -- cardeal ao barril
    ]

-- | Teste: Minhoca exatamente no centro da explosão
testeMinhocaCentroExplosao :: Estado
testeMinhocaCentroExplosao =
  Estado mapaEnunciado
    [ Barril (4,3) True ]
    [ mkViva (4,3) ]  -- mesma posição

-- | Teste: Barril atingido por explosão deve ativar
testeBarrilAtingidoPorExplosao :: Estado
testeBarrilAtingidoPorExplosao =
  Estado mapaEnunciado
    [ Barril (4,3) False
    , Disparo (4,5) Oeste Bazuca Nothing 0
    ]
    [ mkViva (2,2) ]

-- | Teste: Múltiplos barris em cascata
testeBarrisCascata :: Estado
testeBarrisCascata =
  Estado mapaEnunciado
    [ Barril (4,2) True
    , Barril (4,4) False
    ]
    [ mkViva (5,0) ]

-- | Teste: Mina caindo para posição com Pedra (chão)
testeMinaCaiParaPedra :: Estado
testeMinaCaiParaPedra =
  Estado mapaEnunciado
    [ Disparo (4,5) Sul Mina (Just 2) 0 ]
    [ mkViva (3,3) ]

-- | Teste: Mina já no chão de Terra
testeMinaNoChaoTerra :: Estado
testeMinaNoChaoTerra =
  Estado mapaEnunciado
    [ Disparo (4,0) Norte Mina Nothing 0 ]
    [ mkViva (2,2) ]

-- | Teste: Minhoca com vida exatamente zero após dano
testeMinhocaVidaExatamenteZero :: Estado
testeMinhocaVidaExatamenteZero =
  Estado mapaEnunciado
    [ Barril (4,2) True ]
    [ Minhoca (Just (4,2)) (Viva 50) 0 0 0 0 0 ]  -- vai receber 50 de dano

-- | Teste: Objeto genérico (não Barril/Disparo) - testando o catch-all
testeObjetoGenerico :: Estado
testeObjetoGenerico =
  Estado mapaChaoCentroAgua
    []
    [ mkViva (4,4) ]

-- | Teste: Dano zero em minhoca (explosão muito longe)
testeDanoZeroMinhoca :: Estado
testeDanoZeroMinhoca =
  Estado mapaEnunciado
    [ Disparo (0,0) Norte Bazuca Nothing 0 ]  -- muito longe
    [ mkViva (5,9) ]

-- | Teste: Barril na água (deve cair/explodir)
testeBarrilNaAgua :: Estado
testeBarrilNaAgua =
  Estado mapaChaoCentroAgua
    [ Barril (4,8) False ]  -- posição com água
    [ mkViva (3,3) ]

-- | Teste: Minhoca morta sem posição
testeMinhocaMortaSemPosicao :: Estado
testeMinhocaMortaSemPosicao =
  Estado mapaChaoCentroAgua
    []
    [ Minhoca Nothing Morta 0 0 0 0 0 ]

-- | Teste: Explosão afeta apenas terreno (sem minhocas)
testeExplosaoApenasTerreno :: Estado
testeExplosaoApenasTerreno =
  Estado mapaEnunciado
    [ Barril (4,2) True ]
    []  -- sem minhocas


--------------------------------------------------------------------------------
-- Lista para o feedback automático
--------------------------------------------------------------------------------

testesTarefa3 :: [Estado]
testesTarefa3 =
  [ -- 0
    exemplo_e'
  , exemplo_e
  , -- 1..18
    testeQuedaSimples
  , testeQuedaParaAgua
  , testeQuedaForaMapa
  , testeBarrilSuspensoNaoExplode
  , testeBarrilDanoTerrenoMinhoca
  , testeBazucaParadaMinhocaCai
  , testeBazucaBateExplode
  , testeMinaAtivaPorInimigo
  , testeMinaCaiEContinuaDesativada
  , testeMinaTempo0Explode
  , testeDinamiteParabola
  , testeDinamiteNoChaoFicaNorte
  , testeBazucaExplodeAfetaMinhocas
  , testeDinamiteExplodeGrandeArea
  , testeNadaMuda
  , testeMinaNoChaoApontaNorteCorrigido
  , testeSemExtraMinhoca
  , testeSemQueda
  , testeMinhocaSemPosicao
  , testeMinhocaMorta
  , testeMinhocaMortaCaiAgua
  , testeBarrilNaoDeveCair
  , testeBazucaSaiMapa
  , testeMinaSemInimigosPerto
  , testeMinaPosicaoInvalida
  , testeDinamiteParaNoChaoTerra
  , testeDinamiteParaNoChaoPedra
  , testeDinamiteDirecaoSudeste
  , testeDinamiteDirecaoSudoeste
  , testeDinamiteDirecaoNorte
  , testeDinamiteDirecaoSul
  , testeDinamiteDirecaoNordeste
  , testeDinamiteDirecaoNoroeste
  , testeDinamiteSaiMapa
  , testeMinhocaDanoZero
  , testeOutroObjeto
  , testeBarrilPosicaoNaoLivre
  , testeMinhocaMortaNaAgua
  , testeMinhocaVivaPosiçãoNada
  , testeDinamiteRecenteLancada
  , testeExplosaoRaioImpar
  , testeMinhocaRecebeManoDanoNaoMorre
  , testeExplosaoDiagonal
  , testeMinhocaCentroExplosao
  , testeBarrilAtingidoPorExplosao
  , testeBarrisCascata
  , testeMinaCaiParaPedra
  , testeMinaNoChaoTerra
  , testeMinhocaVidaExatamenteZero
  , testeObjetoGenerico
  , testeDanoZeroMinhoca
  , testeBarrilNaAgua
  , testeMinhocaMortaSemPosicao
  , testeExplosaoApenasTerreno
  , testeBazucaNumaBazuca
  ]

--------------------------------------------------------------------------------
-- Harness de feedback (não mexer)
--------------------------------------------------------------------------------

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3