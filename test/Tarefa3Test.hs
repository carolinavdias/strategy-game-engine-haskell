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

-- | Teste 2: Minhoca cai na Água → morre mas fica com a posição nova.
testeQuedaParaAgua :: Estado
testeQuedaParaAgua =
  Estado mapaChaoCentroAgua [] [ mkViva (3,8) ]

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

-- | Teste 11: Dinamite em parábola (no ar) – movimento Este → Sudeste.
testeDinamiteParabola :: Estado
testeDinamiteParabola =
  Estado mapaMesa
    [ Disparo (1,7) Este Dinamite (Just 3) 0 ]
    [ mkViva (2,7) ]

-- | Teste 12 (corrigido): Dinamite no chão com tempo 1.
testeDinamiteNoChaoFicaNorte :: Estado
testeDinamiteNoChaoFicaNorte =
  Estado mapaChaoCentroAgua
    [ Disparo (6,3) Norte Dinamite (Just 1) 0 ]
    [ mkViva (3,3) ]

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

--------------------------------------------------------------------------------
-- Lista para o feedback automático
--------------------------------------------------------------------------------

testesTarefa3 :: [Estado]
testesTarefa3 =
  [ -- 0
    exemplo_e'
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
