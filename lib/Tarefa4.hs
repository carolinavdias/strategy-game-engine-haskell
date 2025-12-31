{-|
Module      : Tarefa4
Description : Bot inteligente com sistema de rotação e maximização de dano
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Implementação do Bot Inteligente

== Arquitetura

O bot foi implementado com três componentes principais:

1. __Função Principal__: 'tatica' executa 100 jogadas sequenciais, 
   usando um sistema de memória para alternar entre minhocas

2. __Sistema de Rotação__: Round-robin que garante distribuição
   justa das jogadas entre todas as minhocas vivas

3. __Maximização de Dano__: Calcula o dano esperado para cada
   arma em cada direção e escolhe a ação mais destrutiva

== Estratégia

* Localiza o inimigo mais próximo usando distância Manhattan
* Calcula dano esperado de cada arma (Bazuca, Dinamite, Mina)
* Se consegue causar dano, ataca
* Se não consegue, aproxima-se do inimigo
* Sem inimigos, entra em modo exploração (destrói terreno = pontos!)

== Sistema de Armas

| Arma      | Diâmetro | Comportamento              |
|-----------|----------|----------------------------|
| Bazuca    | 5        | Viaja em linha reta        |
| Dinamite  | 7        | Colocada adjacente         |
| Mina      | 3        | Colocada adjacente         |

== Fórmula de Dano

@
dano = (diâmetro - custo) * 10
custo(dx, dy) = 2 * max(|dx|, |dy|) + min(|dx|, |dy|)
@
-}

module Tarefa4 where

import Data.Either (partitionEithers)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import Labs2025
import Tarefa2
import Tarefa3

--------------------------------------------------------------------------------
-- * FUNÇÃO PRINCIPAL

-- | Memória do bot: índice da próxima minhoca a jogar
type Memoria = Int

-- | Função principal da Tarefa 4. Retorna lista de 100 jogadas inteligentes.
tatica :: Estado -> [(NumMinhoca, Jogada)]
tatica e = reverse $ extraiJogadas $ foldl avancaTatica (e, [], 0) [0..99]
  where
    extraiJogadas (_, js, _) = js

-- | Aplica uma jogada e avança o estado, mantendo memória
avancaTatica :: (Estado, [(NumMinhoca, Jogada)], Memoria) -> Ticks -> (Estado, [(NumMinhoca, Jogada)], Memoria)
avancaTatica (e, js, mem) tick = (avancaJogada j e, j:js, novaMem)
  where
    (j, novaMem) = jogadaTaticaComMem tick mem e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
avancaJogada :: (NumMinhoca, Jogada) -> Estado -> Estado
avancaJogada (i, j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
  where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'', danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Avança minhoca se não mudou de posição
avancaMinhocaJogada :: Estado -> (NumMinhoca, Minhoca, Minhoca) -> Minhoca
avancaMinhocaJogada e (i, minhoca, minhoca') = 
    if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

-- | Avança objeto se já existia
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto, Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i, objeto') = 
    if objeto' `elem` objetos
    then avancaObjeto e i objeto'
    else Left objeto'

--------------------------------------------------------------------------------
-- * ESTRATÉGIA DO BOT

-- | Interface para o jogo visual (sem memória)
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica tick estado = fst $ jogadaTaticaComMem tick (tick `mod` totalMinhocas) estado
  where
    totalMinhocas = max 1 (length (minhocasEstado estado))

-- | Determina a próxima jogada usando sistema de memória/rotação
jogadaTaticaComMem :: Ticks -> Memoria -> Estado -> ((NumMinhoca, Jogada), Memoria)
jogadaTaticaComMem tick mem estado = 
    case encontraMinhocaAtiva mem minhocas of
        Nothing -> ((0, Move Norte), mem)  -- Fallback
        Just (idx, minhoca) -> 
            let novaMem = (idx + 1) `mod` totalMinhocas
                pos = fromMaybe (0, 0) (posicaoMinhoca minhoca)
                inimigo = encontraInimigoMaisProximo idx pos minhocas
                jogada = decideAcao tick minhoca pos inimigo estado
            in ((idx, jogada), novaMem)
  where
    minhocas = minhocasEstado estado
    totalMinhocas = length minhocas

--------------------------------------------------------------------------------
-- * LOCALIZAÇÃO DE MINHOCAS

-- | Encontra a primeira minhoca ativa a partir do índice dado (round-robin)
encontraMinhocaAtiva :: Int -> [Minhoca] -> Maybe (NumMinhoca, Minhoca)
encontraMinhocaAtiva inicio minhocas =
    case filter minhocaValida (ordenadas ++ anteriores) of
        (x:_) -> Just x
        []    -> Nothing
  where
    indexadas = zip [0..] minhocas
    (anteriores, ordenadas) = splitAt inicio indexadas
    
    minhocaValida :: (Int, Minhoca) -> Bool
    minhocaValida (_, m) = case (vidaMinhoca m, posicaoMinhoca m) of
        (Viva v, Just _) -> v > 0
        _ -> False

-- | Encontra o inimigo mais próximo usando distância Manhattan
encontraInimigoMaisProximo :: NumMinhoca -> Posicao -> [Minhoca] -> Maybe (NumMinhoca, Posicao)
encontraInimigoMaisProximo meuIdx minhaPos minhocas =
    case inimigosVivos of
        [] -> Nothing
        xs -> Just $ minimumBy (comparing (\(_, p) -> distancia minhaPos p)) xs
  where
    inimigosVivos = 
        [ (i, pos)
        | (i, m) <- zip [0..] minhocas
        , i /= meuIdx
        , estaViva m
        , Just pos <- [posicaoMinhoca m]
        ]
    
    minimumBy :: (a -> a -> Ordering) -> [a] -> a
    minimumBy _ [x] = x
    minimumBy cmp (x:xs) = foldl (\acc y -> if cmp acc y == GT then y else acc) x xs
    minimumBy _ [] = error "Lista vazia"

--------------------------------------------------------------------------------
-- * DECISÃO DE AÇÃO

-- | Decide a melhor ação: atacar, aproximar ou explorar
decideAcao :: Ticks -> Minhoca -> Posicao -> Maybe (NumMinhoca, Posicao) -> Estado -> Jogada
decideAcao tick minhoca minhaPos alvo estado =
    case alvo of
        Nothing -> explorar tick minhaPos minhoca  -- Sem inimigos: explorar
        Just (_, posInimigo) ->
            let (melhorAcao, dano) = encontraMelhorAtaque minhoca minhaPos posInimigo
            in if dano > 0
               then melhorAcao  -- Consegue causar dano: ataca!
               else Move (direcaoParaAlvo minhaPos posInimigo)  -- Aproxima-se

-- | Modo exploração: usa armas para destruir terreno (dá pontos!)
explorar :: Ticks -> Posicao -> Minhoca -> Jogada
explorar tick (l, c) minhoca
    | dinamiteMinhoca minhoca > 0 = Dispara Dinamite dir  -- Maior área
    | minaMinhoca minhoca > 0     = Dispara Mina dir
    | bazucaMinhoca minhoca > 0   = Dispara Bazuca dir
    | otherwise                   = Move dir
  where
    -- Alterna direção baseado em tick e posição para não ficar preso
    direcoes = [Norte, Este, Sul, Oeste]
    dir = direcoes !! ((tick + l + c) `mod` 4)

--------------------------------------------------------------------------------
-- * SISTEMA DE COMBATE

-- | Encontra o melhor ataque considerando todas as armas e direções
encontraMelhorAtaque :: Minhoca -> Posicao -> Posicao -> (Jogada, Int)
encontraMelhorAtaque minhoca minhaPos posInimigo =
    if null ataquesPossiveis
    then (Move Norte, 0)
    else maximumBy (comparing snd) ataquesPossiveis
  where
    direcoes = [Norte, Sul, Este, Oeste]
    
    ataquesPossiveis = 
        [ (Dispara Bazuca dir, calculaDanoBazuca minhaPos dir posInimigo)
        | dir <- direcoes, bazucaMinhoca minhoca > 0
        ] ++
        [ (Dispara Dinamite dir, calculaDanoDinamite minhaPos dir posInimigo)
        | dir <- direcoes, dinamiteMinhoca minhoca > 0
        ] ++
        [ (Dispara Mina dir, calculaDanoMina minhaPos dir posInimigo)
        | dir <- direcoes, minaMinhoca minhoca > 0
        ]

-- | Calcula dano da Bazuca (diâmetro 5, viaja em linha reta)
calculaDanoBazuca :: Posicao -> Direcao -> Posicao -> Int
calculaDanoBazuca (l1, c1) dir (li, ci)
    -- Bazuca só atinge se inimigo estiver alinhado na direção do disparo
    | dir == Norte && c1 == ci && li < l1 = calculaDanoExplosao 5 (li, ci) (li, ci)
    | dir == Sul   && c1 == ci && li > l1 = calculaDanoExplosao 5 (li, ci) (li, ci)
    | dir == Este  && l1 == li && ci > c1 = calculaDanoExplosao 5 (li, ci) (li, ci)
    | dir == Oeste && l1 == li && ci < c1 = calculaDanoExplosao 5 (li, ci) (li, ci)
    | otherwise = 0

-- | Calcula dano da Dinamite (diâmetro 7, colocada adjacente)
calculaDanoDinamite :: Posicao -> Direcao -> Posicao -> Int
calculaDanoDinamite minhaPos dir posInimigo = 
    calculaDanoExplosao 7 posExplosao posInimigo
  where
    posExplosao = moveNaDirecao minhaPos dir

-- | Calcula dano da Mina (diâmetro 3, colocada adjacente)
calculaDanoMina :: Posicao -> Direcao -> Posicao -> Int
calculaDanoMina minhaPos dir posInimigo = 
    calculaDanoExplosao 3 posExplosao posInimigo
  where
    posExplosao = moveNaDirecao minhaPos dir

-- | Calcula dano de explosão usando fórmula do jogo
-- dano = (diâmetro - custo) * 10
calculaDanoExplosao :: Int -> Posicao -> Posicao -> Int
calculaDanoExplosao diametro (expL, expC) (alvL, alvC)
    | dano > 0  = dano
    | otherwise = 0
  where
    dl = alvL - expL
    dc = alvC - expC
    custo = 2 * max (abs dl) (abs dc) + min (abs dl) (abs dc)
    dano = (diametro - custo) * 10

--------------------------------------------------------------------------------
-- * FUNÇÕES AUXILIARES

-- | Move uma posição na direção dada
moveNaDirecao :: Posicao -> Direcao -> Posicao
moveNaDirecao (l, c) Norte = (l - 1, c)
moveNaDirecao (l, c) Sul   = (l + 1, c)
moveNaDirecao (l, c) Este  = (l, c + 1)
moveNaDirecao (l, c) Oeste = (l, c - 1)
moveNaDirecao (l, c) _     = (l, c)  -- Diagonais não suportadas em combate

-- | Calcula direção para ir de A para B
direcaoParaAlvo :: Posicao -> Posicao -> Direcao
direcaoParaAlvo (l1, c1) (l2, c2)
    | l1 > l2   = Norte  -- Alvo está acima
    | l1 < l2   = Sul    -- Alvo está abaixo
    | c1 < c2   = Este   -- Alvo está à direita
    | otherwise = Oeste  -- Alvo está à esquerda

-- | Calcula distância Manhattan entre duas posições
distancia :: Posicao -> Posicao -> Int
distancia (l1, c1) (l2, c2) = abs (l1 - l2) + abs (c1 - c2)

-- | Verifica se uma minhoca está viva
estaViva :: Minhoca -> Bool
estaViva m = case vidaMinhoca m of
    Viva v -> v > 0
    Morta  -> False