-- tarefa4 ;)
{-
Module      : Tarefa4
Description : Implementar uma tática de jogo automatizada.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
Maintainer  : l1g053@l1g053.2025@l1

Módulo para a realização da Tarefa 4 de LI1/LP1 em 2025/26.
------- . -------
Implementação de um bot que joga automaticamente, escolhendo jogadas que maximizam
a pontuação do jogo.

A estratégia baseia-se em:
  * cálculo de pontuação esperada para cada jogada possível;
  * priorização de jogadas com maior retorno (destruição de Terra, dano a inimigos);
  * adaptação às diferentes fases do jogo;
  * uso otimizado de armas (Dinamite para Terra, Bazuca para inimigos).

Sistema de pontuação:
  * Dano causado a uma minhoca: X pontos (onde X é a vida perdida);
  * Terra destruída: 10 pontos por bloco;
  * Minhoca eliminada: pontos iguais à vida que tinha.

O objetivo é produzir uma sequência de 100 jogadas que maximize o total de pontos.
-}

module Tarefa4 where

import Data.Either
import Data.List (maximumBy)
import Data.Ord (comparing)

import Labs2025
import Tarefa2
import Tarefa3

--------------------------------------------------------------------------------
-- * Funções principais

-- | Função principal da Tarefa 4. Dado um estado retorna uma lista de jogadas, com exatamente 100 jogadas.
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- | Aplica uma sequência de jogadas a um estado, avançando o tempo entre jogadas.
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Avança o tempo para o estado de uma minhoca, se não efetuou a última jogada.
avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

-- | Avança o tempo para o estado de um objeto, se não foi criado pela última jogada.
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

--------------------------------------------------------------------------------
-- * Estratégia do bot

type Pontuacao = Int

-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
-- Escolhe a jogada que maximiza a pontuação esperada entre todas as opções possíveis.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca,Jogada)
jogadaTatica t e =
    let minhocas = minhocasEstado e
        minhocasVivas = [(i, m) | (i, m) <- zip [0..] minhocas, estaViva m]
    in case minhocasVivas of
        [] -> (0, Move Este)
        ((numMinhoca, minhoca):_) -> 
            escolheMelhorJogada t numMinhoca minhoca e

-- | Avalia todas as jogadas possíveis e escolhe a que dá mais pontos.
escolheMelhorJogada :: Ticks -> NumMinhoca -> Minhoca -> Estado -> (NumMinhoca, Jogada)
escolheMelhorJogada t num minhoca estado =
    case posicaoMinhoca minhoca of
        Nothing -> (num, Move Este)
        Just pos ->
            let jogadas = geraJogadasPossiveis num minhoca pos estado t
                avaliadas = [(j, avaliaJogada j num pos estado) | j <- jogadas]
                melhor = maximumBy (comparing snd) avaliadas
            in (num, fst melhor)

--------------------------------------------------------------------------------
-- * Geração de jogadas possíveis

-- | Gera todas as jogadas que a minhoca pode fazer, considerando as armas disponíveis.
geraJogadasPossiveis :: NumMinhoca -> Minhoca -> Posicao -> Estado -> Ticks -> [Jogada]
geraJogadasPossiveis _ minhoca _ _ t =
    let direcoes = [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]
        
        dinamites = [Dispara Dinamite d | d <- direcoes, dinamiteMinhoca minhoca > 0]
        bazucas = [Dispara Bazuca d | d <- direcoes, bazucaMinhoca minhoca > 0]
        escavadoras = [Dispara Escavadora d | d <- direcoes, escavadoraMinhoca minhoca > 0]
        minas = [Dispara Mina d | d <- direcoes, minaMinhoca minhoca > 0]
        jetpacks = [Dispara Jetpack d | d <- direcoes, jetpackMinhoca minhoca > 0]
        movimentos = [Move d | d <- direcoes]
        
        -- prioriza armas diferentes consoante a fase do jogo
        todas = if t < 40
                   then dinamites ++ bazucas ++ escavadoras ++ jetpacks ++ minas ++ movimentos
                else if t < 80
                   then bazucas ++ dinamites ++ escavadoras ++ minas ++ jetpacks ++ movimentos
                else escavadoras ++ bazucas ++ dinamites ++ movimentos ++ minas ++ jetpacks
                   
    in if null todas then [Move Este] else todas

--------------------------------------------------------------------------------
-- * Avaliação de jogadas

-- | Calcula a pontuação esperada de uma jogada.
-- Considera a destruição de Terra e o dano potencial a minhocas inimigas.
avaliaJogada :: Jogada -> NumMinhoca -> Posicao -> Estado -> Pontuacao
avaliaJogada jogada num pos estado =
    case jogada of
        Dispara Dinamite dir -> 
            let posAlvo = calculaPosicaoDestino pos dir
                terraDestruida = contaTerraEmExplosao posAlvo 7 (mapaEstado estado)
                danoInimigos = estimaDanoInimigos posAlvo 7 num estado
            in terraDestruida * 10 + danoInimigos
        
        Dispara Bazuca dir ->
            let posAlvo = calculaPosicaoDestino pos dir
                terraDestruida = contaTerraEmExplosao posAlvo 5 (mapaEstado estado)
                danoInimigos = estimaDanoInimigos posAlvo 5 num estado
            in terraDestruida * 10 + danoInimigos * 2
        
        Dispara Escavadora dir ->
            let posAlvo = calculaPosicaoDestino pos dir
            in if terrenoNaPos posAlvo (mapaEstado estado) == Terra
                then 10
                else 0
        
        Dispara Mina dir ->
            let posAlvo = calculaPosicaoDestino pos dir
                terraDestruida = contaTerraEmExplosao posAlvo 3 (mapaEstado estado)
                danoInimigos = estimaDanoInimigos posAlvo 3 num estado
            in (terraDestruida * 10 + danoInimigos) `div` 2
        
        Dispara Jetpack dir ->
            let posAlvo = calculaPosicaoDestino pos dir
                terraProxima = contaTerraProxima posAlvo 3 (mapaEstado estado)
            in terraProxima
        
        Move dir ->
            let posAlvo = calculaPosicaoDestino pos dir
                terraProxima = contaTerraProxima posAlvo 2 (mapaEstado estado)
            in terraProxima `div` 2

-- | Conta quantos blocos de Terra seriam destruídos numa explosão centrada na posição dada.
contaTerraEmExplosao :: Posicao -> Int -> Mapa -> Int
contaTerraEmExplosao (l, c) diametro mapa =
    let raio = diametro `div` 2
    in length [ ()
              | dl <- [-raio..raio]
              , dc <- [-raio..raio]
              , let pos = (l + dl, c + dc)
              , let custo = 2 * max (abs dl) (abs dc) + min (abs dl) (abs dc)
              , (diametro - custo) > 0
              , posValida pos mapa
              , terrenoNaPos pos mapa == Terra
              ]

-- | Estima o dano total que seria causado a minhocas inimigas por uma explosão.
estimaDanoInimigos :: Posicao -> Int -> NumMinhoca -> Estado -> Int
estimaDanoInimigos centro diametro numAtual estado =
    let danos = 
            [ dano
            | (i, m) <- zip [0..] (minhocasEstado estado)
            , i /= numAtual
            , estaViva m
            , Just posInim <- [posicaoMinhoca m]
            , Viva _ <- [vidaMinhoca m]
            , let (l1, c1) = centro
            , let (l2, c2) = posInim
            , let dl = l2 - l1
            , let dc = c2 - c1
            , let custo = 2 * max (abs dl) (abs dc) + min (abs dl) (abs dc)
            , let dano = (diametro - custo) * 10
            , dano > 0
            ]
    in sum [min d 50 | d <- danos]

-- | Conta blocos de Terra numa área próxima (não explosão).
contaTerraProxima :: Posicao -> Int -> Mapa -> Int
contaTerraProxima (l, c) raio mapa =
    length [ ()
           | dl <- [-raio..raio]
           , dc <- [-raio..raio]
           , let pos = (l + dl, c + dc)
           , posValida pos mapa
           , terrenoNaPos pos mapa == Terra
           ]

-- | Calcula a posição resultante de avançar numa direção.
calculaPosicaoDestino :: Posicao -> Direcao -> Posicao
calculaPosicaoDestino (l, c) dir = case dir of
    Norte -> (l - 1, c)
    Sul -> (l + 1, c)
    Este -> (l, c + 1)
    Oeste -> (l, c - 1)
    Nordeste -> (l - 1, c + 1)
    Noroeste -> (l - 1, c - 1)
    Sudeste -> (l + 1, c + 1)
    Sudoeste -> (l + 1, c - 1)

--------------------------------------------------------------------------------
-- * Funções auxiliares

-- | Verifica se uma minhoca está viva.
estaViva :: Minhoca -> Bool
estaViva m = case vidaMinhoca m of
    Viva _ -> True
    Morta -> False

-- | Encontra as posições de todas as minhocas inimigas.
encontraInimigos :: NumMinhoca -> Estado -> [Posicao]
encontraInimigos numAtual estado =
    [ pos 
    | (i, m) <- zip [0..] (minhocasEstado estado)
    , i /= numAtual
    , estaViva m
    , Just pos <- [posicaoMinhoca m]
    ]

-- | Calcula a direção aproximada de uma posição para outra.
direcaoParaAlvo :: Posicao -> Posicao -> Direcao
direcaoParaAlvo (l1, c1) (l2, c2) =
    let dl = l2 - l1
        dc = c2 - c1
    in if abs dl > abs dc
        then if dl > 0 then Sul else Norte
        else if dc > 0 then Este else Oeste

-- | Calcula a distância Manhattan entre duas posições.
distancia :: Posicao -> Posicao -> Int
distancia (l1, c1) (l2, c2) = abs (l1 - l2) + abs (c1 - c2)

-- | Verifica se uma posição está dentro dos limites do mapa.
posValida :: Posicao -> Mapa -> Bool
posValida (l, c) mapa =
    l >= 0 && l < length mapa && 
    c >= 0 && c < length (head mapa)

-- | Obtém o terreno numa dada posição do mapa.
terrenoNaPos :: Posicao -> Mapa -> Terreno
terrenoNaPos (l, c) mapa 
    | posValida (l, c) mapa = (mapa !! l) !! c
    | otherwise = Ar