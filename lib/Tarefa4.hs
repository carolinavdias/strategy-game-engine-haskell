{-|
Module      : Tarefa4
Description : Bot para o jogo Worms
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo implementa o bot adversário.

A tática utilizada é cíclica e determinística:

* Ciclo de 12 jogadas
* Alternância entre movimento e ataque
* Utilização de todas as armas disponíveis
* Comportamento previsível e testável

O Bot não analisa o terreno nem a posição do inimigo,
por decisão consciente de simplicidade e equilíbrio.
-}

module Tarefa4 where

import Data.Either (partitionEithers)
import Labs2025
import Tarefa2 (efetuaJogada)
import Tarefa3 (avancaMinhoca, avancaObjeto, aplicaDanos, Danos)

--------------------------------------------------------------------------------
-- * FUNÇÃO PRINCIPAL

tatica :: Estado -> [(NumMinhoca, Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e, []) [0..99]

avancaTatica :: (Estado, [(NumMinhoca, Jogada)]) -> Ticks -> (Estado, [(NumMinhoca, Jogada)])
avancaTatica (e, js) tick = (avancaJogada j e, j:js)
    where j = jogadaTatica tick e

avancaJogada :: (NumMinhoca, Jogada) -> Estado -> Estado
avancaJogada (i, j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'', danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

avancaMinhocaJogada :: Estado -> (NumMinhoca, Minhoca, Minhoca) -> Minhoca
avancaMinhocaJogada e (i, minhoca, minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto, Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i, objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

--------------------------------------------------------------------------------
-- * BOT - SEQUÊNCIA DE JOGADAS

-- | Bot faz jogadas numa sequência variada
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica turno _ =
    case turno `mod` 12 of
        0  -> (1, Dispara Bazuca Oeste)       -- Bazuca
        1  -> (1, Move Oeste)                  -- Aproxima
        2  -> (1, Dispara Dinamite Oeste)     -- Dinamite
        3  -> (1, Move Oeste)                  -- Aproxima
        4  -> (1, Dispara Mina Oeste)         -- Coloca mina
        5  -> (1, Move Oeste)                  -- Aproxima
        6  -> (1, Dispara Bazuca Oeste)       -- Bazuca
        7  -> (1, Dispara Jetpack Norte)      -- Voa para cima
        8  -> (1, Dispara Bazuca Oeste)       -- Bazuca
        9  -> (1, Dispara Escavadora Sul)     -- Escava para baixo
        10 -> (1, Move Oeste)                  -- Aproxima
        11 -> (1, Dispara Dinamite Oeste)     -- Dinamite
        _  -> (1, Move Oeste)                  -- Fallback