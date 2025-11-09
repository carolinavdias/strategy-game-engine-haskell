--tarefa 1 :)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-|
Module      : Tarefa1
Description : Validação de estados do jogo
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
Maintainer  : l1g053@l1g053.2025@l1
Stability   : experimental
Portability : portable
Este módulo valida se um estado do jogo está correto, verificando o mapa, os objetos e as minhocas segundo as regras definidas para o jogo.

 
A validação é feita em três partes principais:
1. o mapa (estrutura e terrenos);
2. os objetos (barris e disparos);
3. as minhocas (posições, vidas e munições).

O objetivo é garantir que o estado do jogo segue todas as regras definidas, para que o jogo possa decorrer sem inconsistências.
-}

module Tarefa1 where
import Labs2025
import Tarefa0_2025

-- | Verifica se o estado cumpre todas as regras do jogo
-- (mapa, objetos e minhocas).
validaEstado :: Estado -> Bool
validaEstado estado =
    validaMapa (mapaEstado estado) &&
    validaObjetos (objetosEstado estado) (mapaEstado estado) (minhocasEstado estado) &&
    validaMinhocas (minhocasEstado estado) (mapaEstado estado) (objetosEstado estado)

-- * VALIDAÇÃO DE MAPA 

--Um mapa é considerado válido quando:
--  |1. não está vazio;
--  |2. todas as linhas têm o mesmo número de colunas;
--  |3. contém apenas terrenos permitidos (Ar, Água, Terra ou Pedra).
validaMapa :: Mapa -> Bool
validaMapa mapa =
    not (null mapa) &&
    eMatrizValida mapa &&
    all validaTerreno (concat mapa)
  where
    validaTerreno :: Terreno -> Bool
    validaTerreno Ar = True
    validaTerreno Agua = True
    validaTerreno Terra = True
    validaTerreno Pedra = True

-- * VALIDAÇÃO DE OBJETOS 

-- | Verifica se todos os objetos (barris e disparos) estão corretamente posicionados e definidos.
validaObjetos :: [Objeto] -> Mapa -> [Minhoca] -> Bool
validaObjetos objs mapa minhocas =
    all (\obj -> validaObjeto obj mapa minhocas objs) objs &&
    validaDisparosUnicos objs

-- | Valida um único objeto.
-- Barris e disparos têm critérios de validação diferentes.
validaObjeto :: Objeto -> Mapa -> [Minhoca] -> [Objeto] -> Bool
validaObjeto obj mapa minhocas todosObjs =
    case obj of
        Barril pos _ ->
            posicaoValidaELivreNoMapa pos mapa &&
            posicaoNaoOcupadaPorOutroBarril pos todosObjs obj &&
            posicaoNaoOcupadaPorMinhoca pos minhocas

        Disparo pos dir tipo tempo dono ->
            validaDisparo pos dir tipo tempo dono mapa minhocas

-- | Verifica se um disparo é válido:
--   * o tipo é permitido,
--   * a posição é válida,
--   * o tempo corresponde ao tipo de arma,
--   * e o dono é um índice válido.
validaDisparo :: Posicao -> Direcao -> TipoArma -> Maybe Ticks -> NumMinhoca -> Mapa -> [Minhoca] -> Bool
validaDisparo pos dir tipo tempo dono mapa minhocas =
    tipo /= Jetpack &&
    tipo /= Escavadora &&
    validaPosicaoDisparo pos dir tipo mapa &&
    validaTempoDisparo tipo tempo &&
    donoValido dono minhocas

-- | Confirma que uma posição é válida e livre (sem terreno opaco).
posicaoValidaELivreNoMapa :: Posicao -> Mapa -> Bool
posicaoValidaELivreNoMapa pos mapa =
    ePosicaoMatrizValida pos mapa &&
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> False

-- | Verifica se a posição do disparo é válida.
-- A bazuca pode perfurar terreno opaco apenas na superfície.
validaPosicaoDisparo :: Posicao -> Direcao -> TipoArma -> Mapa -> Bool
validaPosicaoDisparo pos dir Bazuca mapa =
    ePosicaoMatrizValida pos mapa &&
    case encontraPosicaoMatriz pos mapa of
        Just terreno ->
            not (eTerrenoOpaco terreno) || posicaoAnteriorNaoOpaca pos dir mapa
        Nothing -> False
validaPosicaoDisparo pos _ _ mapa = posicaoValidaELivreNoMapa pos mapa

-- | Verifica se a posição anterior (na direção oposta) não é opaca.
posicaoAnteriorNaoOpaca :: Posicao -> Direcao -> Mapa -> Bool
posicaoAnteriorNaoOpaca pos dir mapa =
    let posAnt = movePosicao (direcaoOposta dir) pos
    in case encontraPosicaoMatriz posAnt mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> True

-- | Devolve a direção oposta à dada.
direcaoOposta :: Direcao -> Direcao
direcaoOposta Norte = Sul
direcaoOposta Sul = Norte
direcaoOposta Este = Oeste
direcaoOposta Oeste = Este
direcaoOposta Nordeste = Sudoeste
direcaoOposta Sudoeste = Nordeste
direcaoOposta Noroeste = Sudeste
direcaoOposta Sudeste = Noroeste

-- | Garante que não há outro barril na mesma posição.
posicaoNaoOcupadaPorOutroBarril :: Posicao -> [Objeto] -> Objeto -> Bool
posicaoNaoOcupadaPorOutroBarril pos objs objAtual =
    all (\obj -> case obj of
            Barril p _ -> p /= pos || objetosIguais obj objAtual
            _ -> True) objs

-- | Verifica se dois objetos são exatamente iguais
-- (para evitar auto-comparações)
objetosIguais :: Objeto -> Objeto -> Bool
objetosIguais (Barril p1 e1) (Barril p2 e2) = p1 == p2 && e1 == e2
objetosIguais (Disparo pos1 dir1 tipo1 tempo1 dono1) (Disparo pos2 dir2 tipo2 tempo2 dono2) =
    pos1 == pos2 && dir1 == dir2 && tipo1 == tipo2 && tempo1 == tempo2 && dono1 == dono2
objetosIguais _ _ = False

-- | Verifica se a posição não está ocupada por nenhuma minhoca.
posicaoNaoOcupadaPorMinhoca :: Posicao -> [Minhoca] -> Bool
posicaoNaoOcupadaPorMinhoca pos = all (\m -> posicaoMinhoca m /= Just pos)

-- | Verifica se o número do dono corresponde a uma minhoca existente.
donoValido :: NumMinhoca -> [Minhoca] -> Bool
donoValido num minhocas = num >= 0 && num < length minhocas

-- | Verifica se o tempo do disparo é válido de acordo com o tipo de arma.
validaTempoDisparo :: TipoArma -> Maybe Ticks -> Bool
validaTempoDisparo Bazuca Nothing = True
validaTempoDisparo Bazuca (Just _) = False
validaTempoDisparo Mina Nothing = True
validaTempoDisparo Mina (Just t) = t >= 0 && t <= 2
validaTempoDisparo Dinamite (Just t) = t >= 0 && t <= 4
validaTempoDisparo Dinamite Nothing = False
validaTempoDisparo _ _ = False

-- | Garante que um mesmo dono não tem disparos duplicados do mesmo tipo de arma.
validaDisparosUnicos :: [Objeto] -> Bool
validaDisparosUnicos objs =
    let disparos = [(tipo, dono) | Disparo _ _ tipo _ dono <- objs]
    in length disparos == length (removerDuplicados disparos)

-- | Remove elementos duplicados de uma lista.
removerDuplicados :: Eq a => [a] -> [a]
removerDuplicados [] = []
removerDuplicados (x:xs) = x : removerDuplicados (filter (/= x) xs)

------------------------------------------------------------------------------------------
-- * VALIDAÇÃO DE MINHOCAS 

--Cada minhoca é considerada válida se:
--  |> tiver uma posição válida e livre, ou então nenhuma posição (caso esteja morta);
--  |> não ocupar a mesma casa que outro objeto ou minhoca;
--  |> se estiver na água, estiver morta;
--  |> se estiver viva, a vida for um valor entre 0 e 100;
--  |> se todas as munições forem números inteiros maiores ou iguais a 0.
validaMinhocas :: [Minhoca] -> Mapa -> [Objeto] -> Bool
validaMinhocas minhocas mapa objs =
    all (\m -> validaMinhoca m mapa objs minhocas) minhocas &&
    validaPosicoesMinhocasUnicas minhocas

-- | Valida uma única minhoca de acordo com a sua posição, estado de vida e munições.
validaMinhoca :: Minhoca -> Mapa -> [Objeto] -> [Minhoca] -> Bool
validaMinhoca minhoca mapa objs todasMinhocas =
    case posicaoMinhoca minhoca of
        Nothing ->
            vidaMinhoca minhoca == Morta
        Just pos ->
            posicaoValidaELivreNoMapa pos mapa &&
            posicaoLivreNoEstado pos objs todasMinhocas minhoca &&
            validaMinhocaEmAgua pos mapa minhoca &&
            validaVidaEMunicoes minhoca

-- | Verifica se a posição está livre (sem barris nem outras minhocas)
posicaoLivreNoEstado :: Posicao -> [Objeto] -> [Minhoca] -> Minhoca -> Bool
posicaoLivreNoEstado pos objs minhocas minhocaAtual =
    posicaoNaoTemBarril pos objs &&
    posicaoNaoOcupadaPorOutraMinhoca pos minhocas minhocaAtual

-- | Verifica se a posição não contém barris.
posicaoNaoTemBarril :: Posicao -> [Objeto] -> Bool
posicaoNaoTemBarril pos = all (\obj -> case obj of
            Barril p _ -> p /= pos
            _ -> True)

-- | Garante que a posição não está ocupada por outra minhoca.
posicaoNaoOcupadaPorOutraMinhoca :: Posicao -> [Minhoca] -> Minhoca -> Bool
posicaoNaoOcupadaPorOutraMinhoca pos minhocas minhocaAtual =
    all (\m -> posicaoMinhoca m /= Just pos ||
               minhocasIguais m minhocaAtual) minhocas

-- | Duas minhocas são consideradas iguais, se tiverem a mesma posição e o mesmo estado de vida.
minhocasIguais :: Minhoca -> Minhoca -> Bool
minhocasIguais m1 m2 =
    posicaoMinhoca m1 == posicaoMinhoca m2 &&
    vidaMinhoca m1 == vidaMinhoca m2

-- | Se a minhoca estiver na água, deve estar morta.
validaMinhocaEmAgua :: Posicao -> Mapa -> Minhoca -> Bool
validaMinhocaEmAgua pos mapa minhoca =
    case encontraPosicaoMatriz pos mapa of
        Just Agua -> vidaMinhoca minhoca == Morta
        _ -> True

-- | Verifica se a vida e as munições da minhoca são válidas.
validaVidaEMunicoes :: Minhoca -> Bool
validaVidaEMunicoes minhoca =
    validaVida (vidaMinhoca minhoca) &&
    jetpackMinhoca minhoca >= 0 &&
    escavadoraMinhoca minhoca >= 0 &&
    bazucaMinhoca minhoca >= 0 &&
    minaMinhoca minhoca >= 0 &&
    dinamiteMinhoca minhoca >= 0
  where
    validaVida :: VidaMinhoca -> Bool
    validaVida Morta = True
    validaVida (Viva v) = v >= 0 && v <= 100

-- | Garante que não há duas minhocas na mesma posição.
validaPosicoesMinhocasUnicas :: [Minhoca] -> Bool
validaPosicoesMinhocasUnicas minhocas =
    let posicoes = [pos | m <- minhocas, Just pos <- [posicaoMinhoca m]]
    in length posicoes == length (removerDuplicados posicoes)
