{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
{-|
Module      : Tarefa3
Description : Avançar tempo do jogo
Copyright   : Carolina Dias e Leonor Sousa, 2025
-}
module Tarefa3 where

import Data.Either
import Data.Maybe (isNothing)
import Labs2025
import Tarefa0_2025

type Dano = Int
type Danos = [(Posicao, Dano)]

--------------------------------------------------------------------------------
-- * Função Principal

avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
  where
    minhocas' = zipWith (avancaMinhoca e) [0..] minhocas
    estadoComMinhocas = e { minhocasEstado = minhocas' }
    (objetos', danoss) = partitionEithers $ 
                         zipWith (avancaObjeto estadoComMinhocas) [0..] objetos
    e' = Estado mapa objetos' minhocas'

--------------------------------------------------------------------------------
-- * Avanço de Minhocas

avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca estado _ minhoca
    | not (minhocaValidaGlobal estado minhoca) = 
        minhoca { vidaMinhoca = Morta, posicaoMinhoca = Nothing }
    | isNothing (posicaoMinhoca minhoca) = minhoca
    | vidaMinhoca minhoca == Morta = minhoca
    | otherwise = cairUmBloco estado minhoca

cairUmBloco :: Estado -> Minhoca -> Minhoca
cairUmBloco estado minhoca =
    case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just pos@(l, c) ->
            let mapa = mapaEstado estado
                posAbaixo = (l + 1, c)
            in 
               if estaNoAr estado pos
               then 
                    if not (ePosicaoMatrizValida posAbaixo mapa)
                    then minhoca { vidaMinhoca = Morta, posicaoMinhoca = Nothing }
                    else case encontraPosicaoMatriz posAbaixo mapa of
                           Just Agua -> minhoca { vidaMinhoca = Morta
                                                , posicaoMinhoca = Just posAbaixo }
                           _ -> minhoca { posicaoMinhoca = Just posAbaixo }
               else minhoca

estaNoAr :: Estado -> Posicao -> Bool
estaNoAr estado (l, c) =
    let posAbaixo = (l + 1, c)
        mapa = mapaEstado estado
    in ePosicaoMatrizValida posAbaixo mapa &&
       ePosicaoMapaLivre posAbaixo mapa &&
       not (posicaoTemBarril posAbaixo estado) &&
       not (posicaoTemMinhoca posAbaixo estado)

minhocaValidaGlobal :: Estado -> Minhoca -> Bool
minhocaValidaGlobal estado minhoca =
    case posicaoMinhoca minhoca of
        Nothing -> True
        Just pos -> ePosicaoMatrizValida pos (mapaEstado estado)

minhocaEstaViva :: Minhoca -> Bool
minhocaEstaViva minhoca = case vidaMinhoca minhoca of
    Viva hp -> hp >= 1
    Morta -> False

--------------------------------------------------------------------------------
-- * Avanço de Objetos

-- | Obtém a posição do dono de um disparo
obterPosicaoDono :: NumMinhoca -> [Minhoca] -> Maybe Posicao
obterPosicaoDono dono minhocas
    | dono >= 0 && dono < length minhocas = posicaoMinhoca (minhocas !! dono)
    | otherwise = Nothing

avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto estado _ objeto
    -- BARRIL: explode e dá dano a TODOS (não tem dono)
    | deveExplodir estado objeto, Barril _ _ <- objeto = 
        Right (calculaDanosExplosao objeto)
    -- BAZUCA/DINAMITE/MINA: explodem mas NÃO dão dano ao dono
    | deveExplodir estado objeto, Disparo _ _ _ _ dono <- objeto =
        let posDono = obterPosicaoDono dono (minhocasEstado estado)
        in Right (calculaDanosExcluindoDono objeto posDono)
    | otherwise = case objeto of
        
        -- BARRIL: se no ar, fica prestes a explodir
        (Barril pos _) ->
            let posAbaixo = posicaoAbaixoDe pos
                noAr = ePosicaoMapaLivre posAbaixo (mapaEstado estado) &&
                       not (posicaoTemBarril posAbaixo estado) &&
                       not (posicaoTemMinhoca posAbaixo estado)
            in if noAr
               then Left (Barril pos True)
               else Left (Barril pos False)
        
        -- BAZUCA: avança na direção, explode se bater em obstáculo (SEM DANO AO DONO)
        disparo@(Disparo pos dir Bazuca _ dono) ->
            let novaPos = proximaPosicao pos dir
                temObstaculo = not (ePosicaoMatrizValida novaPos (mapaEstado estado)) ||
                              not (ePosicaoMapaLivre novaPos (mapaEstado estado)) ||
                              posicaoTemMinhoca novaPos estado ||
                              posicaoTemBarril novaPos estado
                posDono = obterPosicaoDono dono (minhocasEstado estado)
            in if temObstaculo
               then Right (calculaDanosExcluindoDono disparo posDono)
               else Left disparo { posicaoDisparo = novaPos }
        
        -- DINAMITE: parábola se no ar, conta tempo
        (Disparo pos dir Dinamite tempo dono) ->
            let posAbaixo = posicaoAbaixoDe pos
                noAr = ePosicaoMapaLivre posAbaixo (mapaEstado estado) &&
                       not (posicaoTemBarril posAbaixo estado) &&
                       not (posicaoTemMinhoca posAbaixo estado)
                
                novoTempo = case tempo of
                    Just n | n > 0 -> Just (n - 1)
                    other -> other
                
                (novaPos, novaDirecao) = if noAr
                    then executaParabola estado pos dir
                    else (pos, Norte)
                
            in Left (Disparo novaPos novaDirecao Dinamite novoTempo dono)
        
        -- MINA: cai se no ar, ativa quando inimigo na área
        (Disparo pos _ Mina _ dono) ->
            let mapa = mapaEstado estado
                posAbaixo = posicaoAbaixoDe pos
                noAr = ePosicaoMapaLivre posAbaixo (mapaEstado estado) &&
                       encontraPosicaoMatriz posAbaixo mapa == Just Ar &&
                       not (posicaoTemBarril posAbaixo estado) &&
                       not (posicaoTemMinhoca posAbaixo estado)
                
                novaPos = if noAr then posAbaixo else pos
                
                -- Ativa quando inimigo entra na área
                deveAtivar = existeInimigoNaArea novaPos dono 3 estado
                
                posDono = obterPosicaoDono dono (minhocasEstado estado)
                
            in if deveAtivar
               then Right (calculaDanosExcluindoDono (Disparo novaPos Norte Mina Nothing dono) posDono)
               else Left  (Disparo novaPos Norte Mina Nothing dono)
        
        other -> Left other

deveExplodir :: Estado -> Objeto -> Bool
deveExplodir _ (Barril _ True) = True
deveExplodir _ (Disparo _ _ _ (Just 0) _) = True
deveExplodir _ _ = False

--------------------------------------------------------------------------------
-- * Parábola da Dinamite

executaParabola :: Estado -> Posicao -> Direcao -> (Posicao, Direcao)
executaParabola estado pos dir =
    let novaPos = proximaPosicao pos dir
        novaDirecao = rodaDirecao45 dir
        destinoLivre = ePosicaoMatrizValida novaPos (mapaEstado estado) &&
                      ePosicaoMapaLivre novaPos (mapaEstado estado) &&
                      not (posicaoTemBarril novaPos estado) &&
                      not (posicaoTemMinhoca novaPos estado)
    in case dir of
         Norte -> (proximaPosicao pos Sul, Norte)
         Sul   -> (proximaPosicao pos Sul, Norte)
         _ -> if destinoLivre
              then (novaPos, novaDirecao)
              else (proximaPosicao pos Sul, Norte)

rodaDirecao45 :: Direcao -> Direcao
rodaDirecao45 Este      = Sudeste
rodaDirecao45 Sudeste   = Sul
rodaDirecao45 Sul       = Sudoeste
rodaDirecao45 Sudoeste  = Oeste
rodaDirecao45 Oeste     = Noroeste
rodaDirecao45 Noroeste  = Norte
rodaDirecao45 Norte     = Nordeste
rodaDirecao45 Nordeste  = Este

--------------------------------------------------------------------------------
-- * Explosões

calculaDanosExplosao :: Objeto -> Danos
calculaDanosExplosao objeto = 
    [ (somaPos centro (dx, dy), dano)
    | dx <- [-raio .. raio]
    , dy <- [-raio .. raio]
    , let custo = custoPonderado (dx, dy)
    , let dano = (diametro - custo) * 10
    , dano > 0
    ]
  where
    centro = posicaoDoObjeto objeto
    diametro = diametroExplosao objeto
    raio = diametro `div` 2
    somaPos (x, y) (dx, dy) = (x + dx, y + dy)

-- | Calcula danos EXCLUINDO a posição do dono - ARMAS NÃO DÃO DANO AO DONO!
calculaDanosExcluindoDono :: Objeto -> Maybe Posicao -> Danos
calculaDanosExcluindoDono objeto maybePosicaoDono = 
    [ (pos, dano)
    | (pos, dano) <- calculaDanosExplosao objeto
    , Just pos /= maybePosicaoDono
    ]

diametroExplosao :: Objeto -> Int
diametroExplosao (Disparo _ _ Dinamite _ _) = 7
diametroExplosao (Barril _ _)               = 5
diametroExplosao (Disparo _ _ Bazuca _ _)   = 5
diametroExplosao (Disparo _ _ Mina _ _)     = 5
diametroExplosao _                          = 0

custoPonderado :: (Int, Int) -> Int
custoPonderado (dx, dy) =
    let ax = abs dx
        ay = abs dy
    in 2 * max ax ay + min ax ay

--------------------------------------------------------------------------------
-- * Deteção de Inimigos (para Minas)

existeInimigoNaArea :: Posicao -> NumMinhoca -> Int -> Estado -> Bool
existeInimigoNaArea (cx, cy) dono diametro estado =
    any inimigoNaArea (zip [0 :: Int ..] (minhocasEstado estado))
  where
    raio = diametro `div` 2
    inimigoNaArea :: (Int, Minhoca) -> Bool
    inimigoNaArea (idx, minhoca) =
        idx /= dono &&
        minhocaEstaViva minhoca &&
        case posicaoMinhoca minhoca of
            Just (mx, my) -> abs (mx - cx) <= raio && abs (my - cy) <= raio
            Nothing -> False

--------------------------------------------------------------------------------
-- * Aplicação de Danos

aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos estado = 
    estado { minhocasEstado = novasMinhocas
           , mapaEstado = novoMapa
           , objetosEstado = novosObjetos }
  where
    novasMinhocas = map (aplicaDanoMinhoca danos) (minhocasEstado estado)
    novoMapa = foldr destruirTerreno (mapaEstado estado) danos
    novosObjetos = map (marcarBarrilAtingido danos) (objetosEstado estado)

aplicaDanoMinhoca :: Danos -> Minhoca -> Minhoca
aplicaDanoMinhoca danos minhoca =
    let pos = posicaoMinhoca minhoca
        danoTotal = sum [d | (p, d) <- danos, Just p == pos]
    in if danoTotal > 0
       then case vidaMinhoca minhoca of
              Morta -> minhoca
              Viva hp -> 
                let novaVida = hp - danoTotal
                in if novaVida <= 0
                   then minhoca { vidaMinhoca = Morta }
                   else minhoca { vidaMinhoca = Viva novaVida }
       else minhoca

destruirTerreno :: (Posicao, Dano) -> Mapa -> Mapa
destruirTerreno (pos, dano) mapa
    | dano > 0 && ePosicaoMatrizValida pos mapa =
        case encontraPosicaoMatriz pos mapa of
            Just Terra -> atualizaPosicaoMatriz pos Ar mapa
            _ -> mapa
    | otherwise = mapa

marcarBarrilAtingido :: Danos -> Objeto -> Objeto
marcarBarrilAtingido danos (Barril pos _) =
    let atingido = any (\(p, d) -> p == pos && d > 0) danos
    in Barril pos atingido
marcarBarrilAtingido _ obj = obj

--------------------------------------------------------------------------------
-- * Funções Auxiliares

posicaoDoObjeto :: Objeto -> Posicao
posicaoDoObjeto (Barril pos _) = pos
posicaoDoObjeto (Disparo pos _ _ _ _) = pos

posicaoAbaixoDe :: Posicao -> Posicao
posicaoAbaixoDe (l, c) = (l + 1, c)

proximaPosicao :: Posicao -> Direcao -> Posicao
proximaPosicao (l, c) Norte     = (l - 1, c)
proximaPosicao (l, c) Sul       = (l + 1, c)
proximaPosicao (l, c) Este      = (l, c + 1)
proximaPosicao (l, c) Oeste     = (l, c - 1)
proximaPosicao (l, c) Nordeste  = (l - 1, c + 1)
proximaPosicao (l, c) Noroeste  = (l - 1, c - 1)
proximaPosicao (l, c) Sudeste   = (l + 1, c + 1)
proximaPosicao (l, c) Sudoeste  = (l + 1, c - 1)

posicaoTemBarril :: Posicao -> Estado -> Bool
posicaoTemBarril pos estado =
    any (\obj -> case obj of
        Barril p _ -> p == pos
        _ -> False) (objetosEstado estado)

posicaoTemMinhoca :: Posicao -> Estado -> Bool
posicaoTemMinhoca pos estado =
    any (\m -> posicaoMinhoca m == Just pos) (minhocasEstado estado)