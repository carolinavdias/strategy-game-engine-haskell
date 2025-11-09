-- tarefa3 ;)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
{-
Module      : Tarefa3
Description : Avançar tempo do jogo.
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
Maintainer  : l1g053@l1g053.2025@l1
Stability   : experimental
Portability : portable

Módulo para a realização da Tarefa 3 de LI1/LP1 em 2025/26.
------- . -------
Aqui é tratada a **evolução do estado do jogo ao longo do tempo** — isto é, o que acontece a cada “tick” (instante de tempo).

A função principal, `avancaEstado`, atualiza o jogo considerando:
  * o movimento natural das minhocas (gravidade e quedas);
  * o comportamento dos objetos (barris, minas, dinamites, bazucas);
  * as explosões e os seus efeitos sobre o mapa, minhocas e objetos.

O objetivo é simular o desenrolar automático do jogo, mesmo sem intervenção do jogador.
-}

module Tarefa3 where

import Data.Either

import Labs2025
import Tarefa0_2025

type Dano = Int
type Danos = [(Posicao,Dano)]

--------------------------------------------------------------------------------
-- * AVANÇAR ESTADO DO JOGO

-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto e) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'

--------------------------------------------------------------------------------
-- *AVANÇO DE MINHOCAS 

-- | Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, 
-- retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca estado _ minhoca = 
    case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just pos -> aplicaGravidadeMinhoca pos minhoca (mapaEstado estado)

-- | Aplica gravidade a uma minhoca (faz cair se estiver no ar/água)
aplicaGravidadeMinhoca :: Posicao -> Minhoca -> Mapa -> Minhoca
aplicaGravidadeMinhoca pos minhoca mapa 
    | not (minhocaDeveCair pos mapa) = minhoca
    | otherwise = 
        let posInferior = movePosicao Sul pos
        in if ePosicaoMatrizValida posInferior mapa
            then minhocaCaiPara posInferior minhoca mapa
            else minhocaSaiDoMapa minhoca

-- | Verifica se minhoca deve cair (está no ar ou água)
minhocaDeveCair :: Posicao -> Mapa -> Bool
minhocaDeveCair pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> terreno == Ar || terreno == Agua
        Nothing -> False

-- | Minhoca cai para nova posição
minhocaCaiPara :: Posicao -> Minhoca -> Mapa -> Minhoca
minhocaCaiPara novaPos minhoca mapa = 
    case encontraPosicaoMatriz novaPos mapa of
        Just Agua -> 
            if vidaMinhoca minhoca == Morta
                then minhoca { posicaoMinhoca = Just novaPos }
                else minhoca { posicaoMinhoca = Just novaPos, vidaMinhoca = Morta }
        _ -> minhoca { posicaoMinhoca = Just novaPos }

-- | Minhoca sai do mapa (perde posição e morre)
minhocaSaiDoMapa :: Minhoca -> Minhoca
minhocaSaiDoMapa minhoca = minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }

--------------------------------------------------------------------------------
-- *AVANÇO DE OBJETOS 

-- >> Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto expluda,uma lista de posições afetadas com o dano associado.

avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto estado _ (Barril pos explode) 
    | explode = Right (calculaDanosExplosao pos 5 (mapaEstado estado))
    | estaNoArOuAgua pos (mapaEstado estado) = Left (Barril pos True)
    | otherwise = Left (Barril pos False)

avancaObjeto estado _ (Disparo pos dir tipo tempo dono) = 
    case tipo of
        Dinamite -> avancaDinamite estado pos dir tempo dono
        Mina -> avancaMina estado pos dir tempo dono
        Bazuca -> 
            if tempo == Just 0 
                then Right (calculaDanosExplosao pos 5 (mapaEstado estado))
            else if not (ePosicaoEstadoLivre pos estado)
                then Right (calculaDanosExplosao pos 5 (mapaEstado estado))
            else 
                let novaPos = movePosicao dir pos
                in if not (ePosicaoMatrizValida novaPos (mapaEstado estado))
                    then Right []
                    else if not (ePosicaoEstadoLivre novaPos estado)
                        then Right (calculaDanosExplosao novaPos 5 (mapaEstado estado))
                        else Left (Disparo novaPos dir Bazuca tempo dono)
        _ -> Left (Disparo pos dir tipo tempo dono)
        

--------------------------------------------------------------------------------
-- * COMPORTAMENTO DAS MINAS

-- | Avança estado da mina
avancaMina :: Estado -> Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Either Objeto Danos
avancaMina estado pos dir tempo dono =
    case tempo of
        Just 0 -> Right (calculaDanosExplosao pos 3 (mapaEstado estado))
        Just n -> 
            let (novaPos, novaDir) = atualizaFisicaMina estado pos dir
            in if not (ePosicaoMatrizValida novaPos (mapaEstado estado))
                  then Right []
                  else Left (Disparo novaPos novaDir Mina (Just (n - 1)) dono)
        Nothing -> 
            if minaDeveAtivar pos dono estado
                then Left (Disparo pos dir Mina (Just 2) dono)
                else 
                    let (novaPos, novaDir) = atualizaFisicaMina estado pos dir
                    in if not (ePosicaoMatrizValida novaPos (mapaEstado estado))
                          then Right []
                          else Left (Disparo novaPos novaDir Mina Nothing dono)

-- | Atualiza física da mina (posição e direção)
atualizaFisicaMina :: Estado -> Posicao -> Direcao -> (Posicao, Direcao)
atualizaFisicaMina estado pos dir =
    let mapa = mapaEstado estado
        posAbaixo = movePosicao Sul pos
        estaNoChao = case encontraPosicaoMatriz posAbaixo mapa of
                        Just Terra -> True
                        Just Pedra -> True
                        _ -> False
    in if estaNoChao
        then (pos, Norte)
        else if dir == Sul && estaNoArOuAgua pos mapa
              then (movePosicao Sul pos, Norte)
              else (pos, dir)

-- | Verifica se mina deve ativar (minhoca inimiga na área)
minaDeveAtivar :: Posicao -> NumMinhoca -> Estado -> Bool
minaDeveAtivar pos dono estado = 
    let area = calculaAreaExplosao pos 3
        minhocasInimigas = [i | (i, m) <- zip [0..] (minhocasEstado estado), 
                               i /= dono, 
                               case posicaoMinhoca m of
                                  Just p -> p `elem` area
                                  Nothing -> False]
    in not (null minhocasInimigas)

-- | Verifica se posição está no ar ou em água 
estaNoArOuAgua :: Posicao -> Mapa -> Bool
estaNoArOuAgua pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just Ar -> True
        Just Agua -> True
        _ -> False

--------------------------------------------------------------------------------
-- * COMPORTAMENTO DA DINAMITE

-- | Avança estado da dinamite
avancaDinamite :: Estado -> Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Either Objeto Danos
avancaDinamite estado pos dir tempo dono =
    case tempo of
        Just 0 -> Right (calculaDanosExplosao pos 7 (mapaEstado estado))
        Just n -> 
            let mapa = mapaEstado estado
                posAbaixo = movePosicao Sul pos
                estaNoChao = case encontraPosicaoMatriz posAbaixo mapa of
                                Just Terra -> True
                                Just Pedra -> True
                                _ -> False
            in if estaNoChao
                then Left (Disparo pos Norte Dinamite (Just (n - 1)) dono)
                else 
                    let (novaPos, novaDir) = calculaMovimentoDinamite pos dir
                    in if not (ePosicaoMatrizValida novaPos mapa)
                        then Right []
                        else Left (Disparo novaPos novaDir Dinamite (Just n) dono)
        Nothing -> Left (Disparo pos dir Dinamite tempo dono)

-- | Calcula movimento da dinamite (parábola)
calculaMovimentoDinamite :: Posicao -> Direcao -> (Posicao, Direcao)
calculaMovimentoDinamite pos dir = 
    case dir of
        Norte -> (movePosicao Sul pos, Norte)
        Sul -> (movePosicao Sul pos, Norte)
        Este -> (movePosicao Este pos, Sudeste)
        Nordeste -> (movePosicao Este pos, Este)
        Sudeste -> (movePosicao Este pos, Sul)
        Oeste -> (movePosicao Oeste pos, Sudoeste)
        Noroeste -> (movePosicao Oeste pos, Oeste)
        Sudoeste -> (movePosicao Oeste pos, Sul)

--------------------------------------------------------------------------------
-- * EXPLOSÕES E DANOS

-- | Calcula área de explosão (todas as posições num quadrado)
calculaAreaExplosao :: Posicao -> Int -> [Posicao]
calculaAreaExplosao (l, c) diametro = 
    let raio = diametro `div` 2
    in [(l + dl, c + dc) | dl <- [-raio..raio], dc <- [-raio..raio]]

-- | Calcula danos de uma explosão
calculaDanosExplosao :: Posicao -> Int -> Mapa -> Danos
calculaDanosExplosao centro diametro mapa = 
    let raio = diametro `div` 2
        posicoesValidas = [(l, c) | l <- [fst centro - raio .. fst centro + raio],
                                    c <- [snd centro - raio .. snd centro + raio],
                                    ePosicaoMatrizValida (l, c) mapa]
        danosComPos = [(pos, calculaDanoPosicao centro pos diametro) | pos <- posicoesValidas]
    in filter (\(_, dano) -> dano > 0) danosComPos

-- | Calcula dano numa posição específica baseado na distância ao centro
calculaDanoPosicao :: Posicao -> Posicao -> Int -> Dano
calculaDanoPosicao (lc, cc) (lp, cp) diametro = 
    let distL = abs (lc - lp)
        distC = abs (cc - cp)
        distMax = max distL distC
        eCardeal = (distL == 0 || distC == 0) && distMax > 0
    in if distL == 0 && distC == 0
        then diametro * 10
        else if eCardeal
            then max 0 ((diametro - 2 * distMax) * 10)
            else max 0 ((diametro - 2 * distMax - 1) * 10)

--------------------------------------------------------------------------------
-- * APLICAÇÃO DE DANOS AO ESTADO

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos estado = 
    let mapaAtualizado = aplicaDanosMapa danos (mapaEstado estado)
        objetosAtualizados = aplicaDanosObjetos danos (objetosEstado estado)
        minhocasAtualizadas = aplicaDanosMinhocas danos (minhocasEstado estado)
    in Estado mapaAtualizado objetosAtualizados minhocasAtualizadas

-- | Aplica danos ao mapa (destrói terra nas posições afetadas)
aplicaDanosMapa :: Danos -> Mapa -> Mapa
aplicaDanosMapa [] mapa = mapa
aplicaDanosMapa ((pos, _):ds) mapa = 
    let mapaAtualizado = destroiPosicao pos mapa
    in aplicaDanosMapa ds mapaAtualizado

-- | Aplica danos aos objetos (barris atingidos explodem)
aplicaDanosObjetos :: Danos -> [Objeto] -> [Objeto]
aplicaDanosObjetos danos objetos = 
    map (ativaBarrilSeDanificado danos) objetos

-- | Ativa barril para explodir se for danificado
ativaBarrilSeDanificado :: Danos -> Objeto -> Objeto
ativaBarrilSeDanificado danos (Barril pos explode) 
    | explode = Barril pos explode
    | any (\(p, _) -> p == pos) danos = Barril pos True
    | otherwise = Barril pos False
ativaBarrilSeDanificado _ obj = obj

-- | Aplica danos às minhocas
aplicaDanosMinhocas :: Danos -> [Minhoca] -> [Minhoca]
aplicaDanosMinhocas danos = map (reduzVidaMinhoca danos)

-- | Reduz vida de uma minhoca baseado nos danos
reduzVidaMinhoca :: Danos -> Minhoca -> Minhoca
reduzVidaMinhoca danos minhoca = 
    case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just pos -> 
            let danoTotal = sum [dano | (p, dano) <- danos, p == pos]
            in if danoTotal <= 0
                then minhoca
                else case vidaMinhoca minhoca of
                    Morta -> minhoca
                    Viva vidaAtual -> 
                        let novaVida = vidaAtual - danoTotal
                        in if novaVida <= 0
                            then minhoca { vidaMinhoca = Morta }
                            else minhoca { vidaMinhoca = Viva novaVida }


{-
-- <<< RESUMO DA TAREFA 3 >>>
A Tarefa 3 trata da evolução automática do estado do jogo a cada instante de tempo (“tick”).
Foram implementadas as regras que fazem as minhocas caírem, os objetos (barris, minas, dinamites e disparos) avançarem ou explodirem, e os respetivos danos serem aplicados ao mapa e às entidades envolvidas.
O objetivo é simular o desenrolar natural do jogo sem intervenção do jogador, garantindo coerência física e lógica em todas as interações.
;)
-}