-- tarefa3 ;)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
{-
Module      : Tarefa3
Description : Avançar tempo do jogo.
Copyright   :  Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
Maintainer  : l1g053@l1g053.2025@l1


Módulo para a realização da Tarefa 3 de LI1/LP1 em 2025/26.
------- . -------
Aqui é tratada a **evolução do estado do jogo ao longo do tempo** – isto é, o que acontece a cada "tick" (instante de tempo).

A função principal, `avancaEstado`, atualiza o jogo considerando:
  * o movimento natural das minhocas (gravidade e quedas);
  * o comportamento dos objetos (barris, minas, dinamites, bazucas);
  * as explosões e os seus efeitos sobre o mapa, minhocas e objetos.

O objetivo é simular o desenrolar automático do jogo, mesmo sem intervenção do jogador.
-}

module Tarefa3 where

import Data.Either
import Data.Maybe (isNothing)
import Labs2025
import Tarefa0_2025

type Dano = Int
type Danos = [(Posicao,Dano)]

--------------------------------------------------------------------------------
-- * AVANÇAR ESTADO DO JOGO

avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = zipWith (avancaMinhoca e) [0..] minhocas
    (objetos',danoss) = partitionEithers $ zipWith (avancaObjeto $ e {minhocasEstado = minhocas'}) [0..] objetos
    e' = Estado mapa objetos' minhocas'

--------------------------------------------------------------------------------
-- * AVANÇO DE MINHOCAS 

avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca estado _ minhoca
    | not (minhocaValidaGlobal estado minhoca) = minhoca {vidaMinhoca = Morta, posicaoMinhoca = Nothing}
    | isNothing (posicaoMinhoca minhoca) = minhoca
    | otherwise = 
        case posicaoMinhoca minhoca of
            Just (x, y) ->
                let mapa = mapaEstado estado
                    temJetpack = minhocaTemDisparoAtivo Jetpack estado minhoca
                    novaPos = if temJetpack
                                 then (x, y)
                                 else (x + 1, y)  -- gravidade normal para baixo
                in if not (ePosicaoMatrizValida novaPos mapa)
                      then minhoca {vidaMinhoca = Morta, posicaoMinhoca = Nothing}
                      else case encontraPosicaoMatriz novaPos mapa of
                        Just Ar -> minhoca {posicaoMinhoca = Just novaPos}
                        Just Agua -> minhoca {vidaMinhoca = Morta, posicaoMinhoca = Just novaPos}
                        _ -> minhoca
            Nothing -> minhoca

--------------------------------------------------------------------------------
-- * FUNÇÕES AUXILIARES PARA MINHOCAS

-- | Verifica se a minhoca é válida no contexto global do estado
minhocaValidaGlobal :: Estado -> Minhoca -> Bool
minhocaValidaGlobal estado minhoca =
    case posicaoMinhoca minhoca of
        Nothing -> True
        Just pos -> ePosicaoMatrizValida pos (mapaEstado estado)

-- | Verifica se a minhoca tem um disparo ativo de um dado tipo de arma
minhocaTemDisparoAtivo :: TipoArma -> Estado -> Minhoca -> Bool
minhocaTemDisparoAtivo arma estado minhoca =
    case posicaoMinhoca minhoca of
        Nothing -> False
        Just posMinhoca ->
            let objetos = objetosEstado estado
                minhocaNum = encontraIndiceMinhoca minhoca (minhocasEstado estado)
            in any (disparoAtivoNaMinhoca posMinhoca minhocaNum arma) objetos
  where
    disparoAtivoNaMinhoca :: Posicao -> Maybe Int -> TipoArma -> Objeto -> Bool
    disparoAtivoNaMinhoca pos (Just n) tipoArma (Disparo posDisparo _ tipoDisparo _ dono) =
        tipoDisparo == tipoArma && dono == n && posDisparo == pos
    disparoAtivoNaMinhoca _ _ _ _ = False

-- | Encontra o índice de uma minhoca na lista de minhocas
encontraIndiceMinhoca :: Minhoca -> [Minhoca] -> Maybe Int
encontraIndiceMinhoca m ms = encontraIndice 0 ms
  where
    encontraIndice _ [] = Nothing
    encontraIndice i (x:xs)
        | posicaoMinhoca x == posicaoMinhoca m && vidaMinhoca x == vidaMinhoca m = Just i
        | otherwise = encontraIndice (i+1) xs

-- | Verifica se a minhoca está viva
minhocaEstaViva :: Minhoca -> Bool
minhocaEstaViva minhoca = 
    case vidaMinhoca minhoca of
        Viva _ -> True
        Morta -> False

-- | Verifica se a minhoca está morta
minhocaEstaMorta :: Minhoca -> Bool
minhocaEstaMorta = not . minhocaEstaViva

-- | Verifica se a posição está abaixo de outra posição
posicaoAbaixoDe :: Posicao -> Posicao
posicaoAbaixoDe (x, y) = (x + 1, y)

--------------------------------------------------------------------------------
-- * FUNÇÕES AUXILIARES PARA OBJETOS

-- | Extrai a posição de um objeto
posicaoDoObjeto :: Objeto -> Posicao
posicaoDoObjeto (Barril pos _) = pos
posicaoDoObjeto (Disparo pos _ _ _ _) = pos

-- | Extrai o tipo de arma de um disparo
tipoDeArmaDoDisparo :: Objeto -> Maybe TipoArma
tipoDeArmaDoDisparo (Disparo _ _ tipo _ _) = Just tipo
tipoDeArmaDoDisparo _ = Nothing

-- | Verifica se um objeto é um barril
eBarril :: Objeto -> Bool
eBarril (Barril _ _) = True
eBarril _ = False

-- | Verifica se um objeto é um disparo
eDisparo :: Objeto -> Bool
eDisparo (Disparo _ _ _ _ _) = True
eDisparo _ = False

-- | Verifica se um objeto é um disparo de um tipo específico
eDisparoDeTipo :: TipoArma -> Objeto -> Bool
eDisparoDeTipo tipo (Disparo _ _ tipoDisparo _ _) = tipo == tipoDisparo
eDisparoDeTipo _ _ = False

-- | Conta quantos objetos de um tipo específico existem no estado
contaObjetosTipo :: (Objeto -> Bool) -> Estado -> Int
contaObjetosTipo predicado estado = length $ filter predicado (objetosEstado estado)

explodeObject :: Objeto -> Danos
explodeObject object = 
    [ (addToPosition (originOfExplosion object) (dx, dy), (diameter - cost) * 10)
      | dx <- [-radius .. radius],
        dy <- [-radius .. radius],
        let cost = weightedCost (dx, dy),
        (diameter - cost) > 0
    ]
    where
    originOfExplosion :: Objeto -> Posicao
    originOfExplosion (Disparo position _ _ _ _) = position
    originOfExplosion (Barril position _) = position

    diameter = case object of
        (Disparo _ _ Dinamite _ _) -> 7
        (Barril _ _) -> 5
        (Disparo _ _ Bazuca _ _) -> 5
        (Disparo _ _ Mina _ _) -> 3
        (Disparo {}) -> 0

    radius = diameter `div` 2

    weightedCost :: Posicao -> Int
    weightedCost (dx, dy) =
        let ax = abs dx; ay = abs dy
        in 2 * max ax ay + min ax ay

    addToPosition :: Posicao -> Posicao -> Posicao
    addToPosition (x, y) (dx, dy) = (x + dx, y + dy)

-- | Obtém o diâmetro de explosão para um tipo de objeto
diametroExplosao :: Objeto -> Int
diametroExplosao (Disparo _ _ Dinamite _ _) = 7
diametroExplosao (Barril _ _) = 5
diametroExplosao (Disparo _ _ Bazuca _ _) = 5
diametroExplosao (Disparo _ _ Mina _ _) = 3
diametroExplosao (Disparo {}) = 0

-- | Calcula o raio de explosão a partir do diâmetro
raioExplosao :: Int -> Int
raioExplosao diametro = diametro `div` 2

-- | Calcula o custo ponderado de uma posição relativa ao centro da explosão
custoPonderado :: Posicao -> Int
custoPonderado (dx, dy) =
    let ax = abs dx; ay = abs dy
    in 2 * max ax ay + min ax ay

-- | Verifica se uma posição sofre dano de uma explosão
posicaoSofreDano :: Posicao -> Posicao -> Int -> Bool
posicaoSofreDano centroExplosao posicao diametro =
    let (cx, cy) = centroExplosao
        (px, py) = posicao
        (dx, dy) = (px - cx, py - cy)
        custo = custoPonderado (dx, dy)
    in (diametro - custo) > 0

-- | Calcula o dano em uma posição específica dado o centro e diâmetro
calculaDanoNaPosicao :: Posicao -> Posicao -> Int -> Dano
calculaDanoNaPosicao centro posicao diametro =
    let (cx, cy) = centro
        (px, py) = posicao
        (dx, dy) = (px - cx, py - cy)
        custo = custoPonderado (dx, dy)
    in max 0 ((diametro - custo) * 10)

shouldExplode :: Estado -> Objeto -> Bool
shouldExplode _ (Barril _ aboutToExplode) = aboutToExplode
shouldExplode _ (Disparo _ _ _ (Just 0) _) = True
shouldExplode state (Disparo position _ Bazuca _ _) = not (ePosicaoEstadoLivre position state)
shouldExplode _ _ = False

--------------------------------------------------------------------------------
-- * AVANÇO DE OBJETOS 

avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto state _ object
    | shouldExplode state object = Right (explodeObject object)
    | otherwise = case object of
        (Barril (x, y) _) ->
            if ePosicaoMapaLivre (x + 1, y) (mapaEstado state)
                then Left Barril {posicaoBarril = (x, y), explodeBarril = True}
                else Left object
        
        gunshot@(Disparo position direction typeOfGunshot timeUntilExplosion _) ->
            case typeOfGunshot of
                Bazuca ->
                    let (px, py) = position
                        (dx, dy) = delta direction
                        newPos = (px + dx, py + dy)
                    in if ePosicaoMatrizValida newPos (mapaEstado state)
                          then Left gunshot {posicaoDisparo = newPos}
                          else Right []
                
                Dinamite ->
                    let (px, py) = position
                        posBelow = (px + 1, py)
                        onFloor = not (ePosicaoMapaLivre posBelow (mapaEstado state)) && direction == Norte
                        newTempo = case timeUntilExplosion of
                            Just n | n > 0 -> Just (n - 1)
                            _ -> timeUntilExplosion
                    in if not onFloor
                          then
                            let nextDir
                                  | (direction == Norte) || (direction == Sul) = (Sul, Norte)
                                  | direction == Nordeste = (Nordeste, Este)
                                  | direction == Este = (Sudeste, Sudeste)
                                  | direction == Sudeste = (Sul, Sul)
                                  | direction == Noroeste = (Oeste, Oeste)
                                  | direction == Oeste = (Sudoeste, Sudoeste)
                                  | direction == Sudoeste = (Sul, Sul)
                                  | otherwise = (Sul, Norte)
                                (moveDir, newDir) = nextDir
                                (dx, dy) = delta moveDir
                                newPos = (px + dx, py + dy)
                            in if ePosicaoMapaLivre newPos (mapaEstado state)
                                  then Left gunshot {posicaoDisparo = newPos, direcaoDisparo = newDir, tempoDisparo = newTempo}
                                  else Left gunshot {direcaoDisparo = Norte, tempoDisparo = newTempo}
                          else Left gunshot {direcaoDisparo = Norte, tempoDisparo = newTempo}
                
                Mina ->
                    let (px, py) = position
                        (dx, dy) = delta Sul
                        newPos = (px + dx, py + dy)
                        newTempo = case timeUntilExplosion of
                            Just n | n > 0 -> Just (n - 1)
                            _ -> Just 2
                    in if ePosicaoMapaLivre newPos (mapaEstado state)
                          then
                            ( if ePosicaoMatrizValida newPos (mapaEstado state)
                                then Left gunshot {posicaoDisparo = newPos, direcaoDisparo = Norte, tempoDisparo = newTempo}
                                else Right []
                            )
                          else Left gunshot {direcaoDisparo = Norte, tempoDisparo = newTempo}
                
                Jetpack -> Left gunshot
                Escavadora -> Left gunshot

-- | Calcula o deslocamento (dx, dy) para uma dada direção
delta :: Direcao -> (Int, Int)
delta Norte = (-1, 0)
delta Sul = (1, 0)
delta Este = (0, 1)
delta Oeste = (0, -1)
delta Nordeste = (-1, 1)
delta Sudeste = (1, 1)
delta Noroeste = (-1, -1)
delta Sudoeste = (1, -1)

-- | Soma dois deslocamentos (vetores)
somaDeslocamento :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaDeslocamento (dx1, dy1) (dx2, dy2) = (dx1 + dx2, dy1 + dy2)

-- | Aplica um deslocamento a uma posição
aplicaDeslocamento :: Posicao -> (Int, Int) -> Posicao
aplicaDeslocamento (x, y) (dx, dy) = (x + dx, y + dy)

-- | Calcula a próxima posição dada uma direção
proximaPosicao :: Posicao -> Direcao -> Posicao
proximaPosicao pos dir = aplicaDeslocamento pos (delta dir)

--------------------------------------------------------------------------------
-- * APLICAÇÃO DE DANOS AO ESTADO

-- | Aplica dano a uma minhoca individual
aplicaDanoMinhoca :: Dano -> Minhoca -> Minhoca
aplicaDanoMinhoca damage worm =
    let health = vidaMinhoca worm
    in worm
        { vidaMinhoca = case health of
            Morta -> Morta
            Viva h -> if h <= damage then Morta else Viva (h - damage)
        }

-- | Calcula o dano total em uma posição
danoTotalNaPosicao :: Posicao -> Danos -> Dano
danoTotalNaPosicao pos danos = sum [dano | (p, dano) <- danos, p == pos]

-- | Verifica se uma posição tem dano
posicaoTemDano :: Posicao -> Danos -> Bool
posicaoTemDano pos danos = any (\(p, _) -> p == pos) danos

-- | Filtra danos que afetam uma posição específica
danosNaPosicao :: Posicao -> Danos -> Danos
danosNaPosicao pos danos = filter (\(p, _) -> p == pos) danos

-- | Aplica danos a uma lista de minhocas
aplicaDanosAsMinhocas :: Danos -> [Minhoca] -> [Minhoca]
aplicaDanosAsMinhocas danos = map (aplicaDanosAUmaMinhoca danos)
  where
    aplicaDanosAUmaMinhoca :: Danos -> Minhoca -> Minhoca
    aplicaDanosAUmaMinhoca ds minhoca =
        let pos = posicaoMinhoca minhoca
            relevantDamages = [dano | (p, dano) <- ds, Just p == pos]
            totalDamage = sum relevantDamages
        in if totalDamage > 0
              then aplicaDanoMinhoca totalDamage minhoca
              else minhoca

-- | Destrói terreno em uma posição se for destrutível
destruirTerrenoNaPosicao :: Posicao -> Dano -> Mapa -> Mapa
destruirTerrenoNaPosicao pos dano mapa
    | dano > 0 && ePosicaoMatrizValida pos mapa =
        case encontraPosicaoMatriz pos mapa of
            Just Terra -> atualizaPosicaoMatriz pos Ar mapa
            _ -> mapa
    | otherwise = mapa

aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos damages state = 
    state {minhocasEstado = updatedWorms, mapaEstado = updatedMap}
    where
    worms = minhocasEstado state
    mapa = mapaEstado state

    updatedWorms = aplicaDanosAsMinhocas damages worms
    updatedMap = foldr (uncurry destruirTerrenoNaPosicao) mapa damages

