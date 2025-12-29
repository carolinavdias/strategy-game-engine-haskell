-- tarefa4 - Bot Inteligente ;)
{-
Module      : Tarefa4
Description : Bot inteligente que não se mata.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Bot melhorado que:
  * NÃO se mata com explosões
  * Calcula distância até adversários
  * Usa armas estrategicamente
  * Evita cair em água ou buracos
  * Prioriza sobrevivência sobre ataque
-}

module Tarefa4 where


import Data.Either (partitionEithers)
import Data.List (minimumBy, maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, fromMaybe, catMaybes)

import Labs2025
import Tarefa2
import Tarefa3

--------------------------------------------------------------------------------
-- * FUNÇÃO PRINCIPAL

-- | Função principal da Tarefa 4. Retorna uma lista de jogadas inteligentes.
tatica :: Estado -> [(NumMinhoca, Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e, []) [0..99]

-- | Aplica uma sequência de jogadas a um estado, avançando o tempo entre jogadas.
avancaTatica :: (Estado, [(NumMinhoca, Jogada)]) -> Ticks -> (Estado, [(NumMinhoca, Jogada)])
avancaTatica (e, js) tick = (avancaJogada j e, j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
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
-- * ESTRATÉGIA INTELIGENTE DO BOT

type Pontuacao = Int
type Distancia = Int

-- | Para um número de ticks, dado um estado, determina a próxima jogada do BOT.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica ticks estado =
    let minhocas = minhocasEstado estado

        -- Minhocas vivas do bot (índices ímpares)
        minhocasBotVivas =
            [ (i, m)
            | (i, m) <- zip [0..] minhocas
            , odd i
            , estaViva m
            ]

    in case minhocasBotVivas of
        -- Caso extremo: nenhuma minhoca do bot viva
        [] -> (0, Move Este)

        -- Escolhe a minhoca ciclicamente usando ticks
        _  ->
            let idx = ticks `mod` length minhocasBotVivas
                (numMinhoca, minhoca) = minhocasBotVivas !! idx
            in escolheMelhorJogadaInteligente ticks numMinhoca minhoca estado

-- | Escolhe a melhor jogada considerando SEGURANÇA e ESTRATÉGIA
escolheMelhorJogadaInteligente :: Ticks -> NumMinhoca -> Minhoca -> Estado -> (NumMinhoca, Jogada)
escolheMelhorJogadaInteligente t num minhoca estado =
    case posicaoMinhoca minhoca of
        Nothing -> (num, Move Este)
        Just pos ->
            let -- Encontra todos os adversários
                adversarios = encontraAdversarios num estado
                
                -- Gera todas as jogadas possíveis
                jogadasPossiveis = geraJogadasSeguras num minhoca pos estado
                
                -- Se não há adversários, só se move
                jogadas = if null adversarios
                          then [Move d | d <- [Norte, Sul, Este, Oeste]]
                          else jogadasPossiveis
                
                -- Avalia cada jogada considerando distância ao adversário mais próximo
                avaliadas = [(j, avaliaJogadaInteligente j num pos adversarios estado) 
                           | j <- jogadas]
                
                -- Escolhe a melhor (maior pontuação)
                -- Escolhe a melhor (maior pontuação)
                melhor = if null avaliadas
                         then 
                            -- Fallback inteligente: tenta direções diferentes baseado em ticks
                            let dirs = [Norte, Sul, Este, Oeste]
                                dirIdx = fromIntegral t `mod` length dirs
                            in (num, Move (dirs !! dirIdx))
                         else 
                            let best = maximumBy (comparing snd) avaliadas
                            in (num, fst best)
                
            in melhor

--------------------------------------------------------------------------------
-- * GERAÇÃO DE JOGADAS SEGURAS

-- | Gera jogadas que NÃO matam o próprio bot
geraJogadasSeguras :: NumMinhoca -> Minhoca -> Posicao -> Estado -> [Jogada]
geraJogadasSeguras num minhoca pos estado =
    let direcoes = [Norte, Sul, Este, Oeste, Nordeste, Noroeste, Sudeste, Sudoeste]
        
        -- Movimentos seguros (não cair em água/vazio)
        movimentosSeguras = [Move d | d <- direcoes, movimentoSeguro pos d estado]
        
        -- Armas que podem ser usadas COM SEGURANÇA
        dinamitesSeguras = [Dispara Dinamite d | d <- direcoes, 
                            dinamiteMinhoca minhoca > 0,
                            disparoSeguro pos d 7 estado]
        
        bazucasSeguras = [Dispara Bazuca d | d <- direcoes,
                          bazucaMinhoca minhoca > 0,
                          disparoSeguro pos d 5 estado]
        
        escavadoras = [Dispara Escavadora d | d <- direcoes,
                       escavadoraMinhoca minhoca > 0]
        
        minasSeguras = [Dispara Mina d | d <- direcoes,
                        minaMinhoca minhoca > 0,
                        disparoSeguro pos d 3 estado]
        
        jetpacks = [Dispara Jetpack d | d <- direcoes,
                    jetpackMinhoca minhoca > 0,
                    movimentoSeguro pos d estado]
        
    in dinamitesSeguras ++ bazucasSeguras ++ escavadoras ++ minasSeguras ++ jetpacks ++ movimentosSeguras

-- | Verifica se um movimento é seguro (não cai em água ou buraco)
movimentoSeguro :: Posicao -> Direcao -> Estado -> Bool
movimentoSeguro pos dir estado =
    let novaPos = calculaPosicaoDestino pos dir
        mapa = mapaEstado estado
    in posValida novaPos mapa &&
       terrenoNaPos novaPos mapa == Ar &&
       not (temBarril novaPos estado)


-- | Verifica se um disparo é seguro (não explode perto do bot)
disparoSeguro :: Posicao -> Direcao -> Int -> Estado -> Bool
disparoSeguro posBot dir diametro estado =
    let posDisparo = calculaPosicaoDestino posBot dir
        raio = diametro `div` 2
        -- Verifica se o bot está na área de explosão
        (l1, c1) = posBot
        (l2, c2) = posDisparo
        distancia = abs (l1 - l2) + abs (c1 - c2)
    in distancia > raio  -- Seguro se estiver longe da explosão

--------------------------------------------------------------------------------
-- * AVALIAÇÃO INTELIGENTE DE JOGADAS

-- | Avalia uma jogada considerando distância aos adversários e segurança
avaliaJogadaInteligente :: Jogada -> NumMinhoca -> Posicao -> [(NumMinhoca, Posicao)] -> Estado -> Pontuacao
avaliaJogadaInteligente jogada num posBot adversarios estado =
    let  -- Encontra adversário mais próximo
        adversarioMaisProximo = if null adversarios
                                then Nothing
                                else Just $ minimumBy (comparing (\(_, p) -> distancia posBot p)) adversarios
        
        distanciaAdv = case adversarioMaisProximo of
                         Just (_, posAdv) -> distancia posBot posAdv
                         Nothing -> 100
        
    in case jogada of
        -- ARMAS: avaliar baseado em distância e dano potencial
        Dispara Dinamite dir ->
            let posDisparo = calculaPosicaoDestino posBot dir
                alinhado = case adversarioMaisProximo of
                            Just (_, posAdv) -> alinhadoComDir posBot posAdv dir
                            Nothing -> False

                danoEstimado =
                    case adversarioMaisProximo of
                      Just (_, posAdv)
                        | alinhado && distancia posDisparo posAdv <= 4 -> 130
                        | distancia posDisparo posAdv <= 3             -> 100
                        | otherwise                                    -> 20
                      Nothing -> 10

                terraDestruida = contaTerraEmExplosao posDisparo 7 (mapaEstado estado)
            in danoEstimado + terraDestruida * 5

        Dispara Bazuca dir ->
            let posDisparo = calculaPosicaoDestino posBot dir
                alinhado = case adversarioMaisProximo of
                             Just (_, posAdv) -> alinhadoComDir posBot posAdv dir
                             Nothing -> False
                danoEstimado =
                    case adversarioMaisProximo of
                        Just (_, posAdv)
                          | alinhado && distancia posBot posAdv <= 8 -> 220
                          | distancia posDisparo posAdv <= 2         -> 150
                          | otherwise                                -> 40
                        Nothing -> 15
                terraDestruida = contaTerraEmExplosao posDisparo 5 (mapaEstado estado)
            in danoEstimado + terraDestruida * 5
        
        Dispara Escavadora dir ->
            let posDisparo = calculaPosicaoDestino posBot dir
                mapa = mapaEstado estado
            in if terrenoNaPos posDisparo mapa == Terra
                then if bloqueado posBot dir estado
                        then 80     --  escavar para desbloquear
                        else 40
                else 5
        
        Dispara Mina dir ->
            let posDisparo = calculaPosicaoDestino posBot dir
            in if distanciaAdv <= 4
               then 80  -- Mina perto do inimigo = bom
               else 20
        
        Dispara Jetpack dir ->
            let novaPos = calculaPosicaoDestino posBot dir
                novaDistancia = case adversarioMaisProximo of
                                  Just (_, posAdv) -> distancia novaPos posAdv
                                  Nothing -> 100
            in if novaDistancia < distanciaAdv
               then 60  -- Aproximar-se = bom
               else 30  -- Afastar-se = menos bom
        
        -- MOVIMENTO: avaliar baseado em aproximação ao adversário
        Move dir ->
            let novaPos = calculaPosicaoDestino posBot dir
                novaDistancia = case adversarioMaisProximo of
                                  Just (_, posAdv) -> distancia novaPos posAdv
                                  Nothing -> 100
                alinhado = case adversarioMaisProximo of
                     Just (_, posAdv) -> alinhadoComDir posBot posAdv dir
                     Nothing -> False
                -- Priorizar aproximação se longe, manter distância se muito perto
            in
                -- Caminho bloqueado → penalizar forte
              if bloqueado posBot dir estado
                then -30

                -- Deve saltar → muito bom
              else if deveSaltar posBot dir estado
                then 60

                -- Está alinhado para disparar → não estragar
              else if alinhado && distanciaAdv <= 8 && distanciaAdv >= 3
                then 3

                --  Muito longe → aproximar
              else if distanciaAdv > 5
                then if novaDistancia < distanciaAdv then 50 else 10

                --  Muito perto → afastar
              else if distanciaAdv < 2
                then if novaDistancia > distanciaAdv then 40 else 5

                -- Distância ideal
              else 25

--------------------------------------------------------------------------------
-- * FUNÇÕES DE UTILIDADE

-- | Encontra todos os adversários vivos
encontraAdversarios :: NumMinhoca -> Estado -> [(NumMinhoca, Posicao)]
encontraAdversarios numAtual estado =
    [ (i, pos) 
    | (i, m) <- zip [0..] (minhocasEstado estado)
    , i /= numAtual
    , estaViva m
    , Just pos <- [posicaoMinhoca m]
    ]

-- | Verifica se uma minhoca está viva
estaViva :: Minhoca -> Bool
estaViva m = case vidaMinhoca m of
    Viva _ -> True
    Morta -> False

-- | Calcula distância Manhattan entre duas posições
distancia :: Posicao -> Posicao -> Int
distancia (l1, c1) (l2, c2) = abs (l1 - l2) + abs (c1 - c2)

-- | Calcula a próxima posição dada uma direção
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

-- | Conta quantos blocos de Terra seriam destruídos numa explosão
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

-- | Verifica se uma posição está dentro dos limites do mapa
posValida :: Posicao -> Mapa -> Bool
posValida (l, c) mapa =
    l >= 0 && l < length mapa && 
    c >= 0 && c < length (head mapa)

-- | Obtém o terreno numa dada posição do mapa
terrenoNaPos :: Posicao -> Mapa -> Terreno
terrenoNaPos (l, c) mapa 
    | posValida (l, c) mapa = (mapa !! l) !! c
    | otherwise = Ar

caminhoLivre :: Posicao -> Direcao -> Int -> Estado -> Bool
caminhoLivre pos dir n estado =
  let mapa = mapaEstado estado
      posicoes = take n $ tail $ iterate (`calculaPosicaoDestino` dir) pos
  in all (\p -> posValida p mapa &&
                terrenoNaPos p mapa == Ar) posicoes

deveSaltar :: Posicao -> Direcao -> Estado -> Bool
deveSaltar pos dir estado =
  let frente = calculaPosicaoDestino pos dir
      frente2 = calculaPosicaoDestino frente dir
      mapa = mapaEstado estado
  in posValida frente mapa &&
     terrenoNaPos frente mapa == Ar &&
     posValida frente2 mapa &&
     terrenoNaPos frente2 mapa == Terra

alinhadoComDir :: Posicao -> Posicao -> Direcao -> Bool
alinhadoComDir posBot posAdv dir =
    let (x1, y1) = posBot
        (x2, y2) = posAdv
        (dxE, dyE) = (x2 - x1, y2 - y1)

        -- vetor do disparo (1 passo na direção)
        (xD, yD) = calculaPosicaoDestino posBot dir
        (dxD, dyD) = (xD - x1, yD - y1)

        -- produto escalar
        dot = dxE * dxD + dyE * dyD
    in dot > 0

temBarril :: Posicao -> Estado -> Bool
temBarril pos estado =
  any (\b -> posBarril b == pos) (objetosEstado estado)

bloqueado :: Posicao -> Direcao -> Estado -> Bool
bloqueado pos dir estado =
  let frente = calculaPosicaoDestino pos dir
      mapa = mapaEstado estado
  in not (posValida frente mapa) ||
     terrenoNaPos frente mapa /= Ar
