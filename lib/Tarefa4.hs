-- tarefa4 - Bot Inteligente ;)
{-
Module      : Tarefa4
Description : Bot inteligente que não se mata.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

 Implementação do Bot Inteligente

== Arquitetura

O bot foi implementado com três componentes principais:

1. __Função Principal__: 'tatica' executa 100 jogadas sequenciais, 
   avançando o estado do jogo a cada turno

2. __Geração de Jogadas Seguras__: 'geraJogadasSeguras' cria jogadas que:
   
   * Evitam água e buracos
   * Verificam segurança de explosões com raio hexagonal
   * Consideram disponibilidade de armas no inventário

3. __Sistema de Avaliação__: 'avaliaJogadaInteligente' pontua cada jogada:
   
   * Dinamite: 500 pts (distância ≤ 5)
   * Bazuca: 450 pts (distância 5-9)
   * Movimentos: 350-700 pts + bónus de aproximação (300 pts)
   * Penalização: -100000 pts para jogadas perigosas

== Problema: Falha na Priorização

__O bot não funciona corretamente devido a conflitos no sistema de pontuação:__

* Os bónus direcionais (350-700) excedem o bónus de aproximação (300),
  levando a movimentos subótimos que ignoram posicionamento tático

* Pontuações de armas competem desequilibradamente com movimentos,
  resultando em subutilização de armas mesmo em situações favoráveis

* Ausência de ponderação dinâmica baseada no contexto do jogo
  (vida restante, número de adversários, urgência da situação)

* Granularidade excessiva (diferenças de 50 pts) cria hierarquia
  artificial de direções que sobrepõe-se a objetivos táticos

__Consequências:__ O bot favorece direções específicas independentemente 
da estratégia, toma decisões baseadas em critérios secundários e 
não maximiza eficácia em combate.

__Solução Necessária:__ Rebalancear pontuações, eliminar bónus arbitrários,
implementar ponderação dinâmica e estabelecer hierarquia clara:
sobrevivência > ataque eficaz > movimento tático > exploração.
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
jogadaTatica tick estado =
    let minhocas = minhocasEstado estado
        num = tick `mod` length minhocas
        minhoca = minhocas !! num
    in escolheMelhorJogadaInteligente tick num minhoca estado

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
    let -- Direções com ordem que favorece exploração variada
        direcoes = [Norte, Oeste, Sul, Nordeste, Noroeste, Sudeste, Sudoeste, Este]
        mapa = mapaEstado estado
        
        -- Movimentos: só permite se a posição destino for válida E segura
        movs = [Move d | d <- direcoes, 
                let destino = calculaPosicaoDestino pos d,
                posValida destino mapa,
                terrenoNaPos destino mapa /= Agua]
        
        armas = [Dispara Jetpack d    | d <- direcoes, jetpackMinhoca minhoca > 0] ++
                [Dispara Dinamite d   | d <- direcoes, dinamiteMinhoca minhoca > 0] ++
                [Dispara Bazuca d     | d <- direcoes, bazucaMinhoca minhoca > 0] ++
                [Dispara Escavadora d | d <- direcoes, escavadoraMinhoca minhoca > 0]
    in armas ++ movs



-- | Verifica se um movimento é seguro (não cai em água ou buraco)
movimentoSeguro :: Posicao -> Direcao -> Estado -> Bool
movimentoSeguro pos dir estado =
    let novaPos = calculaPosicaoDestino pos dir
        mapa = mapaEstado estado
    in posValida novaPos mapa &&
       (terrenoNaPos novaPos mapa == Ar || terrenoNaPos novaPos mapa == Terra) && -- ^ Permite considerar Terra como destino (o motor do jogo resolve o bloqueio)
       not (temBarril novaPos estado)


-- | Verifica se um disparo é seguro usando o raio hexagonal correto
disparoSeguroHex :: Posicao -> Direcao -> Int -> NumMinhoca -> Estado -> Bool
disparoSeguroHex posBot dir diametro numBot estado =
    let posDisparo = calculaPosicaoDestino posBot dir
        raio = diametro `div` 2
        
        -- Verifica todas as posições dentro do raio hexagonal de explosão
        posicoesExplosao = posicoesNoRaioHex posDisparo raio diametro
        
        -- O bot está na área de explosão?
        botNaExplosao = posBot `elem` posicoesExplosao
        
        -- Outras minhocas do bot (ímpares) estão na explosão?
        minhocas = minhocasEstado estado
        aliados = [(i, m) | (i, m) <- zip [0..] minhocas, 
                   odd i, i /= numBot, estaViva m]
        
        aliadosEmPerigo = any (\(_, m) -> 
            case posicaoMinhoca m of
                Just p -> p `elem` posicoesExplosao
                Nothing -> False) aliados
        
    in not botNaExplosao && not aliadosEmPerigo

-- | Calcula todas as posições dentro do raio hexagonal de uma explosão
posicoesNoRaioHex :: Posicao -> Int -> Int -> [Posicao]
posicoesNoRaioHex (l, c) raio diametro =
    [ (l + dl, c + dc)
    | dl <- [-raio..raio]
    , dc <- [-raio..raio]
    , let custo = 2 * max (abs dl) (abs dc) + min (abs dl) (abs dc)
    , (diametro - custo) > 0
    ]--------------------------------------------------------------------------------
-- * AVALIAÇÃO INTELIGENTE DE JOGADAS

-- | Avalia uma jogada considerando distância aos adversários e segurança
avaliaJogadaInteligente :: Jogada -> NumMinhoca -> Posicao -> [(NumMinhoca, Posicao)] -> Estado -> Pontuacao
avaliaJogadaInteligente jogada num posBot adversarios estado =
    let adversarioMaisProximo = if null adversarios then Nothing 
                                else Just $ minimumBy (comparing (\(_, p) -> distancia posBot p)) adversarios
        distanciaAdv = case adversarioMaisProximo of
                         Just (_, p) -> distancia posBot p
                         Nothing -> 100
    in case jogada of
        -- Dinamite: só útil se inimigo perto
        Dispara Dinamite dir -> 
            if distanciaAdv <= 5 then 500 else 50
        
        -- Bazuca: média distância
        Dispara Bazuca dir -> 
            if distanciaAdv >= 5 && distanciaAdv <= 9 then 450 else 50
        
        -- Escavadora: BAIXA prioridade - só se estiver preso
        Dispara Escavadora dir ->
            let posDisparo = calculaPosicaoDestino posBot dir
                mapa = mapaEstado estado
            in if terrenoNaPos posDisparo mapa == Terra && bloqueado posBot dir estado
               then 100  -- Só se estiver mesmo bloqueado
               else 10
        
        -- Mina: baixa prioridade
        Dispara Mina dir -> 30
        
        -- Jetpack: mobilidade
        Dispara Jetpack dir -> 200
        
        -- MOVIMENTOS: ALTA PRIORIDADE
        Move dir ->
            let novaPos = calculaPosicaoDestino posBot dir
                mapa = mapaEstado estado
            in if not (posValida novaPos mapa) || terrenoNaPos novaPos mapa == Agua || temBarril novaPos estado
               then -100000
               else 
                   let novaDistancia = case adversarioMaisProximo of
                                         Just (_, p) -> distancia novaPos p
                                         Nothing -> 100
                       -- Bônus se aproxima do inimigo
                       bonusAproxima = if null adversarios 
                                      then 0
                                      else if novaDistancia < distanciaAdv 
                                           then 300
                                           else 0
                       -- Bônus por direção (variedade)
                       bonusDir = case dir of
                           Norte    -> 700
                           Oeste    -> 650
                           Sul      -> 600
                           Nordeste -> 550
                           Noroeste -> 500
                           Sudeste  -> 450
                           Sudoeste -> 400
                           Este     -> 350
                   in bonusDir + bonusAproxima
        
        _ -> 1
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
  any (\obj -> case obj of
                 Barril posObj _ -> posObj == pos
                 _ -> False) (objetosEstado estado)

bloqueado :: Posicao -> Direcao -> Estado -> Bool
bloqueado pos dir estado =
  let frente = calculaPosicaoDestino pos dir
      mapa = mapaEstado estado
  in not (posValida frente mapa) ||
     terrenoNaPos frente mapa /= Ar
