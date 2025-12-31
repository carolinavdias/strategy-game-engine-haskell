{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-|
Module      : Tarefa2
Description : Execução de jogadas das minhocas
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
Maintainer  : l1g053@uminho.pt
Stability   : experimental
Portability : portable

= Introdução

Este módulo trata da execução das jogadas no jogo, isto é, o que acontece 
quando uma minhoca se move ou dispara uma arma.

A função principal, 'efetuaJogada', recebe o número da minhoca, a jogada 
pretendida e o estado atual do jogo, devolvendo o novo estado resultante.

= Tipos de Jogadas

As jogadas podem ser de dois tipos:

  1. __Movimento__ — a minhoca desloca-se numa direção válida
  2. __Disparo__ — a minhoca utiliza uma das armas disponíveis

= Regras de Movimento

Segundo o enunciado:

  * Uma minhoca só se pode movimentar se estiver viva
  * A posição de destino tem que estar livre
  * Quando no ar, a minhoca __não se pode movimentar__
  * Para saltar (norte\/nordeste\/noroeste), tem que estar no chão
  * Mover para fora do mapa: minhoca morre e fica sem posição
  * Mover para água: minhoca morre e fica na nova posição

= Regras de Disparo

Cada arma tem comportamento específico:

  * __Jetpack__: move 1 bloco em qualquer direção, se destino livre
  * __Escavadora__: destrói Terra e move para essa posição
  * __Bazuca__: colocado na posição de destino (1 bloco à frente)
  * __Mina__: posição de destino se livre, senão posição atual da minhoca
  * __Dinamite__: posição de destino se livre, senão posição atual, com tempo=4
-}
module Tarefa2 where

import Labs2025

-- * Função Principal

-- | Executa uma jogada para uma minhoca específica.
--
-- Se o índice for inválido, o estado mantém-se igual.
--
-- === Exemplo
-- >>> efetuaJogada 0 (Move Este) estadoInicial
-- Estado { ... minhoca movida para Este ... }
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada numMinhoca jogada estado
  | not (indiceMinhocaValido numMinhoca estado) = estado
  | otherwise = processaJogada numMinhoca jogada estado

-- | Verifica se o índice da minhoca corresponde a uma minhoca existente.
indiceMinhocaValido :: NumMinhoca -> Estado -> Bool
indiceMinhocaValido n estado = n >= 0 && n < length (minhocasEstado estado)

-- | Escolhe o tipo de jogada a processar: movimento ou disparo.
processaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
processaJogada numMinhoca (Move dir) estado = moveMinhoca numMinhoca dir estado
processaJogada numMinhoca (Dispara arma dir) estado = disparaArma numMinhoca arma dir estado

--------------------------------------------------------------------------------
-- * Movimento das Minhocas

-- | Move a minhoca na direção indicada, se estiver viva e com posição definida.
--
-- Uma minhoca só se pode movimentar se:
--
--   * Estiver viva (com vida > 0)
--   * Tiver uma posição definida
--   * __Não__ estiver no ar (regra do enunciado!)
moveMinhoca :: NumMinhoca -> Direcao -> Estado -> Estado
moveMinhoca numMinhoca dir estado =
  let minhoca = minhocasEstado estado !! numMinhoca
  in case (vidaMinhoca minhoca, posicaoMinhoca minhoca) of
       (Morta, _) -> estado
       (Viva hp, _) | hp <= 0 -> estado  -- VIDA = 0 conta como morta!
       (_, Nothing) -> estado
       (Viva _, Just pos) -> executaMovimento numMinhoca dir pos estado

-- | Executa o movimento, verificando se é possível e válido.
--
-- __Regras do enunciado:__
--
--   * \"Quando uma minhoca se encontra no ar, não se pode movimentar.\"
--   * \"Quando uma minhoca se movimenta para cima, a minhoca salta se se encontrar no chão\"
--   * \"A posição do estado para onde se movimenta tem que estar livre\"
executaMovimento :: NumMinhoca -> Direcao -> Posicao -> Estado -> Estado
executaMovimento numMinhoca dir posAtual estado =
  let novaPos = proximaPosicao posAtual dir
      estaNoChao = not (minhocaNoAr estado posAtual)
      estaNoAr = minhocaNoAr estado posAtual
      movimentoParaCima = dir `elem` [Norte, Nordeste, Noroeste]
  in 
     -- REGRA: "Quando uma minhoca se encontra no ar, não se pode movimentar"
     if estaNoAr
       then estado
     -- REGRA: Para saltar tem que estar no chão
     else if movimentoParaCima && not estaNoChao
       then estado
     -- REGRA: Posição de destino deve estar livre
     else if posicaoValidaLivre novaPos estado
       then atualizaPosicaoMinhoca numMinhoca novaPos estado
       else trataPosicaoInvalida numMinhoca novaPos estado

-- | Lida com movimentos para posições inválidas ou perigosas.
--
-- __Comportamentos segundo o enunciado:__
--
--   * \"Quando uma minhoca se move para uma posição inválida (fora do mapa), 
--     esta morre e fica sem posição.\"
--   * \"Quando uma minhoca se move para uma posição de água, 
--     esta morre e fica na nova posição.\"
trataPosicaoInvalida :: NumMinhoca -> Posicao -> Estado -> Estado
trataPosicaoInvalida numMinhoca pos estado =
  let mapa = mapaEstado estado
      (l, c) = pos
      dentroMapa = l >= 0 && l < length mapa && c >= 0 && c < length (head mapa)
  in if not dentroMapa
       then mataMinhocaSemPosicao numMinhoca estado
       else if terrenoNaPosicao pos mapa == Agua
              then mataMinhocaNaAgua numMinhoca pos estado
              else estado

-- | Atualiza a posição de uma minhoca.
-- Se a nova posição for água, ela morre automaticamente.
atualizaPosicaoMinhoca :: NumMinhoca -> Posicao -> Estado -> Estado
atualizaPosicaoMinhoca numMinhoca novaPos estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      terrenoNovo = terrenoNaPosicao novaPos (mapaEstado estado)
      novaMinhoca = if terrenoNovo == Agua
                      then minhoca { posicaoMinhoca = Just novaPos, vidaMinhoca = Morta }
                      else minhoca { posicaoMinhoca = Just novaPos }
      novasMinhocas = substituiNaLista numMinhoca novaMinhoca minhocas
  in estado { minhocasEstado = novasMinhocas }

-- | Mata a minhoca quando tenta sair dos limites do mapa.
-- Fica sem posição (Nothing).
mataMinhocaSemPosicao :: NumMinhoca -> Estado -> Estado
mataMinhocaSemPosicao numMinhoca estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
  in estado { minhocasEstado = substituiNaLista numMinhoca novaMinhoca minhocas }

-- | Mata a minhoca se entrar na água.
-- Mantém a posição na água.
mataMinhocaNaAgua :: NumMinhoca -> Posicao -> Estado -> Estado
mataMinhocaNaAgua numMinhoca pos estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = minhoca { posicaoMinhoca = Just pos, vidaMinhoca = Morta }
  in estado { minhocasEstado = substituiNaLista numMinhoca novaMinhoca minhocas }

--------------------------------------------------------------------------------
-- * Disparos e Armas

-- | Efetua o disparo da arma, caso a minhoca tenha munições e esteja viva.
--
-- __Condições para disparar (enunciado):__
--
--   * \"Quando uma minhoca dispara uma arma tem que estar viva\"
--   * \"ter munições suficientes (>0)\"
--   * \"gasta 1 unidade de munição com o disparo\"
--   * \"Uma minhoca só pode disparar uma arma se não existir um objeto do tipo 
--     de disparo no estado atual para a mesma arma pertencente à mesma minhoca\"
disparaArma :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
disparaArma numMinhoca arma dir estado =
  let minhoca = minhocasEstado estado !! numMinhoca
  in case (vidaMinhoca minhoca, posicaoMinhoca minhoca) of
       (Morta, _) -> estado
       (Viva hp, _) | hp <= 0 -> estado  -- VIDA = 0 conta como morta!
       (_, Nothing) -> estado
       (Viva _, Just pos) ->
         if not (temMunicao minhoca arma) then estado
         else if jaTemDisparoAtivo numMinhoca arma estado then estado
         else executaDisparo numMinhoca arma dir pos estado

-- | Verifica se a minhoca ainda tem munição da arma em questão.
temMunicao :: Minhoca -> TipoArma -> Bool
temMunicao m Jetpack    = jetpackMinhoca m > 0
temMunicao m Escavadora = escavadoraMinhoca m > 0
temMunicao m Bazuca     = bazucaMinhoca m > 0
temMunicao m Mina       = minaMinhoca m > 0
temMunicao m Dinamite   = dinamiteMinhoca m > 0

-- | Verifica se já existe um disparo ativo do mesmo tipo de arma para aquela minhoca.
jaTemDisparoAtivo :: NumMinhoca -> TipoArma -> Estado -> Bool
jaTemDisparoAtivo numMinhoca arma estado =
  any (disparoAtivoDaMinhoca numMinhoca arma) (objetosEstado estado)
  where
    disparoAtivoDaMinhoca :: NumMinhoca -> TipoArma -> Objeto -> Bool
    disparoAtivoDaMinhoca num tipo (Disparo _ _ tipoObj _ dono) = 
      dono == num && tipoObj == tipo
    disparoAtivoDaMinhoca _ _ _ = False

--------------------------------------------------------------------------------
-- * Execução de Cada Arma

-- | Executa o comportamento específico de cada tipo de arma.
--
-- == Comportamentos segundo o enunciado
--
-- === Jetpack
-- \"permite que a minhoca se mova em qualquer direção, 
-- desde que a posição de destino esteja livre\"
--
-- === Escavadora
-- \"destrói a posição de destino se for um terreno de Terra, 
-- e movimenta a minhoca para a nova posição livre nessa direção\"
--
-- === Bazuca
-- \"é colocado na posição de destino, sem tempo e com a direção do disparo\"
--
-- === Mina
-- \"é colocado na posição de destino se estiver livre, 
-- caso contrário na posição atual da minhoca, sem tempo e na direção do disparo\"
--
-- === Dinamite
-- \"é colocado na posição de destino se estiver livre, 
-- caso contrário na posição atual da minhoca, com tempo igual a 4\"
executaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Posicao -> Estado -> Estado

-- | __Jetpack__: move 1 bloco em qualquer direção se destino livre
executaDisparo numMinhoca Jetpack dir pos estado =
  let novaPos = proximaPosicao pos dir  -- 1 bloco apenas!
  in if posicaoValidaLivre novaPos estado
     then gastaMunicaoEMove numMinhoca Jetpack novaPos estado
     else gastaMunicao numMinhoca Jetpack estado  -- Gasta mesmo sem mover

-- | __Escavadora__: destrói Terra ou move para posição livre
executaDisparo numMinhoca Escavadora dir pos estado =
  let novaPos = proximaPosicao pos dir
      mapa = mapaEstado estado
  in if not (posicaoDentroMapa novaPos mapa)
       then estado
       else let terreno = terrenoNaPosicao novaPos mapa
            in if terreno == Terra
                 then let novoMapa = destroiTerra novaPos mapa
                          estadoComMapaNovo = estado { mapaEstado = novoMapa }
                      in if not (posicaoOcupadaPorOutraMinhoca numMinhoca novaPos estadoComMapaNovo)
                           then gastaMunicaoEMove numMinhoca Escavadora novaPos estadoComMapaNovo
                           else gastaMunicao numMinhoca Escavadora estadoComMapaNovo
                 else if terreno `elem` [Ar, Agua] &&
                         not (posicaoOcupadaPorOutraMinhoca numMinhoca novaPos estado)
                      then gastaMunicaoEMove numMinhoca Escavadora novaPos estado
                      else estado

-- | __Bazuca__: \"colocado na posição de destino\" (1 bloco à frente)
executaDisparo numMinhoca Bazuca dir pos estado =
  let posDestino = proximaPosicao pos dir  -- CORRIGIDO: 1 bloco apenas!
      disparo = Disparo posDestino dir Bazuca Nothing numMinhoca
  in gastaMunicao numMinhoca Bazuca (adicionaObjeto disparo estado)

-- | __Mina__: \"posição de destino se livre, caso contrário posição atual\"
executaDisparo numMinhoca Mina dir pos estado =
  let posDestino = proximaPosicao pos dir  -- CORRIGIDO: 1 bloco apenas!
      -- Fallback: posição atual da minhoca (não pos1!)
      posDisparo = if posicaoLivreParaObjeto posDestino estado 
                   then posDestino 
                   else pos  -- CORRIGIDO: posição atual, não pos1!
      disparo = Disparo posDisparo dir Mina Nothing numMinhoca
  in gastaMunicao numMinhoca Mina (adicionaObjeto disparo estado)

-- | __Dinamite__: \"posição de destino se livre, caso contrário posição atual, tempo=4\"
executaDisparo numMinhoca Dinamite dir pos estado =
  let posDestino = proximaPosicao pos dir  -- CORRIGIDO: 1 bloco apenas!
      -- Fallback: posição atual da minhoca (não pos1!)
      posDisparo = if posicaoLivreParaObjeto posDestino estado 
                   then posDestino 
                   else pos  -- CORRIGIDO: posição atual, não pos1!
      disparo = Disparo posDisparo dir Dinamite (Just 4) numMinhoca
  in gastaMunicao numMinhoca Dinamite (adicionaObjeto disparo estado)

--------------------------------------------------------------------------------
-- * Funções de Apoio

-- | Gasta uma munição e move a minhoca para a nova posição.
gastaMunicaoEMove :: NumMinhoca -> TipoArma -> Posicao -> Estado -> Estado
gastaMunicaoEMove numMinhoca arma novaPos estado =
  atualizaPosicaoMinhoca numMinhoca novaPos (gastaMunicao numMinhoca arma estado)

-- | Diminui o número de munições da arma usada pela minhoca.
gastaMunicao :: NumMinhoca -> TipoArma -> Estado -> Estado
gastaMunicao numMinhoca arma estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = case arma of
        Jetpack    -> minhoca { jetpackMinhoca = jetpackMinhoca minhoca - 1 }
        Escavadora -> minhoca { escavadoraMinhoca = escavadoraMinhoca minhoca - 1 }
        Bazuca     -> minhoca { bazucaMinhoca = bazucaMinhoca minhoca - 1 }
        Mina       -> minhoca { minaMinhoca = minaMinhoca minhoca - 1 }
        Dinamite   -> minhoca { dinamiteMinhoca = dinamiteMinhoca minhoca - 1 }
  in estado { minhocasEstado = substituiNaLista numMinhoca novaMinhoca minhocas }

-- | Adiciona um novo objeto ao estado, se estiver dentro do mapa.
-- \"Objetos colocados fora do mapa são eliminados\" (enunciado)
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
  let pos = posicaoObjeto obj
  in if posicaoDentroMapa pos (mapaEstado estado)
       then estado { objetosEstado = obj : objetosEstado estado }
       else estado

-- | Substitui um bloco de Terra por Ar (usado pela escavadora).
destroiTerra :: Posicao -> Mapa -> Mapa
destroiTerra (l, c) mapa =
  take l mapa ++ 
  [take c (mapa !! l) ++ [Ar] ++ drop (c + 1) (mapa !! l)] ++ 
  drop (l + 1) mapa

-- | Substitui um elemento numa lista pelo índice.
substituiNaLista :: Int -> a -> [a] -> [a]
substituiNaLista i novo lista = take i lista ++ [novo] ++ drop (i + 1) lista

--------------------------------------------------------------------------------
-- * Cálculo de Posições

-- | Calcula a próxima posição a partir de uma direção.
--
-- >>> proximaPosicao (5, 5) Norte
-- (4, 5)
-- >>> proximaPosicao (5, 5) Sudeste
-- (6, 6)
proximaPosicao :: Posicao -> Direcao -> Posicao
proximaPosicao (l, c) Norte     = (l - 1, c)
proximaPosicao (l, c) Sul       = (l + 1, c)
proximaPosicao (l, c) Este      = (l, c + 1)
proximaPosicao (l, c) Oeste     = (l, c - 1)
proximaPosicao (l, c) Nordeste  = (l - 1, c + 1)
proximaPosicao (l, c) Noroeste  = (l - 1, c - 1)
proximaPosicao (l, c) Sudeste   = (l + 1, c + 1)
proximaPosicao (l, c) Sudoeste  = (l + 1, c - 1)

--------------------------------------------------------------------------------
-- * Verificação de Posições

-- | Verifica se uma posição é livre para colocar Mina ou Dinamite.
--
-- Uma posição é livre para objeto se:
--
--   1. Está dentro dos limites do mapa
--   2. O terreno é Ar ou Água (não opaco)
--   3. Não existe minhoca nessa posição
--   4. Não existe outro objeto nessa posição
posicaoLivreParaObjeto :: Posicao -> Estado -> Bool
posicaoLivreParaObjeto pos estado =
  let mapa = mapaEstado estado
  in posicaoDentroMapa pos mapa &&
     terrenoNaPosicao pos mapa `elem` [Ar, Agua] &&
     not (posicaoOcupadaPorMinhoca pos estado) &&
     not (posicaoOcupadaPorObjeto pos estado)

-- | Verifica se a posição está ocupada por outra minhoca (diferente da atual).
posicaoOcupadaPorOutraMinhoca :: NumMinhoca -> Posicao -> Estado -> Bool
posicaoOcupadaPorOutraMinhoca numMinhoca pos estado =
  any (\(i, m) -> i /= numMinhoca && posicaoMinhoca m == Just pos)
      (zip [0..] (minhocasEstado estado))

-- | Verifica se existe alguma minhoca na posição.
posicaoOcupadaPorMinhoca :: Posicao -> Estado -> Bool
posicaoOcupadaPorMinhoca pos estado =
  any (\m -> posicaoMinhoca m == Just pos) (minhocasEstado estado)

-- | Verifica se existe algum objeto (barril ou disparo) na posição.
posicaoOcupadaPorObjeto :: Posicao -> Estado -> Bool
posicaoOcupadaPorObjeto pos estado =
  any (\obj -> posicaoObjeto obj == pos) (objetosEstado estado)

--------------------------------------------------------------------------------
-- * Funções Auxiliares

-- | Verifica se a minhoca está \"no ar\".
--
-- Segundo o enunciado: \"Dizemos que uma minhoca/objeto se encontra 'no ar' 
-- quando a posição inferior se encontra livre no estado.\"
minhocaNoAr :: Estado -> Posicao -> Bool
minhocaNoAr estado (l, c) =
  let posInferior = (l + 1, c)
  in posicaoValidaLivre posInferior estado

-- | Verifica se a posição é válida e livre (sem minhocas nem objetos).
posicaoValidaLivre :: Posicao -> Estado -> Bool
posicaoValidaLivre pos estado =
  posicaoLivreNoMapa pos (mapaEstado estado) && 
  posicaoLivreNoEstado pos estado

-- | Uma posição é livre no mapa se está dentro dos limites e o terreno não é opaco.
-- Terrenos opacos: Terra, Pedra. Terrenos não opacos: Ar, Agua.
posicaoLivreNoMapa :: Posicao -> Mapa -> Bool
posicaoLivreNoMapa (l, c) mapa
  | l < 0 || l >= length mapa = False
  | c < 0 || c >= length (head mapa) = False
  | otherwise = terrenoNaPosicao (l, c) mapa `elem` [Ar, Agua]

-- | Verifica se não há objetos nem minhocas na posição.
posicaoLivreNoEstado :: Posicao -> Estado -> Bool
posicaoLivreNoEstado pos estado =
  not (posicaoOcupadaPorObjeto pos estado) &&
  not (posicaoOcupadaPorMinhoca pos estado)

-- | Retorna a posição associada a um objeto.
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril p _)        = p
posicaoObjeto (Disparo p _ _ _ _) = p

-- | Obtém o terreno existente numa determinada posição do mapa.
terrenoNaPosicao :: Posicao -> Mapa -> Terreno
terrenoNaPosicao (l, c) mapa = (mapa !! l) !! c

-- | Verifica se uma posição pertence aos limites do mapa.
posicaoDentroMapa :: Posicao -> Mapa -> Bool
posicaoDentroMapa (l, c) mapa =
  l >= 0 && l < length mapa && c >= 0 && c < length (head mapa)