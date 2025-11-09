--tarefa 2 ;)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-|
Module      : Tarefa2
Description : Execução de jogadas das minhocas
Copyright   : (c) Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
Maintainer  : l1g053@l1g053.2025@l1
Stability   : experimental
Portability : portable

Este módulo trata da execução das jogadas no jogo, isto é, o que acontece quando uma minhoca se move ou dispara uma arma.

A função principal, `efetuaJogada`, recebe o número da minhoca, a jogada pretendida e o estado atual do jogo, devolvendo o novo estado resultante dessa jogada.

As jogadas podem ser de dois tipos:
1. **Movimento** — a minhoca desloca-se numa direção válida;
2. **Disparo** — a minhoca utiliza uma das armas disponíveis (Jetpack, Escavadora, Bazuca, Mina ou Dinamite).

O módulo garante que todas as jogadas respeitam as regras do jogo:
- uma minhoca morta ou sem posição não pode agir;
- não se pode mover para fora do mapa ou para casas ocupadas;
- o disparo só é possível se houver munições disponíveis e não existir já um disparo ativo do mesmo tipo.
- cair na água ou sair dos limites mata automaticamente a minhoca.
- a escavadora remove blocos de terra, enquanto as outras armas criam disparos no mapa.

-}
module Tarefa2 where

import Labs2025

-- *FUNÇÃO PRINCIPAL

-- | Executa uma jogada para uma minhoca específica.
-- Se o índice for inválido, o estado mantém-se igual.
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
-- *MOVIMENTO DAS MINHOCAS

-- | Move a minhoca na direção indicada, se estiver viva e com posição definida.
moveMinhoca :: NumMinhoca -> Direcao -> Estado -> Estado
moveMinhoca numMinhoca dir estado =
  let minhoca = minhocasEstado estado !! numMinhoca
  in case (vidaMinhoca minhoca, posicaoMinhoca minhoca) of
       (Morta, _) -> estado
       (_, Nothing) -> estado
       (Viva _, Just pos) -> executaMovimento numMinhoca dir pos estado

-- | Executa o movimento, verificando se é possível e válido.
-- O movimento só acontece se a posição de destino for livre.
executaMovimento :: NumMinhoca -> Direcao -> Posicao -> Estado -> Estado
executaMovimento numMinhoca dir posAtual estado =
  let novaPos = proximaPosicao posAtual dir
      estaNoChao = not (minhocaNoAr estado posAtual)
      movimentoParaCima = dir `elem` [Norte, Nordeste, Noroeste]
  in if movimentoParaCima && not estaNoChao
       then estado  -- não pode subir se estiver apoiada
       else if posicaoValidaLivre novaPos estado
              then atualizaPosicaoMinhoca numMinhoca novaPos estado
              else trataPosicaoInvalida numMinhoca novaPos estado

-- | Lida com movimentos que tentam sair do mapa ou entrar em posições inválidas.
-- Se for fora dos limites, a minhoca morre.
-- Se for em água, também morre.
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
      novasMinhocas = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas
  in estado { minhocasEstado = novasMinhocas }

-- | Mata a minhoca quando tenta sair dos limites do mapa.
mataMinhocaSemPosicao :: NumMinhoca -> Estado -> Estado
mataMinhocaSemPosicao numMinhoca estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
  in estado { minhocasEstado = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas }

-- | Mata a minhoca se entrar na água.
mataMinhocaNaAgua :: NumMinhoca -> Posicao -> Estado -> Estado
mataMinhocaNaAgua numMinhoca pos estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = minhoca { posicaoMinhoca = Just pos, vidaMinhoca = Morta }
  in estado { minhocasEstado = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas }

--------------------------------------------------------------------------------
-- *DISPAROS E ARMAS

-- | Efetua o disparo da arma, caso a minhoca tenha munições e esteja viva.
disparaArma :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
disparaArma numMinhoca arma dir estado =
  let minhoca = minhocasEstado estado !! numMinhoca
  in case (vidaMinhoca minhoca, posicaoMinhoca minhoca) of
       (Morta, _) -> estado
       (_, Nothing) -> estado
       (Viva _, Just pos) ->
         if not (temMunicao minhoca arma) then estado
         else if jaTemDisparoAtivo numMinhoca arma estado then estado
         else executaDisparo numMinhoca arma dir pos estado

-- | Verifica se a minhoca ainda tem munição da arma em questão.
temMunicao :: Minhoca -> TipoArma -> Bool
temMunicao m Jetpack   = jetpackMinhoca m > 0
temMunicao m Escavadora = escavadoraMinhoca m > 0
temMunicao m Bazuca    = bazucaMinhoca m > 0
temMunicao m Mina      = minaMinhoca m > 0
temMunicao m Dinamite  = dinamiteMinhoca m > 0

-- | Verifica se já existe um disparo ativo do mesmo tipo de arma para aquela minhoca.
jaTemDisparoAtivo :: NumMinhoca -> TipoArma -> Estado -> Bool
jaTemDisparoAtivo numMinhoca arma estado =
  any (\obj -> case obj of
    Disparo _ _ tipo _ dono -> dono == numMinhoca && tipo == arma
    _ -> False) (objetosEstado estado)

-- | Executa o comportamento de cada tipo de arma.
-- Cada arma tem uma forma diferente de atuar no mapa.
executaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Posicao -> Estado -> Estado
-- Jetpack: move para a próxima posição se estiver livre, gastando combustível.
executaDisparo numMinhoca Jetpack dir pos estado =
  let novaPos = proximaPosicao pos dir
  in if posicaoValidaLivre novaPos estado
       then gastaMunicaoEMove numMinhoca Jetpack novaPos estado
       else gastaMunicao numMinhoca Jetpack estado

-- Escavadora: destrói terra ou move para uma posição livre.
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

-- BAZUCA: adiciona um disparo ao estado.
executaDisparo numMinhoca Bazuca dir pos estado =
  let novaPos = proximaPosicao pos dir
      disparo = Disparo novaPos dir Bazuca Nothing numMinhoca
  in gastaMunicao numMinhoca Bazuca (adicionaObjeto disparo estado)

-- MINA: cria uma mina na posição se for válida, ou mantém-se no mesmo local.
executaDisparo numMinhoca Mina dir pos estado =
  let novaPos = proximaPosicao pos dir
      posDisparo = if posicaoValidaParaMina novaPos estado then novaPos else pos
      disparo = Disparo posDisparo dir Mina Nothing numMinhoca
  in gastaMunicao numMinhoca Mina (adicionaObjeto disparo estado)

-- DINAMITE: coloca dinamite com temporizador de 4 unidades de tempo.
executaDisparo numMinhoca Dinamite dir pos estado =
  let novaPos = proximaPosicao pos dir
      posDisparo = if posicaoValidaParaDinamite novaPos estado then novaPos else pos
      disparo = Disparo posDisparo dir Dinamite (Just 4) numMinhoca
  in gastaMunicao numMinhoca Dinamite (adicionaObjeto disparo estado)

--------------------------------------------------------------------------------
-- *FUNÇÕES DE APOIO

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
        Jetpack   -> minhoca { jetpackMinhoca = jetpackMinhoca minhoca - 1 }
        Escavadora -> minhoca { escavadoraMinhoca = escavadoraMinhoca minhoca - 1 }
        Bazuca    -> minhoca { bazucaMinhoca = bazucaMinhoca minhoca - 1 }
        Mina      -> minhoca { minaMinhoca = minaMinhoca minhoca - 1 }
        Dinamite  -> minhoca { dinamiteMinhoca = dinamiteMinhoca minhoca - 1 }
      novasMinhocas = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas
  in estado { minhocasEstado = novasMinhocas }

-- | Adiciona um novo objeto ao estado, se estiver dentro do mapa.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
  let pos = posicaoObjeto obj
  in if posicaoDentroMapa pos (mapaEstado estado)
       then estado { objetosEstado = obj : objetosEstado estado }
       else estado

-- | Substitui um bloco de terra por ar (usado pela escavadora).
destroiTerra :: Posicao -> Mapa -> Mapa
destroiTerra (l, c) mapa =
  take l mapa ++ [take c (mapa !! l) ++ [Ar] ++ drop (c + 1) (mapa !! l)] ++ drop (l + 1) mapa

--------------------------------------------------------------------------------
-- *FUNÇÕES DE VERIFICAÇÃO DE POSIÇÕES

-- | Calcula a próxima posição a partir de uma direção.
proximaPosicao :: Posicao -> Direcao -> Posicao
proximaPosicao (l, c) Norte = (l - 1, c)
proximaPosicao (l, c) Sul = (l + 1, c)
proximaPosicao (l, c) Este = (l, c + 1)
proximaPosicao (l, c) Oeste = (l, c - 1)
proximaPosicao (l, c) Nordeste = (l - 1, c + 1)
proximaPosicao (l, c) Noroeste = (l - 1, c - 1)
proximaPosicao (l, c) Sudeste = (l + 1, c + 1)
proximaPosicao (l, c) Sudoeste = (l + 1, c - 1)

--------------------------------------------------------------------------------
-- *VERIFICAÇÃO DE POSIÇÕES VÁLIDAS PARA OBJETOS

--Uma posição é válida para colocar uma mina se:
--  |1. estiver dentro dos limites do mapa;
--  |2. o terreno for Ar ou Água;
--  |3. não existir já nenhuma minhoca nessa posição;
--  |4. não existir nenhum outro objeto na mesma casa.
posicaoValidaParaMina :: Posicao -> Estado -> Bool
posicaoValidaParaMina pos estado =
  let mapa = mapaEstado estado
  in posicaoDentroMapa pos mapa &&
     terrenoNaPosicao pos mapa `elem` [Ar, Agua] &&
     not (posicaoOcupadaPorMinhoca pos estado) &&
     not (posicaoOcupadaPorObjeto pos estado)

--Uma posição é válida para colocar dinamite se:
--  |1. estiver dentro dos limites do mapa;
--  |2. o terreno for Ar ou Água;
--  |3. não existir minhoca nem objeto nessa posição.
posicaoValidaParaDinamite :: Posicao -> Estado -> Bool
posicaoValidaParaDinamite pos estado =
  let mapa = mapaEstado estado
  in posicaoDentroMapa pos mapa &&
     terrenoNaPosicao pos mapa `elem` [Ar, Agua] &&
     not (posicaoOcupadaPorMinhoca pos estado) &&
     not (posicaoOcupadaPorObjeto pos estado)

--------------------------------------------------------------------------------
-- *VERIFICAÇÃO DE OCUPAÇÃO

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
-- *FUNÇÕES AUXILIARES DE APOIO AO JOGO

-- | Verifica se a minhoca está no ar.
-- Uma minhoca é considerada “no ar” se a célula abaixo dela for livre.
minhocaNoAr :: Estado -> Posicao -> Bool
minhocaNoAr estado (l, c) =
  let posInferior = (l + 1, c)
  in posicaoValidaLivre posInferior estado

-- | Verifica se a posição é válida e livre (sem minhocas nem objetos).
posicaoValidaLivre :: Posicao -> Estado -> Bool
posicaoValidaLivre pos estado =
  posicaoLivreNoMapa pos (mapaEstado estado) && posicaoLivreNoEstado pos estado

--Uma posição é considerada livre no mapa se:
--  |1. estiver dentro dos limites;
--  |2. o terreno for Ar ou Água.
posicaoLivreNoMapa :: Posicao -> Mapa -> Bool
posicaoLivreNoMapa (l, c) mapa
  | l < 0 || l >= length mapa = False
  | c < 0 || c >= length (head mapa) = False
  | otherwise = terrenoNaPosicao (l, c) mapa `elem` [Ar, Agua]

-- | Verifica se não há objetos nem minhocas na posição.
posicaoLivreNoEstado :: Posicao -> Estado -> Bool
posicaoLivreNoEstado pos estado =
  not (any (\obj -> posicaoObjeto obj == pos) (objetosEstado estado)) &&
  not (any (\m -> posicaoMinhoca m == Just pos) (minhocasEstado estado))

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

--------------------------------------------------------------------------------
-- >> RESUMO DO FUNCIONAMENTO <<

{-|

O módulo `Tarefa2` garante que todas as jogadas feitas no jogo respeitam as regras estabelecidas.

Resumo dos comportamentos principais:
> **Movimento:** A minhoca só se move se estiver viva e a posição de destino for livre e dentro do mapa.
> **Colisões:** Se sair do mapa ou cair na água, morre automaticamente.
> **Disparos:** Cada tipo de arma tem regras específicas:
    - *Jetpack*: move para a direção indicada, gastando combustível.
    - *Escavadora*: destrói blocos de terra e avança.
    - *Bazuca*: cria um disparo na direção indicada.
    - *Mina* e *Dinamite*: colocam objetos no mapa (a dinamite com temporizador)
> **Validações:** Nenhuma jogada é executada se faltar munição, se a minhoca já estiver morta ou se houver disparo ativo igual.

Este módulo é essencial para garantir que as jogadas no jogo decorrem de forma justa e coerente.
-}