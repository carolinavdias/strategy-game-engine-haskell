{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025
import Tarefa1


-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, 
-- | uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada numMinhoca jogada estado
  | not (indiceMinhocaValido numMinhoca estado) = estado
  | otherwise = processaJogada numMinhoca jogada estado


-- | Verifica se o índice da minhoca é válido
indiceMinhocaValido :: NumMinhoca -> Estado -> Bool
indiceMinhocaValido n estado = n >= 0 && n < length (minhocasEstado estado)

-- | Processa a jogada conforme o tipo
processaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
processaJogada numMinhoca (Move dir) estado = 
  moveMinhoca numMinhoca dir estado
processaJogada numMinhoca (Dispara arma dir) estado = 
  disparaArma numMinhoca arma dir estado

-- | Move uma minhoca numa direção
moveMinhoca :: NumMinhoca -> Direcao -> Estado -> Estado
moveMinhoca numMinhoca dir estado =
  let minhoca = minhocasEstado estado !! numMinhoca
  in case (vidaMinhoca minhoca, posicaoMinhoca minhoca) of
    (Morta, _) -> estado
    (_, Nothing) -> estado
    (Viva _, Just pos) ->
      if minhocaNoAr estado pos
        then estado
        else executaMovimento numMinhoca dir pos estado

-- | Executa o movimento da minhoca
executaMovimento :: NumMinhoca -> Direcao -> Posicao -> Estado -> Estado
executaMovimento numMinhoca dir posAtual estado =
  let novaPos = proximaPosicao posAtual dir
      estaNoChao = not (minhocaNoAr estado posAtual)
      movimentoParaCima = dir `elem` [Norte, Nordeste, Noroeste]
  in if movimentoParaCima && not estaNoChao
       then estado
       else if posicaoValidaLivre novaPos estado
              then atualizaPosicaoMinhoca numMinhoca novaPos estado
              else trataPosicaoInvalida numMinhoca novaPos estado

-- | Trata movimento para posição inválida
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

-- | Atualiza a posição de uma minhoca
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

-- | Mata minhoca sem posição
mataMinhocaSemPosicao :: NumMinhoca -> Estado -> Estado
mataMinhocaSemPosicao numMinhoca estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
      novasMinhocas = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas
  in estado { minhocasEstado = novasMinhocas }

-- | Mata minhoca na água
mataMinhocaNaAgua :: NumMinhoca -> Posicao -> Estado -> Estado
mataMinhocaNaAgua numMinhoca pos estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = minhoca { posicaoMinhoca = Just pos, vidaMinhoca = Morta }
      novasMinhocas = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas
  in estado { minhocasEstado = novasMinhocas }

-- | Dispara uma arma
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

-- | Verifica se a minhoca tem munição
temMunicao :: Minhoca -> TipoArma -> Bool
temMunicao m Jetpack = jetpackMinhoca m > 0
temMunicao m Escavadora = escavadoraMinhoca m > 0
temMunicao m Bazuca = bazucaMinhoca m > 0
temMunicao m Mina = minaMinhoca m > 0
temMunicao m Dinamite = dinamiteMinhoca m > 0

-- | Verifica se já existe disparo ativo do mesmo tipo
jaTemDisparoAtivo :: NumMinhoca -> TipoArma -> Estado -> Bool
jaTemDisparoAtivo numMinhoca arma estado =
  any (\obj -> case obj of
    Disparo _ _ tipo _ dono -> dono == numMinhoca && tipo == arma
    _ -> False) (objetosEstado estado)

-- | Executa o disparo da arma
executaDisparo :: NumMinhoca -> TipoArma -> Direcao -> Posicao -> Estado -> Estado
executaDisparo numMinhoca Jetpack dir pos estado =
  let novaPos = proximaPosicao pos dir
      mapa = mapaEstado estado
  in if posicaoDentroMapa novaPos mapa && 
        terrenoNaPosicao novaPos mapa `elem` [Ar, Agua] &&
        not (posicaoOcupadaPorOutraMinhoca numMinhoca novaPos estado) &&
        not (posicaoOcupadaPorObjeto novaPos estado)
       then gastaMunicaoEMove numMinhoca Jetpack novaPos estado
       else estado

executaDisparo numMinhoca Escavadora dir pos estado =
  let novaPos = proximaPosicao pos dir
      mapa = mapaEstado estado
  in if not (posicaoDentroMapa novaPos mapa) 
       then estado
     else let terreno = terrenoNaPosicao novaPos mapa
          in if terreno == Terra
               then -- Destrói terra e move
                    let novoMapa = destroiTerra novaPos mapa
                        estadoComMapaNovo = estado { mapaEstado = novoMapa }
                    in if not (posicaoOcupadaPorOutraMinhoca numMinhoca novaPos estadoComMapaNovo)
                         then gastaMunicaoEMove numMinhoca Escavadora novaPos estadoComMapaNovo
                         else gastaMunicao numMinhoca Escavadora estadoComMapaNovo
               else if terreno `elem` [Ar, Agua] && 
                       not (posicaoOcupadaPorOutraMinhoca numMinhoca novaPos estado)
                      then -- Move sem destruir (posição já livre)
                           gastaMunicaoEMove numMinhoca Escavadora novaPos estado
                      else estado

executaDisparo numMinhoca Bazuca dir pos estado =
  let novaPos = proximaPosicao pos dir
      disparo = Disparo novaPos dir Bazuca Nothing numMinhoca
      estadoComDisparo = adicionaObjeto disparo estado
  in gastaMunicao numMinhoca Bazuca estadoComDisparo

executaDisparo numMinhoca Mina dir pos estado =
  let novaPos = proximaPosicao pos dir
      posDisparo = if posicaoValidaParaMina novaPos estado then novaPos else pos
      disparo = Disparo posDisparo dir Mina Nothing numMinhoca
      estadoComDisparo = adicionaObjeto disparo estado
  in gastaMunicao numMinhoca Mina estadoComDisparo

executaDisparo numMinhoca Dinamite dir pos estado =
  let novaPos = proximaPosicao pos dir
      posDisparo = if posicaoValidaParaDinamite novaPos estado then novaPos else pos
      disparo = Disparo posDisparo dir Dinamite (Just 4) numMinhoca
      estadoComDisparo = adicionaObjeto disparo estado
  in gastaMunicao numMinhoca Dinamite estadoComDisparo

-- | Verifica se posição é válida para colocar mina (terreno Ar/Agua, sem minhocas, sem objetos)
posicaoValidaParaMina :: Posicao -> Estado -> Bool
posicaoValidaParaMina pos estado =
  let mapa = mapaEstado estado
  in posicaoDentroMapa pos mapa &&
     terrenoNaPosicao pos mapa `elem` [Ar, Agua] &&
     not (posicaoOcupadaPorMinhoca pos estado) &&
     not (posicaoOcupadaPorObjeto pos estado)

-- | Verifica se posição é válida para colocar dinamite (terreno Ar/Agua, sem minhocas, sem objetos)
posicaoValidaParaDinamite :: Posicao -> Estado -> Bool
posicaoValidaParaDinamite pos estado =
  let mapa = mapaEstado estado
  in posicaoDentroMapa pos mapa &&
     terrenoNaPosicao pos mapa `elem` [Ar, Agua] &&
     not (posicaoOcupadaPorMinhoca pos estado) &&
     not (posicaoOcupadaPorObjeto pos estado)

-- | Verifica se posição está ocupada por outra minhoca (não a atual)
posicaoOcupadaPorOutraMinhoca :: NumMinhoca -> Posicao -> Estado -> Bool
posicaoOcupadaPorOutraMinhoca numMinhoca pos estado =
  any (\(i, m) -> i /= numMinhoca && posicaoMinhoca m == Just pos) 
      (zip [0..] (minhocasEstado estado))

-- | Verifica se posição está ocupada por uma minhoca (qualquer uma)
posicaoOcupadaPorMinhoca :: Posicao -> Estado -> Bool
posicaoOcupadaPorMinhoca pos estado =
  any (\m -> posicaoMinhoca m == Just pos) (minhocasEstado estado)

-- | Verifica se posição está ocupada por um objeto
posicaoOcupadaPorObjeto :: Posicao -> Estado -> Bool
posicaoOcupadaPorObjeto pos estado =
  any (\obj -> posicaoObjeto obj == pos) (objetosEstado estado)

-- | Gasta munição e move minhoca
gastaMunicaoEMove :: NumMinhoca -> TipoArma -> Posicao -> Estado -> Estado
gastaMunicaoEMove numMinhoca arma novaPos estado =
  let estadoComMunicao = gastaMunicao numMinhoca arma estado
  in atualizaPosicaoMinhoca numMinhoca novaPos estadoComMunicao

-- | Gasta munição da minhoca
gastaMunicao :: NumMinhoca -> TipoArma -> Estado -> Estado
gastaMunicao numMinhoca arma estado =
  let minhocas = minhocasEstado estado
      minhoca = minhocas !! numMinhoca
      novaMinhoca = case arma of
        Jetpack -> minhoca { jetpackMinhoca = jetpackMinhoca minhoca - 1 }
        Escavadora -> minhoca { escavadoraMinhoca = escavadoraMinhoca minhoca - 1 }
        Bazuca -> minhoca { bazucaMinhoca = bazucaMinhoca minhoca - 1 }
        Mina -> minhoca { minaMinhoca = minaMinhoca minhoca - 1 }
        Dinamite -> minhoca { dinamiteMinhoca = dinamiteMinhoca minhoca - 1 }
      novasMinhocas = take numMinhoca minhocas ++ [novaMinhoca] ++ drop (numMinhoca + 1) minhocas
  in estado { minhocasEstado = novasMinhocas }

-- | Adiciona objeto ao estado (filtrando objetos fora do mapa)
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado =
  let pos = posicaoObjeto obj
      mapa = mapaEstado estado
  in if posicaoDentroMapa pos mapa
       then estado { objetosEstado = obj : objetosEstado estado }
       else estado

-- | Destrói terra numa posição
destroiTerra :: Posicao -> Mapa -> Mapa
destroiTerra (l, c) mapa =
  let linha = mapa !! l
      novaLinha = take c linha ++ [Ar] ++ drop (c + 1) linha
  in take l mapa ++ [novaLinha] ++ drop (l + 1) mapa

-- * Funções auxiliares

-- | Calcula a próxima posição dada uma direção
proximaPosicao :: Posicao -> Direcao -> Posicao
proximaPosicao (l, c) Norte = (l - 1, c)
proximaPosicao (l, c) Sul = (l + 1, c)
proximaPosicao (l, c) Este = (l, c + 1)
proximaPosicao (l, c) Oeste = (l, c - 1)
proximaPosicao (l, c) Nordeste = (l - 1, c + 1)
proximaPosicao (l, c) Noroeste = (l - 1, c - 1)
proximaPosicao (l, c) Sudeste = (l + 1, c + 1)
proximaPosicao (l, c) Sudoeste = (l + 1, c - 1)

-- | Verifica se minhoca está no ar
minhocaNoAr :: Estado -> Posicao -> Bool
minhocaNoAr estado (l, c) =
  let posInferior = (l + 1, c)
  in posicaoValidaLivre posInferior estado

-- | Verifica se posição é válida e livre no estado
posicaoValidaLivre :: Posicao -> Estado -> Bool
posicaoValidaLivre pos estado =
  posicaoLivreNoMapa pos (mapaEstado estado) && posicaoLivreNoEstado pos estado

-- | Verifica se posição está livre no mapa
posicaoLivreNoMapa :: Posicao -> Mapa -> Bool
posicaoLivreNoMapa (l, c) mapa
  | l < 0 || l >= length mapa = False
  | c < 0 || c >= length (head mapa) = False
  | otherwise = terrenoNaPosicao (l, c) mapa `elem` [Ar, Agua]

-- | Verifica se posição está livre no estado (sem objetos/minhocas)
posicaoLivreNoEstado :: Posicao -> Estado -> Bool
posicaoLivreNoEstado pos estado =
  not (any (\obj -> posicaoObjeto obj == pos) (objetosEstado estado)) &&
  not (any (\m -> posicaoMinhoca m == Just pos) (minhocasEstado estado))

-- | Obtém posição de um objeto
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril p _) = p
posicaoObjeto (Disparo p _ _ _ _) = p

-- | Obtém terreno numa posição
terrenoNaPosicao :: Posicao -> Mapa -> Terreno
terrenoNaPosicao (l, c) mapa = (mapa !! l) !! c

-- | Verifica se posição está dentro do mapa
posicaoDentroMapa :: Posicao -> Mapa -> Bool
posicaoDentroMapa (l, c) mapa =
  l >= 0 && l < length mapa && c >= 0 && c < length (head mapa)

