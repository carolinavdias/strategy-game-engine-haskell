{-|
Module      : Tarefa1_2025
Description : Validação de estados do jogo.
-}
module Tarefa1_2025 where

import Labs2025
import Tarefa0_2025

-- | Valida se um estado é válido segundo as regras do jogo.
validaEstado :: Estado -> Bool
validaEstado estado = 
    validaMapa (mapaEstado estado) &&
    validaObjetos (objetosEstado estado) (mapaEstado estado) (minhocasEstado estado) &&
    validaMinhocas (minhocasEstado estado) (mapaEstado estado) (objetosEstado estado)

-- ========== VALIDAÇÃO DO MAPA ==========

-- | Valida se o mapa é válido.
validaMapa :: Mapa -> Bool
validaMapa [] = False  -- Não pode ser vazio
validaMapa mapa = todasLinhasMesmoTamanho mapa

-- | Verifica se todas as linhas têm o mesmo tamanho.
todasLinhasMesmoTamanho :: Mapa -> Bool
todasLinhasMesmoTamanho [] = True
todasLinhasMesmoTamanho (l:ls) = all (\linha -> length linha == length l) ls

-- ========== VALIDAÇÃO DE OBJETOS ==========

-- | Valida todos os objetos.
validaObjetos :: [Objeto] -> Mapa -> [Minhoca] -> Bool
validaObjetos objs mapa minhocas = 
    all (\obj -> validaObjeto obj mapa minhocas objs) objs &&
    validaDisparosUnicos objs

-- | Valida um objeto individual.
validaObjeto :: Objeto -> Mapa -> [Minhoca] -> [Objeto] -> Bool
validaObjeto obj mapa minhocas todosObjs = case obj of
    Barril pos -> 
        posicaoValidaELivre pos mapa &&
        posicaoNaoOcupadaPorBarril pos todosObjs obj &&
        posicaoNaoOcupadaPorMinhoca pos minhocas
    
    Mina pos dono -> 
        posicaoValidaELivre pos mapa &&
        posicaoNaoOcupadaPorBarril pos todosObjs obj
    
    Dinamite pos dono tempo -> 
        posicaoValidaELivre pos mapa &&
        posicaoNaoOcupadaPorBarril pos todosObjs obj &&
        tempo >= 0 && tempo <= 4
    
    Disparo tipo dono pos dir tempo -> 
        tipo /= Jetpack && tipo /= Escavadora &&
        validaPosicaoDisparo pos dir mapa tipo &&
        donoValido dono minhocas &&
        validaTempoDisparo tipo tempo

-- | Verifica se a posição é válida e livre (não opaca).
posicaoValidaELivre :: Posicao -> Mapa -> Bool
posicaoValidaELivre (l, c) mapa = 
    l >= 0 && c >= 0 && l < length mapa &&
    (not (null mapa) && c < length (head mapa)) &&
    case encontraPosicaoMatriz (l, c) mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> False

-- | Valida posição de disparo (bazuca pode perfurar superfície).
validaPosicaoDisparo :: Posicao -> Direcao -> Mapa -> TipoArma -> Bool
validaPosicaoDisparo pos@(l, c) dir mapa Bazuca = 
    l >= 0 && c >= 0 && l < length mapa && 
    (not (null mapa) && c < length (head mapa)) &&
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> 
            if eTerrenoOpaco terreno
            then posicaoAnteriorNaoOpaca pos dir mapa
            else True
        Nothing -> False
validaPosicaoDisparo pos _ mapa _ = posicaoValidaELivre pos mapa

-- | Verifica se a posição anterior não é opaca (para bazuca).
-- A posição anterior é calculada na direção oposta ao movimento do projétil.
posicaoAnteriorNaoOpaca :: Posicao -> Direcao -> Mapa -> Bool
posicaoAnteriorNaoOpaca (l, c) dir mapa = 
    let radianos = dir * pi / 180
        dx = -cos radianos  -- Direção oposta
        dy = -sin radianos  -- Direção oposta
        posAnt = (l + round dy, c + round dx)
    in case encontraPosicaoMatriz posAnt mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> True  -- Se sair do mapa, considera válido

-- | Verifica se a posição não está ocupada por outro barril.
posicaoNaoOcupadaPorBarril :: Posicao -> [Objeto] -> Objeto -> Bool
posicaoNaoOcupadaPorBarril pos objs objAtual = 
    all (\obj -> case obj of
            Barril p -> p /= pos || objetosIguais obj objAtual
            _ -> True) objs

-- | Verifica se dois objetos são o mesmo (comparação por posição).
objetosIguais :: Objeto -> Objeto -> Bool
objetosIguais (Barril p1) (Barril p2) = p1 == p2
objetosIguais _ _ = False

-- | Verifica se a posição não está ocupada por uma minhoca.
posicaoNaoOcupadaPorMinhoca :: Posicao -> [Minhoca] -> Bool
posicaoNaoOcupadaPorMinhoca pos minhocas = 
    all (\m -> posicaoMinhoca m /= Just pos) minhocas

-- | Verifica se o dono é um índice válido.
donoValido :: NumMinhoca -> [Minhoca] -> Bool
donoValido num minhocas = num >= 0 && num < length minhocas

-- | Valida o tempo do disparo conforme o tipo de arma.
validaTempoDisparo :: TipoArma -> Maybe Int -> Bool
validaTempoDisparo Bazuca Nothing = True
validaTempoDisparo Bazuca (Just _) = False
validaTempoDisparo Mina Nothing = True
validaTempoDisparo Mina (Just t) = t >= 0 && t <= 2
validaTempoDisparo Dinamite (Just t) = t >= 0 && t <= 4
validaTempoDisparo Dinamite Nothing = False
validaTempoDisparo _ _ = False

-- | Valida que cada dono não tem disparos duplicados do mesmo tipo.
validaDisparosUnicos :: [Objeto] -> Bool
validaDisparosUnicos objs = validaDisparosPorDono objs

validaDisparosPorDono :: [Objeto] -> Bool
validaDisparosPorDono objs = 
    let disparos = [(tipo, dono) | Disparo tipo dono _ _ _ <- objs]
    in length disparos == length (removerDuplicados disparos)

removerDuplicados :: Eq a => [a] -> [a]
removerDuplicados [] = []
removerDuplicados (x:xs) = x : removerDuplicados (filter (/= x) xs)

-- ========== VALIDAÇÃO DE MINHOCAS ==========

-- | Valida todas as minhocas.
validaMinhocas :: [Minhoca] -> Mapa -> [Objeto] -> Bool
validaMinhocas minhocas mapa objs = 
    all (\m -> validaMinhoca m mapa objs minhocas) minhocas &&
    validaPosicoesMinhocasUnicas minhocas

-- | Valida uma minhoca individual.
validaMinhoca :: Minhoca -> Mapa -> [Objeto] -> [Minhoca] -> Bool
validaMinhoca minhoca mapa objs todasMinhocas = case posicaoMinhoca minhoca of
    Nothing -> 
        vidaMinhoca minhoca == 0  -- Sem posição = morta
    Just pos -> 
        validaPosicaoMinhocaComTerreno pos mapa minhoca &&
        posicaoNaoOcupadaPorBarril pos objs undefined &&
        posicaoNaoOcupadaPorOutraMinhoca pos todasMinhocas minhoca &&
        validaVidaEMunicoes minhoca

-- | Valida posição da minhoca considerando o terreno.
validaPosicaoMinhocaComTerreno :: Posicao -> Mapa -> Minhoca -> Bool
validaPosicaoMinhocaComTerreno pos mapa minhoca =
    posicaoValidaELivre pos mapa &&
    validaMinhocaEmAgua pos mapa minhoca

-- | Verifica se minhoca em água está morta.
validaMinhocaEmAgua :: Posicao -> Mapa -> Minhoca -> Bool
validaMinhocaEmAgua pos mapa minhoca = 
    case encontraPosicaoMatriz pos mapa of
        Just Agua -> vidaMinhoca minhoca == 0
        _ -> True

-- | Verifica se a posição não está ocupada por outra minhoca.
posicaoNaoOcupadaPorOutraMinhoca :: Posicao -> [Minhoca] -> Minhoca -> Bool
posicaoNaoOcupadaPorOutraMinhoca pos minhocas minhocaAtual = 
    all (\m -> posicaoMinhoca m /= Just pos || 
               minhocasIguais m minhocaAtual) minhocas

-- | Compara minhocas (assumindo que posições únicas as identificam).
minhocasIguais :: Minhoca -> Minhoca -> Bool
minhocasIguais m1 m2 = posicaoMinhoca m1 == posicaoMinhoca m2

-- | Valida vida e munições da minhoca.
validaVidaEMunicoes :: Minhoca -> Bool
validaVidaEMunicoes minhoca = 
    let vida = vidaMinhoca minhoca
        viva = vida > 0
    in (not viva || (vida >= 0 && vida <= 100)) &&
       jetpackMinhoca minhoca >= 0 &&
       escavadoraMinhoca minhoca >= 0 &&
       bazucaMinhoca minhoca >= 0 &&
       minaMinhoca minhoca >= 0 &&
       dinamiteMinhoca minhoca >= 0

-- | Valida que minhocas não compartilham a mesma posição.
validaPosicoesMinhocasUnicas :: [Minhoca] -> Bool
validaPosicoesMinhocasUnicas minhocas = 
    let posicoes = [pos | m <- minhocas, Just pos <- [posicaoMinhoca m]]
    in length posicoes == length (removerDuplicados posicoes)