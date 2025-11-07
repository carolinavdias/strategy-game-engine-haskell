{-|
Module      : Tarefa1_2025
Description : Validação de estados do jogo.
-}
module Tarefa1 where
import Labs2025
import Tarefa0_2025

-- | Valida se um estado é válido segundo as regras do jogo.
validaEstado :: Estado -> Bool
validaEstado estado = 
    validaMapa (mapaEstado estado) &&
    validaObjetos (objetosEstado estado) (mapaEstado estado) (minhocasEstado estado) &&
    validaMinhocas (minhocasEstado estado) (mapaEstado estado) (objetosEstado estado)

-- * VALIDAÇÃO DE MAPA 

-- | O mapa é válido quando:
-- __1.__ Não vazio
-- __2.__ Denota corretamente uma grelha (mesmo número de colunas em todas as linhas)
-- __3.__ Contém terrenos válidos (Ar/Agua/Terra/Pedra)

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

-- | Valida todos os objetos do estado.
validaObjetos :: [Objeto] -> Mapa -> [Minhoca] -> Bool
validaObjetos objs mapa minhocas = 
    all (\obj -> validaObjeto obj mapa minhocas objs) objs &&
    validaDisparosUnicos objs

-- | Valida um objeto individual.
validaObjeto :: Objeto -> Mapa -> [Minhoca] -> [Objeto] -> Bool
validaObjeto obj mapa minhocas todosObjs = 
    case obj of
        Barril pos _ -> 
            posicaoValidaELivreNoMapa pos mapa &&
            posicaoNaoOcupadaPorOutroBarril pos todosObjs obj &&
            posicaoNaoOcupadaPorMinhoca pos minhocas
        
        Disparo pos dir tipo tempo dono -> 
            validaDisparo pos dir tipo tempo dono mapa minhocas

-- | Valida um disparo.
validaDisparo :: Posicao -> Direcao -> TipoArma -> Maybe Ticks -> NumMinhoca -> Mapa -> [Minhoca] -> Bool
validaDisparo pos dir tipo tempo dono mapa minhocas =
    tipo /= Jetpack &&
    tipo /= Escavadora &&
    validaPosicaoDisparo pos dir tipo mapa &&
    validaTempoDisparo tipo tempo &&
    donoValido dono minhocas

-- | Verifica se a posição é válida e livre no mapa (não contém terreno opaco).
posicaoValidaELivreNoMapa :: Posicao -> Mapa -> Bool
posicaoValidaELivreNoMapa pos mapa = 
    ePosicaoMatrizValida pos mapa &&
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> False

-- | Valida posição de disparo.
-- Bazuca pode perfurar terreno opaco apenas na superfície.
validaPosicaoDisparo :: Posicao -> Direcao -> TipoArma -> Mapa -> Bool
validaPosicaoDisparo pos dir Bazuca mapa = 
    ePosicaoMatrizValida pos mapa &&
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> 
            if eTerrenoOpaco terreno
            then posicaoAnteriorNaoOpaca pos dir mapa
            else True
        Nothing -> False
validaPosicaoDisparo pos _ _ mapa = posicaoValidaELivreNoMapa pos mapa

-- | Verifica se a posição anterior (direção oposta) não é opaca.
posicaoAnteriorNaoOpaca :: Posicao -> Direcao -> Mapa -> Bool
posicaoAnteriorNaoOpaca pos dir mapa = 
    let posAnt = movePosicao (direcaoOposta dir) pos
    in case encontraPosicaoMatriz posAnt mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> True

-- | Retorna a direção oposta.
direcaoOposta :: Direcao -> Direcao
direcaoOposta Norte = Sul
direcaoOposta Sul = Norte
direcaoOposta Este = Oeste
direcaoOposta Oeste = Este
direcaoOposta Nordeste = Sudoeste
direcaoOposta Sudoeste = Nordeste
direcaoOposta Noroeste = Sudeste
direcaoOposta Sudeste = Noroeste

-- | Verifica se a posição não está ocupada por outro barril.
posicaoNaoOcupadaPorOutroBarril :: Posicao -> [Objeto] -> Objeto -> Bool
posicaoNaoOcupadaPorOutroBarril pos objs objAtual = 
    all (\obj -> case obj of
            Barril p _ -> p /= pos || objetosIguais obj objAtual
            _ -> True) objs

-- | Verifica se dois objetos são o mesmo (para evitar auto-comparação).
objetosIguais :: Objeto -> Objeto -> Bool
objetosIguais (Barril p1 e1) (Barril p2 e2) = p1 == p2 && e1 == e2
objetosIguais _ _ = False

-- | Verifica se a posição não está ocupada por uma minhoca.
posicaoNaoOcupadaPorMinhoca :: Posicao -> [Minhoca] -> Bool
posicaoNaoOcupadaPorMinhoca pos minhocas = 
    all (\m -> posicaoMinhoca m /= Just pos) minhocas

-- | Verifica se o dono é um índice válido na lista de minhocas.
donoValido :: NumMinhoca -> [Minhoca] -> Bool
donoValido num minhocas = num >= 0 && num < length minhocas

-- | Valida o tempo do disparo conforme o tipo de arma.
validaTempoDisparo :: TipoArma -> Maybe Ticks -> Bool
validaTempoDisparo Bazuca Nothing = True
validaTempoDisparo Bazuca (Just _) = False
validaTempoDisparo Mina Nothing = True
validaTempoDisparo Mina (Just t) = t >= 0 && t <= 2
validaTempoDisparo Dinamite (Just t) = t >= 0 && t <= 4
validaTempoDisparo Dinamite Nothing = False
validaTempoDisparo _ _ = False

-- | Valida que cada dono não tem disparos duplicados do mesmo tipo.
validaDisparosUnicos :: [Objeto] -> Bool
validaDisparosUnicos objs = 
    let disparos = [(tipo, dono) | Disparo _ _ tipo _ dono <- objs]
    in length disparos == length (removerDuplicados disparos)

-- | Remove duplicados de uma lista.
removerDuplicados :: Eq a => [a] -> [a]
removerDuplicados [] = []
removerDuplicados (x:xs) = x : removerDuplicados (filter (/= x) xs)

-- *  VALIDAÇÃO DE MINHOCAS 

-- | Cada minhoca é válida, o que se verifica quando:
--Tem uma posição válida e livre, ou opcionalmente nenhuma posição.
--A sua posição não se encontra ocupada por um objeto de barril ou por outra minhoca.
--Quando não tem posição ou se encontra numa posição em que o terreno é água, a minhoca tem que estar obrigatoriamente morta.
--Quando viva, a vida da minhoca é um inteiro entre 0 e 100.
--A quantidade de munições das diversas armas é um número inteiro maior ou igual a 0.

-- | Valida todas as minhocas.
validaMinhocas :: [Minhoca] -> Mapa -> [Objeto] -> Bool
validaMinhocas minhocas mapa objs = 
    all (\m -> validaMinhoca m mapa objs minhocas) minhocas &&
    validaPosicoesMinhocasUnicas minhocas

-- | Valida uma minhoca individual.
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

-- | Verifica se a posição está livre no estado (não tem barril nem outra minhoca).
posicaoLivreNoEstado :: Posicao -> [Objeto] -> [Minhoca] -> Minhoca -> Bool
posicaoLivreNoEstado pos objs minhocas minhocaAtual =
    posicaoNaoTemBarril pos objs &&
    posicaoNaoOcupadaPorOutraMinhoca pos minhocas minhocaAtual

-- | Verifica se a posição não tem barril.
posicaoNaoTemBarril :: Posicao -> [Objeto] -> Bool
posicaoNaoTemBarril pos objs = 
    all (\obj -> case obj of
            Barril p _ -> p /= pos
            _ -> True) objs

-- | Verifica se a posição não está ocupada por outra minhoca.
posicaoNaoOcupadaPorOutraMinhoca :: Posicao -> [Minhoca] -> Minhoca -> Bool
posicaoNaoOcupadaPorOutraMinhoca pos minhocas minhocaAtual = 
    all (\m -> posicaoMinhoca m /= Just pos || 
               minhocasIguais m minhocaAtual) minhocas

-- | Compara minhocas (são iguais se tiverem a mesma posição e vida).
minhocasIguais :: Minhoca -> Minhoca -> Bool
minhocasIguais m1 m2 = 
    posicaoMinhoca m1 == posicaoMinhoca m2 &&
    vidaMinhoca m1 == vidaMinhoca m2

-- | Verifica se minhoca em água está morta.
validaMinhocaEmAgua :: Posicao -> Mapa -> Minhoca -> Bool
validaMinhocaEmAgua pos mapa minhoca = 
    case encontraPosicaoMatriz pos mapa of
        Just Agua -> vidaMinhoca minhoca == Morta
        _ -> True

-- | Valida vida e munições da minhoca.
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

-- | Valida que minhocas não compartilham a mesma posição.
validaPosicoesMinhocasUnicas :: [Minhoca] -> Bool
validaPosicoesMinhocasUnicas minhocas = 
    let posicoes = [pos | m <- minhocas, Just pos <- [posicaoMinhoca m]]
    in length posicoes == length (removerDuplicados posicoes)
