{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where

import Data.Either

import Labs2025
import Tarefa0_2025

type Dano = Int
type Danos = [(Posicao,Dano)]

-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'


-- ========== AVANÇO DE MINHOCAS ==========

-- | Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, 
-- retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca estado _ minhoca = 
    case posicaoMinhoca minhoca of
        Nothing -> minhoca  -- Sem posição, permanece morta
        Just pos -> aplicaGravidadeMinhoca pos minhoca (mapaEstado estado)

-- | Aplica gravidade a uma minhoca (faz cair se estiver no ar/água)
aplicaGravidadeMinhoca :: Posicao -> Minhoca -> Mapa -> Minhoca
aplicaGravidadeMinhoca pos minhoca mapa 
    | not (minhocaDeveCair pos mapa) = minhoca  -- No chão, não cai
    | otherwise = 
        let posInferior = movePosicao Sul pos
        in if ePosicaoMatrizValida posInferior mapa
            then minhocaCaiPara posInferior minhoca mapa
            else minhocaSaiDoMapa minhoca  -- Cai fora do mapa

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

-- ========== AVANÇO DE OBJETOS ==========

-- | Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto, 
-- retorna o novo estado do objeto no próximo tick ou, caso o objeto expluda, 
-- uma lista de posições afetadas com o dano associado.

avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto estado _ (Barril pos explode) 
    | explode = Right (calculaDanosExplosao pos 5 (mapaEstado estado))
    | barrilDeveCair pos (mapaEstado estado) = Right (calculaDanosExplosao pos 5 (mapaEstado estado))
    | otherwise = Left (Barril pos explode)

avancaObjeto estado _ (Disparo pos dir Bazuca tempo dono) 
    | bazucaDeveExplodir pos estado = Right (calculaDanosExplosao pos 5 (mapaEstado estado))
    | otherwise = 
        let novaPos = movePosicao dir pos
        in if ePosicaoMatrizValida novaPos (mapaEstado estado)
            then Left (Disparo novaPos dir Bazuca tempo dono)
            else Right []  -- Sai do mapa, removida

avancaObjeto estado _ (Disparo pos dir Mina tempo dono) = 
    avancaMina estado pos dir tempo dono

avancaObjeto estado _ (Disparo pos dir Dinamite tempo dono) = 
    avancaDinamite estado pos dir tempo dono

avancaObjeto _ _ obj = Left obj

-- ========== BARRIL ==========

-- | Verifica se barril deve cair (no ar/água)
barrilDeveCair :: Posicao -> Mapa -> Bool
barrilDeveCair pos mapa = not (ePosicaoMapaLivre pos mapa) || estaNoArOuAgua pos mapa 

-- | Verifica se posição está no ar ou em água 
estaNoArOuAgua :: Posicao -> Mapa -> Bool
estaNoArOuAgua pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just Ar -> True
        Just Agua -> True
        _ -> False

-- ========== BAZUCA ==========

-- | Verifica se bazuca deve explodir (posição não livre)
bazucaDeveExplodir :: Posicao -> Estado -> Bool
bazucaDeveExplodir pos estado = not (ePosicaoEstadoLivre pos estado)

-- ========== MINA ==========

-- | Avança estado da mina
avancaMina :: Estado -> Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Either Objeto Danos
avancaMina estado pos dir tempo donomi =
    case tempo of
        Just 0 -> Right (calculaDanosExplosao pos 3 (mapaEstado estado)) -- temp chegou a 0, explode, raio == 3
        Just n -> Left (atualizaMinaComTempo estado pos dir (Just (n-1)) donomi) -- mina ativa temp n, e move 
        Nothing -> 
            if minaDeveAtivar pos donomi estado -- se existe inimigo proximo ATIVA
                then Left (Disparo pos dir Mina (Just 2) donomi) -- inicia temp ==2s
                else Left (atualizaMinaComTempo estado pos dir Nothing donomi)

-- | Atualiza posição e direção da mina aplicando física
atualizaMinaComTempo :: Estado -> Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Objeto
atualizaMinaComTempo estado pos dir tempo dono =
    let mapa = mapaEstado estado
        novaPos = if minaDeveCair pos dir mapa
                     then movePosicao Sul pos
                     else pos
        novaDir = if minaEstaNoChao novaPos mapa then Norte else dir
    in if ePosicaoMatrizValida novaPos mapa
          then Disparo novaPos novaDir Mina tempo dono
          else Disparo pos dir Mina tempo dono  -- Mantém se nova posição inválida

-- | Verifica se mina deve cair (no ar/água com direção Sul)
minaDeveCair :: Posicao -> Direcao -> Mapa -> Bool
minaDeveCair pos dir mapa = 
    dir == Sul && estaNoArOuAgua pos mapa

-- | Verifica se mina está no chão
minaEstaNoChao :: Posicao -> Mapa -> Bool
minaEstaNoChao pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just Terra -> True
        Just Pedra -> True
        _ -> False

-- | Verifica se mina deve ativar (minhoca inimiga na área)
minaDeveAtivar :: Posicao -> NumMinhoca -> Estado -> Bool
minaDeveAtivar pos dono estado = 
    let area = calculaAreaExplosao pos 3
        minhocasInimigas = minhocasInimigasNaArea area dono (minhocasEstado estado)
    in not (null minhocasInimigas)

-- | Retorna minhocas inimigas numa área
minhocasInimigasNaArea :: [Posicao] -> NumMinhoca -> [Minhoca] -> [NumMinhoca]
minhocasInimigasNaArea area dono minhocas = 
    [i | (i, m) <- zip [0..] minhocas, 
         i /= dono, 
         case posicaoMinhoca m of
            Just p -> p `elem` area
            Nothing -> False]

-- ========== DINAMITE ==========

-- | Avança estado da dinamite
avancaDinamite :: Estado -> Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Either Objeto Danos
avancaDinamite estado pos dir tempo dono =
    case tempo of
        Nothing -> Left (Disparo pos dir Dinamite tempo dono) -- Dinamite acabou de ser lançada, dinamite na pos e dir iniciais
        Just 0 -> Right (calculaDanosExplosao pos 7 (mapaEstado estado)) -- tem chegou a 0, dá a lista de danos causados
        Just n -> 
            let mapa = mapaEstado estado -- Dinamite em movimento
                (novaPos, novaDir) = if dinamiteEstaNoChao pos mapa
                                        then (pos, Norte)
                                        else calculaMovimentoDinamite pos dir mapa
            in if ePosicaoMatrizValida novaPos mapa
                  then Left (Disparo novaPos novaDir Dinamite (Just (n-1)) dono)
                  else Right []  -- Sai do mapa

-- | Verifica se dinamite está no chão
dinamiteEstaNoChao :: Posicao -> Mapa -> Bool
dinamiteEstaNoChao pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just Terra -> True
        Just Pedra -> True
        _ -> False

-- | Calcula movimento da dinamite (parábola)
calculaMovimentoDinamite :: Posicao -> Direcao -> Mapa -> (Posicao, Direcao)
calculaMovimentoDinamite pos dir mapa = 
    case dir of
        Norte -> (movePosicao Sul pos, Sul) -- gravidade
        Sul -> (movePosicao Sul pos, Norte)
        Este -> (movePosicao Sudeste pos, Sudeste) -- diagonal
        Oeste -> (movePosicao Sudoeste pos, Sudoeste) -- diagonal
        Nordeste -> (movePosicao Este pos, Sudeste) --gravidade
        Noroeste -> (movePosicao Oeste pos, Sudoeste) -- gravidade
        Sudeste -> (movePosicao Sul pos, Norte)
        Sudoeste -> (movePosicao Sul pos, Norte)

-- ========== EXPLOSÕES ==========

-- | Calcula área de explosão (todas as posições num quadrado)
calculaAreaExplosao :: Posicao -> Int -> [Posicao]
calculaAreaExplosao (l, c) diametro = 
    let raio = diametro `div` 2
    in [(l + dl, c + dc) | dl <- [-raio..raio], dc <- [-raio..raio]] -- circulo da explosao (lista)

-- | Calcula danos de uma explosão
calculaDanosExplosao :: Posicao -> Int -> Mapa -> Danos
calculaDanosExplosao centro diametro mapa = 
    let raio = diametro `div` 2
        posicoesValidas = [(l, c) | l <- [fst centro - raio .. fst centro + raio], -- cria area quadrada
                                    c <- [snd centro - raio .. snd centro + raio],
                                    ePosicaoMatrizValida (l, c) mapa]
        danosComPos = [(pos, calculaDanoPosicao centro pos diametro) | pos <- posicoesValidas] -- para cada posicao nessa area, calcula danos e cria par (posicao,dano)
    in filter (\(_, dano) -> dano > 0) danosComPos -- remove pos que n foram afetadas

-- | Calcula dano numa posição específica baseado na distância ao centro
calculaDanoPosicao :: Posicao -> Posicao -> Int -> Dano
calculaDanoPosicao (lc, cc) (lp, cp) diametro = 
    let distL = abs (lc - lp) -- Distância em linhas (vertical)
        distC = abs (cc - cp)  -- Distância em colunas (horizontal)
        distMax = max distL distC
        eCardeal = (distL == 0 || distC == 0) && distMax > 0
        eDiagonal = distL > 0 && distC > 0  -- Classificar diracao
    in if distL == 0 && distC == 0 -- se a explosao é exatamente no centro
        then diametro * 10  -- Centro da explosão
        else if eCardeal
            then max 0 ((diametro - 2 * distMax) * 10)  -- Direções cardeais, conta com o centro
            else max 0 ((diametro - 2 * distMax - 1) * 10)  -- Direções diagonais, n causa dano no centro

-- ========== APLICAÇÃO DE DANOS ==========

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
    let mapaAtualizado = destroiPosicao pos mapa  -- Usa função de Tarefa0_2025
    in aplicaDanosMapa ds mapaAtualizado

-- | Aplica danos aos objetos (barris atingidos explodem)
aplicaDanosObjetos :: Danos -> [Objeto] -> [Objeto]
aplicaDanosObjetos danos objetos = 
    map (ativaBarrilSeDanificado danos) objetos

-- | Ativa barril para explodir se for danificado
ativaBarrilSeDanificado :: Danos -> Objeto -> Objeto
ativaBarrilSeDanificado danos (Barril pos _) 
    | barrilFoiAtingido pos danos = Barril pos True
    | otherwise = Barril pos False
ativaBarrilSeDanificado _ obj = obj

-- | Verifica se barril foi atingido pelos danos
barrilFoiAtingido :: Posicao -> Danos -> Bool
barrilFoiAtingido pos danos = any (\(p, _) -> p == pos) danos

-- | Aplica danos às minhocas
aplicaDanosMinhocas :: Danos -> [Minhoca] -> [Minhoca]
aplicaDanosMinhocas danos = map (reduzVidaMinhoca danos)

-- | Reduz vida de uma minhoca baseado nos danos
reduzVidaMinhoca :: Danos -> Minhoca -> Minhoca
reduzVidaMinhoca danos minhoca = 
    case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just pos -> 
            let danoTotal = calculaDanoTotal pos danos
            in aplicaDanoNaVida danoTotal minhoca

-- | Calcula dano total numa posição
calculaDanoTotal :: Posicao -> Danos -> Int
calculaDanoTotal pos danos = sum [dano | (p, dano) <- danos, p == pos]

-- | Aplica dano na vida da minhoca
aplicaDanoNaVida :: Int -> Minhoca -> Minhoca
aplicaDanoNaVida dano minhoca 
    | dano <= 0 = minhoca
    | otherwise = 
        case vidaMinhoca minhoca of
            Morta -> minhoca
            Viva vidaAtual -> 
                let novaVida = vidaAtual - dano
                in if novaVida <= 0
                    then minhoca { vidaMinhoca = Morta }
                    else minhoca { vidaMinhoca = Viva novaVida }
