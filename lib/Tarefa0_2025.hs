{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | Este módulo 
module Tarefa0_2025 where
    
import Labs2025

-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca Jetpack minhoca = jetpackMinhoca minhoca
encontraQuantidadeArmaMinhoca Escavadora minhoca = escavadoraMinhoca minhoca
encontraQuantidadeArmaMinhoca Bazuca minhoca = bazucaMinhoca minhoca
encontraQuantidadeArmaMinhoca Mina minhoca = minaMinhoca minhoca
encontraQuantidadeArmaMinhoca Dinamite minhoca = dinamiteMinhoca minhoca

-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca Jetpack minhoca n = minhoca { jetpackMinhoca = n }
atualizaQuantidadeArmaMinhoca Escavadora minhoca n = minhoca { escavadoraMinhoca = n }
atualizaQuantidadeArmaMinhoca Bazuca minhoca n = minhoca { bazucaMinhoca = n } 
atualizaQuantidadeArmaMinhoca Mina minhoca n = minhoca { minaMinhoca = n }
atualizaQuantidadeArmaMinhoca Dinamite minhoca n = minhoca {dinamiteMinhoca = n }

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel Terra = True
eTerrenoDestrutivel _ = False

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco Terra = True
eTerrenoOpaco Pedra = True
eTerrenoOpaco _ = False

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> not (eTerrenoOpaco terreno)
        Nothing -> False  -- se a pos não existe no mapa, não está livre!

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado = 
    ePosicaoMapaLivre pos (mapaEstado estado) &&
    not (posicaoTemMinhoca pos (minhocasEstado estado)) &&
    not (posicaoTemBarril pos (objetosEstado estado))
  where
    -- verificar se há uma minhoca na posição:posicaoTemMinhoca
    posicaoTemMinhoca :: Posicao -> [Minhoca] -> Bool
    posicaoTemMinhoca _ [] = False
    posicaoTemMinhoca p (m:ms) = 
        case posicaoMinhoca m of
            Just posMinhoca -> p == posMinhoca || posicaoTemMinhoca p ms
            Nothing -> posicaoTemMinhoca p ms
    
    -- verifica se há um barril na posição: posicaoTemBarril
    posicaoTemBarril :: Posicao -> [Objeto] -> Bool
    posicaoTemBarril _ [] = False
    posicaoTemBarril p (obj:objs) = 
        case obj of
            Barril posBarril _ -> p == posBarril || posicaoTemBarril p objs
            _ -> posicaoTemBarril p objs

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo _ _ [] = False
minhocaTemDisparo arma numMinhoca (obj:objs) =
    case obj of
        Disparo _ _ tipoDisp _ dono -> 
            (tipoDisp == arma && dono == numMinhoca) || minhocaTemDisparo arma numMinhoca objs
        _ -> minhocaTemDisparo arma numMinhoca objs

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao pos mapa = 
    case encontraPosicaoMatriz pos mapa of
        Just terreno -> 
            if eTerrenoDestrutivel terreno
                then atualizaPosicaoMatriz pos Ar mapa
                else mapa
        Nothing -> mapa  -- se a posição não existe, retorna o mapa não alterado

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj estado = estado { objetosEstado = obj : objetosEstado estado }
