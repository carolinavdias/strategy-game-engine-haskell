-- | Este módulo 
-- <https://haslab.github.io/Teaching/LI1/2526/haddocks/worms/Labs2025.html#t:TipoArma>
-- <https://haslab.github.io/Teaching/LI1/2526/haddocks/worms/src/Labs2025.html#TipoArma>
module Tarefa0_2025 where
    
import Labs2025

-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca Jetpack mi = jetpackMinhoca mi
encontraQuantidadeArmaMinhoca Escavadora mi = escavadoraMinhoca mi
encontraQuantidadeArmaMinhoca Bazuca mi = bazucaMinhoca mi
encontraQuantidadeArmaMinhoca Mina mi = minaMinhoca mi
encontraQuantidadeArmaMinhoca Dinamite mi = dinamiteMinhoca mi

-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca Jetpack mi  x = mi { jetpackMinhoca = x }
atualizaQuantidadeArmaMinhoca Escavadora mi  x = mi { escavadoraMinhoca = x}
atualizaQuantidadeArmaMinhoca Bazuca mi x = mi {bazucaMinhoca = x}
atualizaQuantidadeArmaMinhoca Mina mi x = mi { minaMinhoca =x }
atualizaQuantidadeArmaMinhoca Dinamite mi x = mi {dinamiteMinhoca = x}

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel x =  x == Terra

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco x = (x == Terra) || (x== Pedra)

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre x = not(eTerrenoOpaco x)

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado =
    let mapaLivre = case encontraPosicaoMatriz pos (mapaEstado estado) of
                        Just terreno -> not (eTerrenoOpaco terreno)
                        Nothing      -> False
        minhocaLivre = all (\m -> posicaoMinhoca m /= Just pos) (minhocasEstado estado)
        barrilLivre = all (\obj -> case obj of
                                    Barril { posicaoBarril = p } -> p /= pos _ -> True) (objetosEstado estado)
    in mapaLivre && minhocaLivre && barrilLivre

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo arma numMinhoca objs =any (\obj -> case obj of Disparo { tipoDisparo = t, donoDisparo = n } -> t == arma && n == numMinhoca
                                                _ -> False ) objs

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao (l, c) (m:ms) = if l == 0  undefined

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto = undefined
