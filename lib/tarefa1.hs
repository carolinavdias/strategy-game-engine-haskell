{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}
{-|
Module      : Labs2025
Description : Tipos de dados do jogo.

Módulo com os tipos de dados que vão ser utilizados para modelar as tarefas do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | 
module Labs2025
    ( module Labs2025
    , module Tarefa0_geral
    ) where

import Tarefa0_geral

-- | Um tipo de terreno do mapa.
data Terreno
    -- | Terreno vazio.
    = Ar
    -- | Terreno que afoga minhocas.
    | Agua
    -- | Terreno opaco e destrutivel.
    | Terra
    -- | Terreno opaco e indestrutivel.
    | Pedra
    deriving (Eq,Ord,Show,Read,Enum)

-- | O mapa do jogo é uma matriz de terrenos.
type Mapa = Matriz Terreno

-- | Os diversos tipos de arma disponíveis para uma minhoca.
data TipoArma = Jetpack | Escavadora | Bazuca | Mina | Dinamite
    deriving (Eq,Ord,Show,Read,Enum)

-- | O estado de saúde de uma minhoca.
data VidaMinhoca
    -- | Está viva com um número inteiros de pontos de vida.
    = Viva Int
    -- | Está morta.
    | Morta
    deriving (Eq,Ord,Show,Read)

-- | O estado completo de uma minhoca.
data Minhoca = Minhoca
    -- | Uma posição no mapa. Opcional porque a minhoca pode ter saído do mapa.
    { posicaoMinhoca :: Maybe Posicao
    -- | O estado de saúde da minhoca.
    , vidaMinhoca :: VidaMinhoca
    -- | Munições de @Jetpack@.
    , jetpackMinhoca :: Int
    -- | Munições de @Escavadora@.
    , escavadoraMinhoca :: Int
    -- | Munições de @Bazuca@.
    , bazucaMinhoca :: Int
    -- | Munições de @Mina@.
    , minaMinhoca :: Int
    -- | Munições de @Dinamite@.
    , dinamiteMinhoca :: Int
    }
    deriving (Eq,Ord,Show,Read)

-- | Um tick é a unidade de tempo do jogo.
type Ticks = Int

-- | O índice de uma minhoca na lista de minhocas.
type NumMinhoca = Int

-- | O índice de um objeto na lista de objetos.
type NumObjeto = Int

-- | Um objeto colocado no mapa.
data Objeto
    -- | Um disparo de uma arma.
    = Disparo
        -- | A posição do disparo no mapa.
        { posicaoDisparo :: Posicao
        -- | A direção do disparo.
        , direcaoDisparo :: Direcao
        -- | O tipo de arma do disparo.
        , tipoDisparo :: TipoArma
        -- | O tempo até o disparo explodir. Opcional porque nem todos os disparos de todas as armas têm um tempo pré-definido para explodir.
        , tempoDisparo :: Maybe Ticks
        -- | A minhoca que efetuou o disparo.
        , donoDisparo :: NumMinhoca
        }
    -- | Um barril de pólvora.
    | Barril
        -- | A posição do barril no mapa.
        { posicaoBarril :: Posicao
        -- | Se o barril está prestes a explodir ou não.
        , explodeBarril :: Bool
        }
    deriving (Eq,Ord,Show,Read)

-- | Estado do jogo.
data Estado = Estado
    -- | O mapa atual.
    { mapaEstado :: Mapa
    -- | Uma lista com os objetos presentes no mapa. Para as funções que vai desenvolver, deve considerar que a ordem dos elementos é irrelevante.
    , objetosEstado :: [Objeto]
    -- | Uma lista com as minhocas no jogo. A ordem dos elementos é relevante, no sentido cada minhoca vai ser identificada pelo seu índice na lista.
    , minhocasEstado :: [Minhoca]
    }
    deriving (Eq,Ord,Show,Read)

-- | Uma jogada que uma minhoca pode efetuar.
data Jogada
    -- | Disparar uma arma numa dada direção.
    = Dispara TipoArma Direcao
    -- | Mover-se numa dada direção.
    | Move Direcao
    deriving (Eq,Ord,Show,Read)


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
destroiPosicao (l, c) mapa =
    [ if i == l
        then [ if j == c && terreno == Terra then Ar else terreno
             | (j, terreno) <- zip [0..] linha ]
        else linha
    | (i, linha) <- zip [0..] mapa ]

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto = undefined