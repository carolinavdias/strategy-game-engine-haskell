{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}

module Tarefa0_geral where

-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- a introdução ao <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]

-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/posicaomatriz.png>>
type Posicao = (Int,Int) -- (linha , coluna)

-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)

-- | Uma direção é dada pela rosa dos ventos. Ou seja, os 4 pontos cardeais e os 4 pontos colaterais.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rosadosventos.jpg>>
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

-- * Funções não-recursivas.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i>=0 && i < length l

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz m = (length m ,length (head m))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (l, c) m = l >= 0 && l < length m && c>= 0 && c < length (head m)

-- | Mover uma posição uma unidade no sentido de uma direção.
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao Norte (l, c) = (l-1, c)
movePosicao Sul (l, c) = (l+1,c)
movePosicao Este (l, c) = (l, c+1)
movePosicao Oeste (l, c) = (l, c-1)
movePosicao Nordeste (l, c) = (l-1, c+1)
movePosicao Sudeste (l, c) = (l+1, c+1)
movePosicao Noroeste (l, c) = (l-1, c-1)
movePosicao Sudoeste (l, c) = (l +1, c-1)

-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.
--
-- __NB:__ Considere uma janela retangular com origem no canto superior esquerdo definida como uma matriz. A função recebe a dimensao da janela.
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (maxL, maxC) dir pos = 
    let novaPosicao = movePosicao dir pos
        (l, c) = novaPosicao
    in if l >= 0 && l < maxL && c >= 0 && c<maxC
        then  novaPosicao
        else pos

-- n sei se é menor e igua ou só menor e n sei o que por no else

-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.
--
-- __NB:__ Considere posições válidas. Efetue arredondamentos como achar necessário. 

origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (lt,ct) (l, c) = (l - div lt 2, c-div ct 2)


-- | Roda um par (posição,direção) 25% para a direita.
--
-- __NB:__ Vendo um par (posição,direção) como um vector, avança da posição de origem 
-- para a posição de destino do vetor, e cria um novo vetor a partir da 
-- posição de destino com a próxima direção na rosa dos ventos para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rodaposicaodirecao.png>>

rodaPosicaoDirecao :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao (pos, Noroeste) = (movePosicao Noroeste pos, Norte)
rodaPosicaoDirecao (pos, dir) = (movePosicao dir pos, succ dir)

-- * Funções recursivas.

-- | Devolve o elemento num dado índce de uma lista.
--
-- __NB:__ Retorna @Nothing@ se o índice não existir.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista _ [] = Nothing
encontraIndiceLista 0 (x:_) = Just x
encontraIndiceLista a (_:xs) = encontraIndiceLista (a-1) xs

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 novo (_:xs) = novo:xs
atualizaIndiceLista i novo (x:xs) = x : atualizaIndiceLista (i-1) novo xs

-- | Devolve o elemento numa dada posição de uma matriz.
--
-- __NB:__ Retorna @Nothing@ se a posição não existir.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (l, c) m 
    | l < 0 || c < 0 = Nothing
    | l >= length m = Nothing
    | c >= length (head m) = Nothing
    | otherwise = Just ((m !! l) !! c)



-- | Modifica um elemento numa dada posição de uma matriz.
--
-- __NB:__ Devolve a própria matriz se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l, c) n x 
    | l < 0 || c < 0 = x
    | l >= length x = x
    | c >= length (head x) = x
    |otherwise = atualizaIndiceLista l (atualizaIndiceLista c n (x !! l)) x

-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] x = x
moveDirecoesPosicao (dir:dirs) (l, c) = moveDirecoesPosicao dirs (movePosicao dir (l,c))

-- | Aplica a mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []
moveDirecaoPosicoes dir (x:xs) = movePosicao dir x : moveDirecaoPosicoes dir xs

-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo.
--
-- __NB:__ Todas as linhas devem ter o mesmo número de colunas. 
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = True
eMatrizValida m = all (\linha -> length linha == length (head m)) m
