{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025
import Tarefa0_geral

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado e = undefined

-- O mapa é válido, i.e. satisfaz os seguintes critérios:
--Não vazio;
--Denota corretamente uma grelha, i.e. tem o mesmo número de colunas em todas as linhas.
--Contém terrenos, que podem ser do tipo Ar/Agua (não opacos) ou do tipo Terra/Pedra (opacos);

validaMapa :: Mapa -> Bool
validaMapa x = eMatrizValida e && all validaPeca (concat m)
  where
     validaPeca p = case p of
      Ar    -> True
      Agua  -> True
      Terra -> True
      Pedra -> True
      _     -> False

--Cada objeto é válido, o que se verifica quando:
--Tem uma posição válida (dentro dos limites do mapa) e livre (que não corresponde a nenhum terreno opaco).
    --Uma excepção será feita para disparos de bazuca, que podem “temporariamente perfurar” terrenos opacos, 
        --mas apenas na superfície, i.e., apenas se a posição anterior dada a direção do projétil não for também um terreno opaco.
--A posição:
    --De um barril: não se encontra ocupada por um barril (que não o próprio objeto) ou por uma minhoca.
    --De uma mina ou dinamite: não se encontra ocupada por um barril.
    --De outro objeto: não requer nenhuma validação adicional.
--Se for um disparo:
    --Não pode ser um disparo de jetpack ou de escavadora, já que estas armas terão efeito instantâneo no estado do jogo e não criarão disparos.
    --O tempo do disparo tem que ser coerente com o tipo de arma:
        --Bazuca: sem tempo
        --Mina: sem tempo, ou um inteiro entre 0 e 2
        --Dinamite: um inteiro entre 0 e 4
    --O dono do disparo tem que ser um índice válido na lista de minhocas. 
    --O mesmo dono não pode ter simultaneamente mais do que um disparo de cada tipo.


--Cada minhoca é válida, o que se verifica quando:
--Tem uma posição válida e livre, ou opcionalmente nenhuma posição.
--A sua posição não se encontra ocupada por um objeto de barril ou por outra minhoca.
--Quando não tem posição ou se encontra numa posição em que o terreno é água, a minhoca tem que estar obrigatoriamente morta.
--Quando viva, a vida da minhoca é um inteiro entre 0 e 100.
--A quantidade de munições das diversas armas é um número inteiro maior ou igual a 0.

validaMinhocas :: [Minhoca] -> Bool
