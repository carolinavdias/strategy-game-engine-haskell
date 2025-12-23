module Tempo where

import Worms
import Tarefa3

-- | Tempo em segundos.
type Segundos = Float

-- | Frequência de ticks da física (10 ticks por segundo)
tickRate :: Float
tickRate = 0.1

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> Worms
reageTempo dt worms = case modoJogo worms of
  Esperando -> 
    -- No modo de espera, aplicar física constantemente
    aplicaFisica dt worms
  
  Animando tempo -> 
    if tempo <= 0
      then worms { modoJogo = Esperando }
      else (aplicaFisica dt worms) { modoJogo = Animando (tempo - dt) }
  
  GameOver -> worms

--------------------------------------------------------------------------------
-- * FÍSICA DO JOGO

-- | Aplica física do jogo (gravidade, explosões, etc)
aplicaFisica :: Segundos -> Worms -> Worms
aplicaFisica dt worms = 
  let acumulado = acumulaTempo dt worms
  in if acumulado >= tickRate
     then executaTick (acumulado - tickRate) worms
     else worms

-- | Acumula tempo para o próximo tick (usando a mensagem como buffer temporário)
-- Nota: Isto é um hack - numa implementação real, adicionarias um campo ao Worms
acumulaTempo :: Segundos -> Worms -> Segundos
acumulaTempo dt _ = dt  -- Simplificado - avança sempre imediatamente

-- | Executa um tick de física do jogo
executaTick :: Segundos -> Worms -> Worms
executaTick _ worms = 
  let estadoAtual = estadoJogo worms
      novoEstado = avancaEstado estadoAtual
      novaMsg = verificaMudancas estadoAtual novoEstado (mensagem worms)
  in worms 
      { estadoJogo = novoEstado
      , mensagem = novaMsg
      }

--------------------------------------------------------------------------------
-- * VERIFICAÇÃO DE EVENTOS

verificaMudancas :: Estado -> Estado -> String -> String
verificaMudancas estadoAntigo estadoNovo msgAtual =
  let minhocasAntigas = minhocasEstado estadoAntigo
      minhocasNovas = minhocasEstado estadoNovo
      objetosAntigos = objetosEstado estadoAntigo
      objetosNovos = objetosEstado estadoNovo
      
      mortesNovas = contaMortes minhocasAntigas minhocasNovas
      explosoesNovas = length objetosAntigos - length objetosNovos
  in if mortesNovas > 0
     then "MINHOCA MORREU! " ++ msgAtual
     else if explosoesNovas > 0
     then "EXPLOSAO! " ++ msgAtual
     else msgAtual

contaMortes :: [Minhoca] -> [Minhoca] -> Int
contaMortes antigas novas = 
  length [() | (m1, m2) <- zip antigas novas, 
               estaViva m1 && not (estaViva m2)]
  where
    estaViva m = case vidaMinhoca m of
      Viva _ -> True
      Morta -> False