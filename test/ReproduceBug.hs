module Main where

import Labs2025
import Tarefa4
import Tarefa2
import Tarefa1

main :: IO ()
main = do

    putStrLn "=== Testing jogadaTatica logic ==="
    let mapa = [[Ar, Terra, Ar]] 
    -- Bot at (0,1) (Terra). Left is Ar, Right is Ar.
    -- Theoretically can move into Ar.
    let minhocaBot = Minhoca (Just (0,1)) (Viva 100) 0 0 0 0 0
    let estado = Estado mapa [] [Minhoca Nothing Morta 0 0 0 0 0, minhocaBot] -- Index 1 is Bot
    
    putStrLn "Calling jogadaTatica..."
    let (num, jogada) = jogadaTatica 0 estado
    putStrLn $ "Selected Move: " ++ show jogada
