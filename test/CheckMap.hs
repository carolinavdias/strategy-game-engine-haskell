module Main where

import Labs2025
import Tarefa4
import Tarefa2
import Tarefa1

-- Replicate map construction from SelecaoModo.hs
mapaExemplo :: Mapa
mapaExemplo = 
      [ [Pedra | _ <- [1..34]] ] ++
      [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++
      [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++
      [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

terrenoAt :: Posicao -> Mapa -> Terreno
terrenoAt (l, c) m 
    | l < 0 || l >= length m = Ar -- Out of bounds
    | c < 0 || c >= length (head m) = Ar
    | otherwise = (m !! l) !! c

main :: IO ()
main = do
    putStrLn "=== Checking Map and Bot Position ==="
    let botPosStart = (8, 26) -- Where bot spawns
    
    putStrLn $ "Map Dimensions: " ++ show (length mapaExemplo) ++ "x" ++ show (length (head mapaExemplo))
    
    let positionsToCheck = 
            [ (8, 26)   -- Start
            , (9, 26)   -- Below Start
            , (10, 26)  -- Below Below Start (Terra?)
            , (8, 25), (8, 27) -- Sides
            , (9, 25), (9, 27) -- Sides at Row 9
            ]
            
    mapM_ (\pos -> do
        let t = terrenoAt pos mapaExemplo
        putStrLn $ "Pos " ++ show pos ++ ": " ++ show t
        ) positionsToCheck

    putStrLn "\n=== Checking Move Safety from (9, 26) ==="
    let estado = Estado mapaExemplo [] [Minhoca Nothing Morta 0 0 0 0 0, Minhoca (Just (9, 26)) (Viva 100) 0 0 0 0 0]
    
    let moves = [Este, Oeste, Norte, Sul]
    mapM_ (\dir -> do
        let safe = movimentoSeguro (9, 26) dir estado
        putStrLn $ "Move " ++ show dir ++ " from (9,26): " ++ show safe
        ) moves
