module Main where

import Labs2025
import Tarefa1
import Tarefa2

-- Replicate map construction from SelecaoModo.hs
mapaExemplo :: Mapa
mapaExemplo = 
      [ [Pedra | _ <- [1..34]] ] ++
      [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++
      [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++
      [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]

main :: IO ()
main = do
    putStrLn "=== Debugging ValidaEstado ==="
    let botPosStart = (8, 26) 
    let minhoca1 = Minhoca (Just (8, 6)) (Viva 100) 2 3 5 2 3
    let minhocaBot = Minhoca (Just botPosStart) (Viva 100) 2 3 5 2 3
    let estado = Estado mapaExemplo [] [minhoca1, minhocaBot]

    putStrLn $ "Initial State Valid? " ++ show (validaEstado estado)
    
    putStrLn "Executing move: Move Oeste (from (8, 26) to (8, 25))"
    let estadoNovo = efetuaJogada 1 (Move Oeste) estado
    
    -- Check new position
    let (Minhoca (Just pos) _ _ _ _ _ _) = (minhocasEstado estadoNovo) !! 1
    putStrLn $ "New Position: " ++ show pos
    
    putStrLn "Validating New State Components:"
    putStrLn $ "  validaMapa: " ++ show (validaMapa (mapaEstado estadoNovo))
    putStrLn $ "  validaObjetos: " ++ show (validaObjetos (objetosEstado estadoNovo) (mapaEstado estadoNovo) (minhocasEstado estadoNovo))
    putStrLn $ "  validaMinhocas: " ++ show (validaMinhocas (minhocasEstado estadoNovo) (mapaEstado estadoNovo) (objetosEstado estadoNovo))
    
    putStrLn $ "Overall validaEstado: " ++ show (validaEstado estadoNovo)
