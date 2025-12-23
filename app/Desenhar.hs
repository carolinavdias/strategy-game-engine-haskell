module Desenhar where

import Graphics.Gloss
import Worms
import Labs2025


-- | Função que desenha o estado do jogo no Gloss.
desenha :: Worms -> Picture
desenha worms = Pictures
  [ desenhaMapa (mapaEstado $ estadoJogo worms)
  , desenhaObjetos (objetosEstado $ estadoJogo worms)
  , desenhaMinhocas (minhocasEstado $ estadoJogo worms) (minhocaAtual worms)
  , desenhaInterface worms
  ]

--------------------------------------------------------------------------------
-- * DESENHO DO MAPA

desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = Pictures $ concat
  [ [desenhaTerreno (l, c) terreno | (c, terreno) <- zip [0..] linha]
  | (l, linha) <- zip [0..] mapa
  ]

desenhaTerreno :: Posicao -> Terreno -> Picture
desenhaTerreno (l, c) terreno =
  Translate x y $ Color cor $ rectangleSolid tamanho tamanho
  where
    tamanho = 30
    x = fromIntegral c * tamanho - 450  -- centralizar
    y = 300 - fromIntegral l * tamanho  -- inverter Y
    cor = case terreno of
      Ar -> makeColorI 135 206 235 50    -- azul claro transparente
      Terra -> makeColorI 139 69 19 255  -- castanho
      Pedra -> makeColorI 128 128 128 255 -- cinzento
      Agua -> makeColorI 0 191 255 255    -- azul água

--------------------------------------------------------------------------------
-- * DESENHO DE OBJETOS

desenhaObjetos :: [Objeto] -> Picture
desenhaObjetos = Pictures . map desenhaObjeto

desenhaObjeto :: Objeto -> Picture
desenhaObjeto (Barril pos explode) =
  let (l, c) = pos
      tamanho = 30
      x = fromIntegral c * tamanho - 450
      y = 300 - fromIntegral l * tamanho
      cor = if explode then red else makeColorI 139 90 43 255
  in Translate x y $ Pictures
      [ Color cor $ circleSolid 12
      , Color black $ Circle 12
      ]

desenhaObjeto (Disparo pos dir tipo tempo _) =
  let (l, c) = pos
      tamanho = 30
      x = fromIntegral c * tamanho - 450
      y = 300 - fromIntegral l * tamanho
      (cor, forma) = case tipo of
        Bazuca -> (red, circleSolid 8)
        Mina -> (makeColorI 50 50 50 255, rectangleSolid 15 15)
        Dinamite -> (red, rectangleSolid 10 20)
        _ -> (white, circleSolid 5)
      textoTempo = case tempo of
        Just t -> Translate (-5) (-5) $ Scale 0.1 0.1 $ Color white $ Text (show t)
        Nothing -> Blank
  in Translate x y $ Pictures [Color cor forma, textoTempo]

--------------------------------------------------------------------------------
-- * DESENHO DE MINHOCAS

desenhaMinhocas :: [Minhoca] -> NumMinhoca -> Picture
desenhaMinhocas minhocas atual = Pictures $ zipWith (desenhaMinhoca atual) [0..] minhocas

desenhaMinhoca :: NumMinhoca -> NumMinhoca -> Minhoca -> Picture
desenhaMinhoca atual num minhoca = case posicaoMinhoca minhoca of
  Nothing -> Blank
  Just (l, c) ->
    let tamanho = 30
        x = fromIntegral c * tamanho - 450
        y = 300 - fromIntegral l * tamanho
        cor = if num == atual 
              then yellow 
              else if num `mod` 2 == 0 then green else blue
        vida = case vidaMinhoca minhoca of
          Viva v -> v
          Morta -> 0
        corVida = if vida > 50 then green else if vida > 20 then orange else red
    in Translate x y $ Pictures
        [ Color cor $ circleSolid 12  -- corpo
        , Color black $ Circle 12      -- contorno
        , Translate 0 18 $ Color black $ rectangleSolid 20 3  -- barra fundo
        , Translate 0 18 $ Color corVida $ rectangleSolid (fromIntegral vida * 0.2) 3  -- vida
        , Translate (-8) 20 $ Scale 0.08 0.08 $ Color white $ Text (show num)  -- número
        ]

--------------------------------------------------------------------------------
-- * INTERFACE

desenhaInterface :: Worms -> Picture
desenhaInterface worms = Pictures
  [ desenhaMensagem (mensagem worms)
  , desenhaInfoMinhoca worms
  , desenhaControlos
  ]

desenhaMensagem :: String -> Picture
desenhaMensagem msg = 
  Translate (-450) 350 $ Scale 0.15 0.15 $ Color white $ Text msg

desenhaInfoMinhoca :: Worms -> Picture
desenhaInfoMinhoca worms = 
  let estado = estadoJogo worms
      num = minhocaAtual worms
      minhocas = minhocasEstado estado
  in if num >= 0 && num < length minhocas
     then let m = minhocas !! num
              vida = case vidaMinhoca m of
                Viva v -> show v
                Morta -> "MORTA"
              info = [ "=== MINHOCA " ++ show num ++ " ==="
                     , "Vida: " ++ vida
                     , "Jetpack: " ++ show (jetpackMinhoca m)
                     , "Escavadora: " ++ show (escavadoraMinhoca m)
                     , "Bazuca: " ++ show (bazucaMinhoca m)
                     , "Mina: " ++ show (minaMinhoca m)
                     , "Dinamite: " ++ show (dinamiteMinhoca m)
                     ]
          in Pictures $ zipWith desenhaLinha [0..] info
     else Blank
  where
    desenhaLinha :: Int -> String -> Picture
    desenhaLinha i txt = 
      Translate 400 (300 - fromIntegral i * 20) $ 
      Scale 0.1 0.1 $ Color white $ Text txt

desenhaControlos :: Picture
desenhaControlos = Pictures $ zipWith desenhaLinha [0..] controles
  where
    controles = 
      [ "=== CONTROLOS ==="
      , "WASD: Mover"
      , "Q/E: Diagonais"
      , "1: Jetpack"
      , "2: Escavadora"
      , "3: Bazuca"
      , "4: Mina"
      , "5: Dinamite"
      , "SPACE: Proximo turno"
      , "R: Reiniciar"
      ]
    desenhaLinha i txt = 
      Translate (-950) (300 - fromIntegral i * 20) $ 
      Scale 0.1 0.1 $ Color white $ Text txt