{-|
Module      : Desenhar  
Description : Renderização visual do jogo
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
-}

module Desenhar 
  ( desenha
  , desenharMenu
  , desenharSelecao
  , desenharGameOver
  , desenharVictory
  , desenharTutorial
  ) where

import Graphics.Gloss
import EstadoJogo
import Assets
import Labs2025
import Menu (desenharMenu)
import SelecaoModo (desenharSelecao)

-- Função principal de renderização do jogo
desenha :: Assets -> EstadoPartida -> Picture
desenha assets partida = Pictures
  [ desenharBackground assets
  , desenharMapa (mapaEstado $ estadoWorms partida)
  , desenharObjetos assets (objetosEstado $ estadoWorms partida)
  , desenharMinhocasJogo assets 
      (minhocasEstado $ estadoWorms partida) 
      (frameCounter partida) 
      (jogadorAtual partida) 
      (armaSelecionadaP1 partida) 
      (armaSelecionadaP2 partida)
  , desenharAnimacoes assets (animacoes partida)
  , desenharMolduraPedra assets
  , desenharMinhocasLaterais assets
  , desenharHUDCompleto assets partida
  , desenharMenusArmas assets partida
  , desenharTextosControlos assets
  , desenharPausaSeNecessario (pausado partida)
  ]

-- Moldura decorativa de pedra
desenharMolduraPedra :: Assets -> Picture
desenharMolduraPedra assets =
  case stoneFrame (frameAssets assets) of
    Just frame -> Scale 0.83 0.83 frame
    Nothing -> Blank

-- Interface de jogo completa
desenharHUDCompleto :: Assets -> EstadoPartida -> Picture
desenharHUDCompleto assets partida = Pictures
  [ desenharInfoJogador1 assets partida
  , desenharInfoJogador2 assets partida
  , desenharTimer assets partida
  ]

desenharInfoJogador1 :: Assets -> EstadoPartida -> Picture
desenharInfoJogador1 assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVerdes = [(i, m) | (i, m) <- zip [0..] minhocas, even i, minhocaViva m]
  in case minhocasVerdes of
       ((_, minhoca):_) -> Translate (-790) 520 $ Pictures
         [ desenharTextoJogador assets True
         , Translate (-50) (-70) $ desenharBarraVida assets (vidaMinhoca minhoca)
         ]
       [] -> Blank

desenharInfoJogador2 :: Assets -> EstadoPartida -> Picture
desenharInfoJogador2 assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasAzuis = [(i, m) | (i, m) <- zip [0..] minhocas, odd i, minhocaViva m]
  in case minhocasAzuis of
       ((_, minhoca):_) -> Translate 780 520 $ Pictures
         [ desenharTextoJogador assets False
         , Translate (60) (-70) $ desenharBarraVida assets (vidaMinhoca minhoca)
         ]
       [] -> Blank

desenharTextoJogador :: Assets -> Bool -> Picture
desenharTextoJogador assets isJogador1 =
  let textoImg = if isJogador1 
                 then textPlayer1 (uiAssets assets)
                 else textPlayer2 (uiAssets assets)
  in case textoImg of
       Just img -> Scale 1.4 1.4 $ img
       Nothing -> Color white $ Scale 0.15 0.15 $ 
                  Text (if isJogador1 then "JOGADOR 1" else "JOGADOR 2")

desenharBarraVida :: Assets -> VidaMinhoca -> Picture
desenharBarraVida assets Morta = escolherBarraHP assets 0
desenharBarraVida assets (Viva hp) = escolherBarraHP assets hp

escolherBarraHP :: Assets -> Int -> Picture
escolherBarraHP assets hp
  | hp >= 100 = getBarraOr assets (hpBar100 $ uiAssets assets) 100
  | hp >= 80  = getBarraOr assets (hpBar80 $ uiAssets assets) 80
  | hp >= 60  = getBarraOr assets (hpBar60 $ uiAssets assets) 60
  | hp >= 40  = getBarraOr assets (hpBar40 $ uiAssets assets) 40
  | hp >= 20  = getBarraOr assets (hpBar20 $ uiAssets assets) 20
  | otherwise = getBarraOr assets (hpBar0 $ uiAssets assets) 0

getBarraOr :: Assets -> Maybe Picture -> Int -> Picture
getBarraOr _ (Just img) _ = Scale 1.5 1.5 $ img
getBarraOr _ Nothing hp = Pictures
  [ Color (greyN 0.2) $ rectangleSolid 140 30
  , Color (corPorHP hp) $ rectangleSolid (fromIntegral hp * 1.3) 25
  ]

corPorHP :: Int -> Color
corPorHP hp
  | hp > 70 = makeColorI 0 255 0 255
  | hp > 30 = makeColorI 255 255 0 255
  | otherwise = makeColorI 255 0 0 255

-- Timer com texto simulando negrito
desenharTimer :: Assets -> EstadoPartida -> Picture
desenharTimer assets partida =
  let isVerde = jogadorAtual partida == 0
      timerImg = if isVerde 
                 then timerGreen (uiAssets assets)
                 else timerBlue (uiAssets assets)
      segundos = round (tempoRestante partida) :: Int
  in Translate 10 530 $ Pictures
     [ case timerImg of
         Just img -> Scale 1.1 1.1 $ img
         Nothing -> Color (if isVerde then green else cyan) $ circleSolid 60
     , Translate (-23) (-15) $ desenharNumeroTimer segundos (tempoRestante partida)
     ]

-- Simula efeito negrito desenhando texto múltiplas vezes
desenharNumeroTimer :: Int -> Float -> Picture
desenharNumeroTimer segundos tempoTotal =
  let cor = if segundos < 10 && even (floor tempoTotal :: Int)
            then red else black
      tamanho = 0.4
      texto = show segundos
  in Pictures
       [ Color cor $ Translate 0 0 $ Scale tamanho tamanho $ Text texto
       , Color cor $ Translate 2 0 $ Scale tamanho tamanho $ Text texto
       , Color cor $ Translate 0 2 $ Scale tamanho tamanho $ Text texto
       , Color cor $ Translate 2 2 $ Scale tamanho tamanho $ Text texto
       ]

-- Minhocas decorativas laterais
desenharMinhocasLaterais :: Assets -> Picture
desenharMinhocasLaterais assets = Pictures
  [ desenharMinhocaLateralVerde assets
  , desenharMinhocaLateralAzul assets
  ]

desenharMinhocaLateralVerde :: Assets -> Picture
desenharMinhocaLateralVerde assets =
  case wormGreenBig (spriteAssets assets) of
    Just img -> Translate (-830) 200 $ Scale 2.4 2.4 $ img
    Nothing -> Blank

desenharMinhocaLateralAzul :: Assets -> Picture
desenharMinhocaLateralAzul assets =
  case wormBlueBig (spriteAssets assets) of
    Just img -> Translate 820 200 $ Scale 2.5 2.5 $ img
    Nothing -> Blank

-- Sistema de menus de armas
desenharMenusArmas :: Assets -> EstadoPartida -> Picture
desenharMenusArmas assets partida = Pictures
  [ desenharLadoEsquerdo assets partida
  , desenharLadoDireito assets partida
  ]

desenharLadoEsquerdo :: Assets -> EstadoPartida -> Picture
desenharLadoEsquerdo assets partida =
  if menuArmasAbertoP1 partida
  then desenharMenuArmasEsquerda assets partida
  else desenharBotaoArmasVerde assets

desenharLadoDireito :: Assets -> EstadoPartida -> Picture
desenharLadoDireito assets partida =
  if menuArmasAbertoP2 partida
  then desenharMenuArmasDireita assets partida
  else desenharBotaoArmasAzul assets

desenharBotaoArmasVerde :: Assets -> Picture
desenharBotaoArmasVerde assets =
  case buttonWeaponsGreen (uiAssets assets) of
    Just img -> Translate (-860) (-90) $ Scale 1.3 1.3 $ img
    Nothing -> Translate (-860) (-90) $ Pictures
      [ Color green $ rectangleSolid 120 120
      , Color white $ Scale 0.15 0.15 $ Text "Q"
      ]

desenharBotaoArmasAzul :: Assets -> Picture
desenharBotaoArmasAzul assets =
  case buttonWeaponsBlue (uiAssets assets) of
    Just img -> Translate 860 (-90) $ Scale 1.3 1.3 $ img
    Nothing -> Translate 860 (-90) $ Pictures
      [ Color cyan $ rectangleSolid 120 120
      , Color white $ Scale 0.15 0.15 $ Text "Q"
      ]

desenharMenuArmasEsquerda :: Assets -> EstadoPartida -> Picture
desenharMenuArmasEsquerda assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVerdes = [(i, m) | (i, m) <- zip [0..] minhocas, even i, minhocaViva m]
  in case minhocasVerdes of
       ((_, minhoca):_) -> Translate (-820) (-150) $
         desenharListaArmas assets minhoca (armaSelecionadaP1 partida) True
       [] -> Blank

desenharMenuArmasDireita :: Assets -> EstadoPartida -> Picture
desenharMenuArmasDireita assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasAzuis = [(i, m) | (i, m) <- zip [0..] minhocas, odd i, minhocaViva m]
  in case minhocasAzuis of
       ((_, minhoca):_) -> Translate 820 (-150) $
         desenharListaArmas assets minhoca (armaSelecionadaP2 partida) False
       [] -> Blank

-- Lista de armas com teclas numeradas
desenharListaArmas :: Assets -> Minhoca -> Maybe TipoArma -> Bool -> Picture 
desenharListaArmas assets minhoca armaSel isJogador1 = Pictures
  [ desenharArmaComNumero assets Bazuca (bazucaMinhoca minhoca) (armaSel == Just Bazuca) 0 150 (if isJogador1 then "1" else "6")
  , desenharArmaComNumero assets Dinamite (dinamiteMinhoca minhoca) (armaSel == Just Dinamite) 0 50 (if isJogador1 then "2" else "7")
  , desenharArmaComNumero assets Mina (minaMinhoca minhoca) (armaSel == Just Mina) 0 (-50) (if isJogador1 then "3" else "8")
  , desenharArmaComNumero assets Escavadora (escavadoraMinhoca minhoca) (armaSel == Just Escavadora) 0 (-150) (if isJogador1 then "4" else "9")
  , desenharArmaComNumero assets Jetpack (jetpackMinhoca minhoca) (armaSel == Just Jetpack) 0 (-250) (if isJogador1 then "5" else "0")
  ]

desenharArmaComNumero :: Assets -> TipoArma -> Int -> Bool -> Float -> Float -> String -> Picture
desenharArmaComNumero assets arma qtd selecionado x y numero = Translate x y $ Pictures
  [ brilho
  , icone
  , textoNumero
  , textoQtd
  ]
  where
    escala = if selecionado then 1.5 else 1.0
    temMunicao = qtd > 0
    
    brilho = if selecionado
             then Color (makeColorI 0 212 255 120) $ circleSolid 45
             else Blank
    
    icone = Scale escala escala $ 
            Color (if temMunicao then white else greyN 0.4) $
            getIconeArma assets arma
    
    textoNumero = Translate (-50) 0 $ Pictures
      [ Color black $ Translate 0 0 $ Scale 0.3 0.3 $ Text numero
      , Color black $ Translate 1 0 $ Scale 0.3 0.3 $ Text numero
      , Color black $ Translate 0 1 $ Scale 0.3 0.3 $ Text numero
      , Color black $ Translate 1 1 $ Scale 0.3 0.3 $ Text numero
      ]
    
    textoQtd = Translate 0 (-40) $ Pictures
      [ Color (if temMunicao then white else greyN 0.5) $ 
        Translate 0 0 $ Scale 0.25 0.25 $ Text ("x" ++ show qtd)
      , Color (if temMunicao then white else greyN 0.5) $ 
        Translate 1 0 $ Scale 0.25 0.25 $ Text ("x" ++ show qtd)
      , Color (if temMunicao then white else greyN 0.5) $ 
        Translate 0 1 $ Scale 0.25 0.25 $ Text ("x" ++ show qtd)
      , Color (if temMunicao then white else greyN 0.5) $ 
        Translate 1 1 $ Scale 0.25 0.25 $ Text ("x" ++ show qtd)
      ]

getIconeArma :: Assets -> TipoArma -> Picture
getIconeArma assets Bazuca = getPicOr (bazucaIcon $ objetoAssets assets) (Color yellow $ rectangleSolid 20 8)
getIconeArma assets Dinamite = getPicOr (dinamiteIcon $ objetoAssets assets) (Color red $ rectangleSolid 8 22)
getIconeArma assets Mina = getPicOr (minaIcon $ objetoAssets assets) (Color black $ circleSolid 12)
getIconeArma assets Escavadora = getPicOr (escavadoraIcon $ objetoAssets assets) (Color yellow $ rectangleSolid 22 15)
getIconeArma assets Jetpack = getPicOr (jetpackIcon $ objetoAssets assets) (Color cyan $ rectangleSolid 15 22)

getPicOr :: Maybe Picture -> Picture -> Picture
getPicOr (Just p) _ = p
getPicOr Nothing fallback = fallback

-- Texto de controlos no rodapé
desenharTextosControlos :: Assets -> Picture
desenharTextosControlos assets =
  case textControls (uiAssets assets) of
    Just img -> Translate 0 (-520) $ Scale 1 1 $ img
    Nothing -> Translate 0 (-520) $ 
               Color white $ Scale 0.2 0.2 $
               Text "P-PAUSA | X-VOLTAR | ESPACO-PASSAR"

-- Background em modo fullscreen
desenharBackground :: Assets -> Picture
desenharBackground assets =
  case gameBackground (backgroundAssets assets) of
    Just bg -> bg
    Nothing -> Color (makeColorI 135 206 235 255) $ rectangleSolid 1920 1200

-- Renderização do mapa do jogo
tamanhoBloco :: Float
tamanhoBloco = 32.0

desenharMapa :: Mapa -> Picture
desenharMapa mapa = 
  let numColunas = if null mapa then 0 else length (head mapa)
      numLinhas = length mapa
      larguraMapa = fromIntegral numColunas * tamanhoBloco
      alturaMapa = fromIntegral numLinhas * tamanhoBloco
      
      offsetX = -larguraMapa / 2 + 10
      offsetY = alturaMapa / 2 - 20
  
  in Scale 1.3 1.3 $    
    Translate offsetX offsetY $ Pictures $ concat
    [ [ desenharBloco terreno (l, c)
      | (c, terreno) <- zip [0..] linha
      ]
    | (l, linha) <- zip [0..] mapa
    ]

desenharBloco :: Terreno -> Posicao -> Picture
desenharBloco Ar _ = Blank
desenharBloco terreno (l, c) = 
  Translate x y $ Pictures [corFundo terreno, textura terreno]
  where
    x = fromIntegral c * tamanhoBloco
    y = -fromIntegral l * tamanhoBloco
    
    corFundo Agua = Color (makeColorI 30 144 255 180) $ rectangleSolid tamanhoBloco tamanhoBloco
    corFundo Terra = Color (makeColorI 139 69 19 255) $ rectangleSolid tamanhoBloco tamanhoBloco
    corFundo Pedra = Color (makeColorI 105 105 105 255) $ rectangleSolid tamanhoBloco tamanhoBloco
    corFundo _ = Blank
    
    textura Agua = Color (makeColorI 255 255 255 40) $ 
                   Translate 0 (sin (fromIntegral c * 0.5) * 2) $ 
                   rectangleSolid (tamanhoBloco - 4) 2
    textura Terra = Pictures
      [ Color (makeColorI 101 51 10 100) $ Translate dx dy $ circleSolid 2
      | dx <- [-8, 0, 8], dy <- [-8, 0, 8]
      ]
    textura Pedra = Color (makeColorI 80 80 80 150) $ 
                    rectangleSolid (tamanhoBloco - 6) (tamanhoBloco - 6)
    textura _ = Blank

-- Renderização de minhocas no jogo
desenharMinhocasJogo :: Assets -> [Minhoca] -> Int -> Int -> Maybe TipoArma -> Maybe TipoArma -> Picture
desenharMinhocasJogo assets minhocas frameCount jogadorAtual armaP1 armaP2 = Pictures
  [ desenharMinhocaJogo assets i minhoca (getFrameParaMinhoca i) (getArmaParaMinhoca i)
  | (i, minhoca) <- zip [0..] minhocas
  , posicaoMinhoca minhoca /= Nothing
  ]
  where
    getFrameParaMinhoca numMinhoca
      | even numMinhoca && jogadorAtual == 0 = frameCount
      | odd numMinhoca && jogadorAtual == 1 = frameCount
      | otherwise = 0
    
    getArmaParaMinhoca numMinhoca
      | even numMinhoca && jogadorAtual == 0 = armaP1
      | odd numMinhoca && jogadorAtual == 1 = armaP2
      | otherwise = Nothing

desenharMinhocaJogo :: Assets -> Int -> Minhoca -> Int -> Maybe TipoArma -> Picture
desenharMinhocaJogo assets numMinhoca minhoca frameCount armaEquipada =
  case posicaoMinhoca minhoca of
    Nothing -> Blank
    Just (l, c) ->
      let numColunas = 34
          numLinhas = 20
          larguraMapa = fromIntegral numColunas * tamanhoBloco
          alturaMapa = fromIntegral numLinhas * tamanhoBloco
          offsetXMapa = -larguraMapa / 2
          offsetYMapa = alturaMapa / 2 - 60
          
          x = fromIntegral c * tamanhoBloco
          y = -fromIntegral l * tamanhoBloco
          
          sprite = escolherSpriteMinhoca assets numMinhoca (vidaMinhoca minhoca) frameCount armaEquipada
      in Translate (offsetXMapa + x) (offsetYMapa + y) $ Scale 1.8 1.8 $ Pictures
           [ sprite
           , Scale 0.55 0.55 $ desenharBarraVidaMinhoca (vidaMinhoca minhoca)
           ]

-- Seleciona sprite apropriado baseado em estado e animação
escolherSpriteMinhoca :: Assets -> Int -> VidaMinhoca -> Int -> Maybe TipoArma -> Picture
escolherSpriteMinhoca assets numMinhoca Morta _ _ = 
  Color (greyN 0.5) $ circleSolid 8

escolherSpriteMinhoca assets numMinhoca (Viva _) frameCount armaEquipada
  | even numMinhoca = escolherSpriteVerde
  | otherwise = escolherSpriteAzul
  where
    sprites = spriteAssets assets
    
    escolherSpriteVerde = case armaEquipada of
      Just Bazuca -> getOr (wormGreenBazuca sprites) fallbackVerde
      Just Dinamite -> getOr (wormGreenDinamite sprites) fallbackVerde
      Just Mina -> getOr (wormGreenMina sprites) fallbackVerde
      Just Escavadora -> getOr (wormGreenEscavadora sprites) fallbackVerde
      Just Jetpack -> getOr (wormGreenJetpack sprites) fallbackVerde
      Nothing -> escolherAnimacaoVerde
    
    escolherSpriteAzul = case armaEquipada of
      Just Bazuca -> getOr (wormBlueBazuca sprites) fallbackAzul
      Just Dinamite -> getOr (wormBlueDinamite sprites) fallbackAzul
      Just Mina -> getOr (wormBlueMina sprites) fallbackAzul
      Just Escavadora -> getOr (wormBlueEscavadora sprites) fallbackAzul
      Just Jetpack -> getOr (wormBlueJetpack sprites) fallbackAzul
      Nothing -> escolherAnimacaoAzul
    
    escolherAnimacaoVerde
      | frameCount == 0 = getOr (minhocaVerdeIdle sprites) fallbackVerde
      | even (frameCount `div` 10) = getOr (minhocaVerdeWalk1 sprites) fallbackVerde
      | otherwise = getOr (minhocaVerdeWalk2 sprites) fallbackVerde
    
    escolherAnimacaoAzul
      | frameCount == 0 = getOr (minhocaAzulIdle sprites) fallbackAzul
      | even (frameCount `div` 10) = getOr (minhocaAzulWalk1 sprites) fallbackAzul
      | otherwise = getOr (minhocaAzulWalk2 sprites) fallbackAzul
    
    fallbackVerde = Color green $ circleSolid 16
    fallbackAzul = Color cyan $ circleSolid 16
    
    getOr Nothing fallback = fallback
    getOr (Just img) _ = img

desenharBarraVidaMinhoca :: VidaMinhoca -> Picture
desenharBarraVidaMinhoca Morta = Blank
desenharBarraVidaMinhoca (Viva vida) = Translate 0 45 $ Pictures
  [ Color (greyN 0.2) $ rectangleSolid 32 6
  , Color (corPorHP vida) $ Translate deslocamento 0 $ rectangleSolid largura 4
  ]
  where
    proporcao = fromIntegral vida / 100.0
    largura = 30 * proporcao
    deslocamento = -15 + largura / 2

minhocaViva :: Minhoca -> Bool
minhocaViva m = case vidaMinhoca m of
  Viva _ -> True
  Morta -> False

-- Renderização de objetos do jogo
desenharObjetos :: Assets -> [Objeto] -> Picture
desenharObjetos assets objetos = Pictures [desenharObjeto assets obj | obj <- objetos]

desenharObjeto :: Assets -> Objeto -> Picture
desenharObjeto assets (Barril (l, c) prestes) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300
      sprite = getPicOr (barrilSprite $ objetoAssets assets) fallback
      fallback = Color (makeColorI 139 69 19 255) $ rectangleSolid 20 28
      aviso = if prestes then Color (makeColorI 255 0 0 150) $ Circle 18 else Blank
  in Translate x y $ Pictures [sprite, aviso]

desenharObjeto assets (Disparo (l, c) dir tipo _ _) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300
  in Translate x y $ desenharDisparo assets tipo dir

desenharDisparo :: Assets -> TipoArma -> Direcao -> Picture
desenharDisparo assets tipo dir =
  let angulo = anguloParaDirecao dir
  in Rotate angulo $ getIconeArma assets tipo

anguloParaDirecao :: Direcao -> Float
anguloParaDirecao Norte = 90
anguloParaDirecao Sul = -90
anguloParaDirecao Este = 0
anguloParaDirecao Oeste = 180
anguloParaDirecao Nordeste = 45
anguloParaDirecao Sudeste = -45
anguloParaDirecao Noroeste = 135
anguloParaDirecao Sudoeste = -135

-- Sistema de animações
desenharAnimacoes :: Assets -> [AnimacaoAtiva] -> Picture
desenharAnimacoes assets animacoes = Pictures [desenharAnimacao assets anim | anim <- animacoes]

desenharAnimacao :: Assets -> AnimacaoAtiva -> Picture
desenharAnimacao assets (AnimExplosao (l, c) _ frame) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300
  in Translate x y $ escolherFrameExplosao assets frame

desenharAnimacao _ (AnimDano (l, c) dano tempo) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300 + tempo * 20
      alpha = round (tempo * 255)
  in Translate x y $ Color (makeColorI 255 0 0 alpha) $ Scale 0.2 0.2 $ Text ("-" ++ show dano)

desenharAnimacao _ (AnimMovimento _ _ _ _) = Blank

escolherFrameExplosao :: Assets -> Int -> Picture
escolherFrameExplosao assets frame
  | frame == 0 = getPicOr (explosao1 $ objetoAssets assets) (Color red $ circleSolid 30)
  | frame == 1 = getPicOr (explosao2 $ objetoAssets assets) (Color orange $ circleSolid 40)
  | otherwise = getPicOr (explosao3 $ objetoAssets assets) (Color yellow $ circleSolid 30)

-- Telas de vitória e game over
desenharVictory :: Assets -> EstadoFinal -> Picture
desenharVictory assets estadoFinal = 
  let vencedor = vencedorJogo estadoFinal
      bg = case vencedor of
            Just VenceuVerde -> victoryGreen (backgroundAssets assets)
            Just VenceuAzul -> victoryBlue (backgroundAssets assets)
            Nothing -> gameBackground (backgroundAssets assets)
      
      bgImagem = case bg of
                   Just img -> img
                   Nothing -> Color black $ rectangleSolid 1920 1200
      
      textoVitoria = case vencedor of
        Just VenceuVerde -> "JOGADOR 1 VENCEU!"
        Just VenceuAzul -> "JOGADOR 2 VENCEU!"
        Nothing -> "VITORIA!"
      
      corRestart = if opcaoFinal estadoFinal == Restart then yellow else white
      corMenu = if opcaoFinal estadoFinal == VoltarMenu then yellow else white
      
      btnRestart = case buttonRestart (uiAssets assets) of
        Just img -> Scale (if opcaoFinal estadoFinal == Restart then 1.3 else 1.0) (if opcaoFinal estadoFinal == Restart then 1.3 else 1.0) $ img
        Nothing -> Color corRestart $ rectangleSolid 256 64
      
      btnMenu = case buttonMenu (uiAssets assets) of
        Just img -> Scale (if opcaoFinal estadoFinal == VoltarMenu then 1.3 else 1.0) (if opcaoFinal estadoFinal == VoltarMenu then 1.3 else 1.0) $ img
        Nothing -> Color corMenu $ rectangleSolid 256 64
      
  in Pictures
       [ bgImagem
       , Translate 0 (-100) $ btnRestart
       , Translate 0 (-200) $ btnMenu
       ]

desenharGameOver :: Assets -> EstadoFinal -> Picture
desenharGameOver assets estadoFinal = desenharVictory assets estadoFinal

-- Tela de tutorial
desenharTutorial :: Assets -> EstadoTutorial -> Picture
desenharTutorial _ _ = Pictures
  [ Color (makeColorI 20 20 100 255) $ rectangleSolid 1920 1200
  , Color white $ Translate (-150) 400 $ Scale 0.4 0.4 $ Text "TUTORIAL"
  ]

-- Sobreposição de pausa
desenharPausaSeNecessario :: Bool -> Picture
desenharPausaSeNecessario False = Blank
desenharPausaSeNecessario True = Pictures
  [ Color (makeColorI 0 0 0 200) $ rectangleSolid 1920 1200
  , Color white $ Scale 0.6 0.6 $ Text "PAUSA"
  , Translate (-200) (-80) $ Color (greyN 0.8) $ Scale 0.25 0.25 $ Text "P - Continuar"
  , Translate (-200) (-120) $ Color (greyN 0.8) $ Scale 0.25 0.25 $ Text "X - VOLTAR"
  ]