{-|
Module      : Desenhar  
Description : Renderização visual do jogo
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo é responsável por todo o desenho visual do jogo.

Inclui:

* Desenho do mapa e entidades
* HUD adaptativo consoante o modo de jogo
* Animações de movimento e explosões
* Menus contextuais de armas
* Feedback visual (cores, escalas, destaques)

O desenho é totalmente separado da lógica,
seguindo uma arquitetura limpa e modular.

-}

module Desenhar 
  ( desenha
  , desenharMenu
  , desenharSelecao
  , desenharSelecaoMapa
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
import Tutorial

-- * Função principal de renderização.
-- | Recebe os assets carregados e o estado atual da partida,
-- | produzindo a imagem completa a desenhar no ecrã.
desenha :: Assets -> EstadoPartida -> Picture
desenha assets partida = Pictures
  [ desenharBackground assets
  , desenharMapa assets (mapaEstado $ estadoWorms partida)
  , desenharObjetos assets (mapaEstado $ estadoWorms partida) (objetosEstado $ estadoWorms partida)
  , desenharMinhocasJogo assets 
      (mapaEstado $ estadoWorms partida)
      (minhocasEstado $ estadoWorms partida) 
      (frameCounter partida) 
      (jogadorAtual partida) 
      (armaSelecionadaP1 partida) 
      (armaSelecionadaP2 partida)
      (modoPartida partida)  -- ^ passa modo para desenhar robô
  , desenharAnimacoes assets (mapaEstado $ estadoWorms partida) (animacoes partida)
  , desenharMolduraPedra assets
  , desenharMinhocasLaterais assets (modoPartida partida)  -- ^ passa modo
  , desenharHUDCompleto assets partida
  , desenharMenusArmas assets partida
  , desenharTextosControlos assets
  , desenharPausaSeNecessario (pausado partida)
  ]

-- | Moldura decorativa de pedra
desenharMolduraPedra :: Assets -> Picture
desenharMolduraPedra assets =
  case stoneFrame (frameAssets assets) of
    Just frame -> Scale 0.83 0.83 frame
    Nothing -> Blank

-- | Interface de jogo completa
-- | No modo Treino, não mostra info jogador 2 nem timer
desenharHUDCompleto :: Assets -> EstadoPartida -> Picture
desenharHUDCompleto assets partida
  | modoPartida partida == Treino = Pictures
      [ desenharInfoJogador1Treino assets partida  -- ^ Só info do jogador verde
      ]
  | otherwise = Pictures
      [ desenharInfoJogador1 assets partida
      , desenharInfoJogador2 assets partida
      , desenharTimer assets partida
      ]

--  | Info do jogador 1 para modo Treino (mostra "TREINO" em vez de "JOGADOR 1")
desenharInfoJogador1Treino :: Assets -> EstadoPartida -> Picture
desenharInfoJogador1Treino assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVerdes = [(i, m) | (i, m) <- zip [0..] minhocas, even i, minhocaViva m]
  in case minhocasVerdes of
       ((_, minhoca):_) -> Translate (-790) 520 $ Pictures
         [ case textTreino (uiAssets assets) of  
             Just img -> Scale 1.3 1.3 img
             Nothing -> Color white $ Scale 0.2 0.2 $ Text "TREINO"
         , Translate (-50) (-70) $ desenharBarraVida assets (vidaMinhoca minhoca)
         ]
       [] -> Blank

desenharInfoJogador1 :: Assets -> EstadoPartida -> Picture
desenharInfoJogador1 assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVerdes = [(i, m) | (i, m) <- zip [0..] minhocas, even i, minhocaViva m]
  in case minhocasVerdes of
       ((_, minhoca):_) -> Translate (-790) 520 $ Pictures
         [ desenharTextoJogador assets True (modoPartida partida)
         , Translate (-50) (-70) $ desenharBarraVida assets (vidaMinhoca minhoca)
         ]
       [] -> Blank

desenharInfoJogador2 :: Assets -> EstadoPartida -> Picture
desenharInfoJogador2 assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasAzuis = [(i, m) | (i, m) <- zip [0..] minhocas, odd i, minhocaViva m]
  in case minhocasAzuis of
       ((_, minhoca):_) -> Translate 780 520 $ Pictures
         [ desenharTextoJogador assets False (modoPartida partida)
         , Translate (60) (-70) $ desenharBarraVida assets (vidaMinhoca minhoca)
         ]
       [] -> Blank

-- | Mostra "BOT" em vez de "JOGADOR 2" no modo VsBot
desenharTextoJogador :: Assets -> Bool -> ModoJogo -> Picture
desenharTextoJogador assets isJogador1 modo =
  let textoImg = if isJogador1 
                 then textPlayer1 (uiAssets assets)
                 else textPlayer2 (uiAssets assets)
      -- ^ No modo VsBot, jogador 2 é o Bot
      textoFallback = if isJogador1 
                      then "JOGADOR 1" 
                      else if modo == VsBot then "BOT" else "JOGADOR 2"
  in case textoImg of
       Just img -> Scale 1.4 1.4 $ img
       Nothing -> Color white $ Scale 0.15 0.15 $ Text textoFallback

desenharBarraVida :: Assets -> VidaMinhoca -> Picture
desenharBarraVida assets Morta = escolherBarraHP assets 0
desenharBarraVida assets (Viva hp) = escolherBarraHP assets hp

escolherBarraHP :: Assets -> Int -> Picture
escolherBarraHP assets hp
  | hp >= 100 = getBarraOr assets (hpBar100 $ uiAssets assets) hp
  | hp >= 80  = getBarraOr assets (hpBar80 $ uiAssets assets) hp
  | hp >= 60  = getBarraOr assets (hpBar60 $ uiAssets assets) hp
  | hp >= 40  = getBarraOr assets (hpBar40 $ uiAssets assets) hp
  | hp >= 20  = getBarraOr assets (hpBar20 $ uiAssets assets) hp
  | otherwise = getBarraOr assets (hpBar0 $ uiAssets assets) hp

getBarraOr :: Assets -> Maybe Picture -> Int -> Picture
getBarraOr _ (Just img) _ = Scale 1.5 1.5 img
getBarraOr _ Nothing hp = Pictures
  [ Color (greyN 0.2) $ rectangleSolid 140 30
  , Color (corPorHP hp) $ rectangleSolid (fromIntegral hp * 1.3) 25
  ]

corPorHP :: Int -> Color
corPorHP hp
  | hp > 70 = makeColorI 0 255 0 255
  | hp > 30 = makeColorI 255 255 0 255
  | otherwise = makeColorI 255 0 0 255

-- | Timer com texto simulando negrito
desenharTimer :: Assets -> EstadoPartida -> Picture
desenharTimer assets partida =
  let isVerde = jogadorAtual partida == 0
      timerImg = if isVerde 
                 then timerGreen (uiAssets assets)
                 else timerBlue (uiAssets assets)
      segundos = round (tempoRestante partida) :: Int
  in Translate 10 480 $ Pictures
     [ case timerImg of
         Just img -> Scale 1.1 1.1 $ img
         Nothing -> Color (if isVerde then green else cyan) $ circleSolid 60
     , Translate (-23) (-15) $ desenharNumeroTimer segundos (tempoRestante partida)
     ]

-- | Simula efeito negrito desenhando texto múltiplas vezes
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

-- |Minhocas decorativas laterais
desenharMinhocasLaterais :: Assets -> ModoJogo -> Picture
desenharMinhocasLaterais assets modo
  | modo == Treino = Pictures
      [ desenharMinhocaTreinoLateral assets  -- ^ Treino na esquerda, nada na direita
      ]
  | otherwise = Pictures
      [ desenharMinhocaLateralVerde assets
      , desenharMinhocaLateralAzul assets modo
      ]

-- | Minhoca de treino na lateral ESQUERDA (mesma posição da verde)
desenharMinhocaTreinoLateral :: Assets -> Picture
desenharMinhocaTreinoLateral assets =
  case wormTrainingBig (spriteAssets assets) of
    Just img -> Translate (-825) (-350) $ Scale 2.1 2.1 $ img
    Nothing -> Translate (-830) (-350) $ Color white $ Scale 0.15 0.15 $ Text "TREINO"

desenharMinhocaLateralVerde :: Assets -> Picture
desenharMinhocaLateralVerde assets =
  case wormGreenBig (spriteAssets assets) of
    Just img -> Translate (-830) 200 $ Scale 2.4 2.4 $ img
    Nothing -> Blank

-- | Mostra robô no modo VsBot!
desenharMinhocaLateralAzul :: Assets -> ModoJogo -> Picture
desenharMinhocaLateralAzul assets modo =
  let sprite = if modo == VsBot
               then robotBig (spriteAssets assets)  -- ^ Robô!
               else wormBlueBig (spriteAssets assets)  -- ^ Minhoca
      fallback = if modo == VsBot
                 then desenharRobotFallback  -- ^ Robô simples
                 else Blank
  in case sprite of
       Just img -> Translate 820 200 $ Scale 2.5 2.5 $ img
       Nothing -> Translate 820 200 $ Scale 2.5 2.5 $ fallback

--  |Robô fallback (quando não há imagem)
desenharRobotFallback :: Picture
desenharRobotFallback = Pictures
  [ -- ^ Corpo
    Color (makeColorI 100 100 120 255) $ rectangleSolid 30 35
  , -- ^ Cabeça
    Translate 0 25 $ Color (makeColorI 120 120 140 255) $ rectangleSolid 25 20
  , -- ^ Olhos (LEDs vermelhos)
    Translate (-6) 27 $ Color red $ circleSolid 4
  , Translate 6 27 $ Color red $ circleSolid 4
  , -- ^ Antena
    Translate 0 38 $ Color (makeColorI 80 80 80 255) $ rectangleSolid 3 10
  , Translate 0 45 $ Color red $ circleSolid 3
  , -- ^ Braços
    Translate (-18) 5 $ Color (makeColorI 80 80 100 255) $ rectangleSolid 8 20
  , Translate 18 5 $ Color (makeColorI 80 80 100 255) $ rectangleSolid 8 20
  , -- ^ Pernas
    Translate (-8) (-25) $ Color (makeColorI 80 80 100 255) $ rectangleSolid 8 15
  , Translate 8 (-25) $ Color (makeColorI 80 80 100 255) $ rectangleSolid 8 15
  ]

-- * Sistema de menus de armas
-- | No modo Treino, só menu verde na posição DIREITA
desenharMenusArmas :: Assets -> EstadoPartida -> Picture
desenharMenusArmas assets partida
  | modoPartida partida == Treino = Pictures
      [ desenharLadoEsquerdoTreino assets partida  -- ^ Menu verde na DIREITA
      ]
  | otherwise = Pictures
      [ desenharLadoEsquerdo assets partida
      , desenharLadoDireito assets partida
      ]

-- | Lado para modo Treino (menu verde na posição DIREITA)
desenharLadoEsquerdoTreino :: Assets -> EstadoPartida -> Picture
desenharLadoEsquerdoTreino assets partida =
  if menuArmasAbertoP1 partida
  then desenharMenuArmasEsquerdaNaPosicaoDireita assets partida
  else desenharBotaoArmasVerdeNaPosicaoDireita assets

-- | Botão de armas verde na posição DIREITA (860)
desenharBotaoArmasVerdeNaPosicaoDireita :: Assets -> Picture
desenharBotaoArmasVerdeNaPosicaoDireita assets =
  case buttonWeaponsGreen (uiAssets assets) of
    Just img -> Translate 860 (-90) $ Scale 1.3 1.3 $ img
    Nothing -> Translate 860 (-90) $ Pictures
      [ Color green $ rectangleSolid 120 120
      , Color white $ Scale 0.15 0.15 $ Text "Q"
      ]

-- | Menu de armas verde na posição DIREITA
desenharMenuArmasEsquerdaNaPosicaoDireita :: Assets -> EstadoPartida -> Picture
desenharMenuArmasEsquerdaNaPosicaoDireita assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVerdes = [(i, m) | (i, m) <- zip [0..] minhocas, even i, minhocaViva m]
  in case minhocasVerdes of
       ((_, minhoca):_) -> Translate 820 (-150) $  -- ^ Posição direita!
         desenharListaArmas assets minhoca (armaSelecionadaP1 partida) True
       [] -> Blank

desenharLadoEsquerdo :: Assets -> EstadoPartida -> Picture
desenharLadoEsquerdo assets partida =
  if menuArmasAbertoP1 partida
  then desenharMenuArmasEsquerda assets partida
  else desenharBotaoArmasVerde assets

desenharLadoDireito :: Assets -> EstadoPartida -> Picture
desenharLadoDireito assets partida =
  if menuArmasAbertoP2 partida
  then desenharMenuArmasDireita assets partida
  else desenharBotaoArmasAzul assets (modoPartida partida)  -- ^ Passa modo!

desenharBotaoArmasVerde :: Assets -> Picture
desenharBotaoArmasVerde assets =
  case buttonWeaponsGreen (uiAssets assets) of
    Just img -> Translate (-860) (-90) $ Scale 1.3 1.3 $ img
    Nothing -> Translate (-860) (-90) $ Pictures
      [ Color green $ rectangleSolid 120 120
      , Color white $ Scale 0.15 0.15 $ Text "Q"
      ]

-- | Mostra botão do robô no modo VsBot
desenharBotaoArmasAzul :: Assets -> ModoJogo -> Picture
desenharBotaoArmasAzul assets modo =
  let btnImg = if modo == VsBot
               then buttonWeaponsRobot (uiAssets assets)  -- ^ Botão do robô!
               else buttonWeaponsBlue (uiAssets assets)   -- ^ Botão azul normal
      fallbackColor = if modo == VsBot 
                      then makeColorI 100 100 120 255  -- ^ Cinza metálico para robô
                      else cyan
  in case btnImg of
       Just img -> Translate 860 (-90) $ Scale 1.3 1.3 $ img
       Nothing -> Translate 860 (-90) $ Pictures
         [ Color fallbackColor $ rectangleSolid 120 120
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

-- | Lista de armas com teclas numeradas
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

-- * Texto de controlos no rodapé
desenharTextosControlos :: Assets -> Picture
desenharTextosControlos assets =
  case textControls (uiAssets assets) of
    Just img -> Translate 0 (-520) $ Scale 1 1 $ img
    Nothing -> Translate 0 (-520) $ 
               Color white $ Scale 0.2 0.2 $
               Text "P-PAUSA | X-VOLTAR | ESPACO-PASSAR"

-- * Background em modo fullscreen
desenharBackground :: Assets -> Picture
desenharBackground assets =
  case gameBackground (backgroundAssets assets) of
    Just bg -> bg
    Nothing -> Color (makeColorI 135 206 235 255) $ rectangleSolid 1920 1200

-- * Renderização do mapa do jogo

tamanhoBloco :: Float
tamanhoBloco = 32.0

desenharMapa :: Assets -> Mapa -> Picture
desenharMapa assets mapa = 
  let numColunas = if null mapa then 0 else length (head mapa)
      numLinhas = length mapa
      larguraMapa = fromIntegral numColunas * tamanhoBloco
      alturaMapa = fromIntegral numLinhas * tamanhoBloco
      
      offsetX = -larguraMapa / 2 + 10
      offsetY = alturaMapa / 2 - 20
  
  in Scale 1.3 1.3 $    
    Translate offsetX offsetY $ Pictures $ concat
    [ [ desenharBloco assets terreno (l, c)
      | (c, terreno) <- zip [0..] linha
      ]
    | (l, linha) <- zip [0..] mapa
    ]

desenharBloco :: Assets -> Terreno -> Posicao -> Picture
desenharBloco _ Ar _ = Blank
desenharBloco assets terreno (l, c) = 
  Translate x y $ escolherImagemTerreno assets terreno
  where
    x = fromIntegral c * tamanhoBloco 
    y = -fromIntegral l * tamanhoBloco 

-- | Escolhe a imagem correta para cada tipo de terreno
escolherImagemTerreno :: Assets -> Terreno -> Picture
escolherImagemTerreno assets Agua = 
  case agua (terrenoAsserts assets) of
    Just img -> Scale escala escala img
    Nothing -> Color (makeColorI 30 144 255 180) $ rectangleSolid tamanhoBloco tamanhoBloco
  where
    escala = tamanhoBloco / 32.0  -- ^ Ajusta para o tamanho do bloco

escolherImagemTerreno assets Terra = 
  case terra (terrenoAsserts assets) of
    Just img -> Scale escala escala img
    Nothing -> Color (makeColorI 139 69 19 255) $ rectangleSolid tamanhoBloco tamanhoBloco
  where
    escala = tamanhoBloco / 32.0

escolherImagemTerreno assets Pedra = 
  case pedra (terrenoAsserts assets) of
    Just img -> Scale escala escala img
    Nothing -> Color (makeColorI 105 105 105 255) $ rectangleSolid tamanhoBloco tamanhoBloco
  where
    escala = tamanhoBloco / 32.0

escolherImagemTerreno _ _ = Blank

-- *  Renderização de minhocas no jogo (recebe modo)

desenharMinhocasJogo :: Assets -> Mapa -> [Minhoca] -> Int -> Int -> Maybe TipoArma -> Maybe TipoArma -> ModoJogo -> Picture
desenharMinhocasJogo assets mapa minhocas frameCount jogadorAtualNum armaP1 armaP2 modo =
  Pictures
    [ desenharMinhocaJogo assets mapa i minhoca
        (getFrameParaMinhoca i)
        (getArmaParaMinhoca i)
        modo  -- ^ Passa modo!
    | (i, minhoca) <- zip [0..] minhocas
    , posicaoMinhoca minhoca /= Nothing
    ]
    where
    getFrameParaMinhoca numMinhoca
      | even numMinhoca && jogadorAtualNum == 0 = frameCount
      | odd  numMinhoca && jogadorAtualNum == 1 = frameCount
      | otherwise = 0

    getArmaParaMinhoca numMinhoca
      | even numMinhoca && jogadorAtualNum == 0 = armaP1
      | odd  numMinhoca && jogadorAtualNum == 1 = armaP2
      | otherwise = Nothing

-- | Desenha minhoca ou robô dependendo do modo
desenharMinhocaJogo :: Assets -> Mapa -> Int -> Minhoca -> Int -> Maybe TipoArma -> ModoJogo -> Picture
desenharMinhocaJogo assets mapa numMinhoca minhoca frameCount armaEquipada modo =
  case posicaoMinhoca minhoca of
    Nothing -> Blank
    Just (l, c) ->
      let
          numLinhas  = length mapa
          numColunas = if null mapa then 0 else length (head mapa)

          larguraMapa = fromIntegral numColunas * tamanhoBloco
          alturaMapa  = fromIntegral numLinhas * tamanhoBloco

          offsetX = -larguraMapa / 2 + 10
          offsetY =  alturaMapa / 2 -20

          x = fromIntegral c * tamanhoBloco
          y = -fromIntegral l * tamanhoBloco

          sprite =
            escolherSpriteMinhoca assets numMinhoca
              (vidaMinhoca minhoca) frameCount armaEquipada modo  -- ^ Passa modo!

      in Scale 1.3 1.3 $
         Translate offsetX offsetY $
         Translate x y $
         Scale 1.4 1.4 $
           Pictures
             [ sprite
             , Scale 0.55 0.55 $
               desenharBarraVidaMinhoca (vidaMinhoca minhoca)
             ]

-- | Seleciona sprite apropriado (minhoca azul mantém-se no mapa, mesmo em VsBot)
escolherSpriteMinhoca :: Assets -> Int -> VidaMinhoca -> Int -> Maybe TipoArma -> ModoJogo -> Picture
escolherSpriteMinhoca _ _ Morta _ _ _ = 
  Color (greyN 0.5) $ circleSolid 8

escolherSpriteMinhoca assets numMinhoca (Viva _) frameCount armaEquipada _modo
  | even numMinhoca = escolherSpriteVerde  -- ^ Minhoca verde
  | otherwise = escolherSpriteAzul          -- ^ Minhoca azul (sempre!)
  where
    sprites = spriteAssets assets
    
    -- ^ Sprites da minhoca verde
    escolherSpriteVerde = case armaEquipada of
      Just Bazuca -> getOr (wormGreenBazuca sprites) fallbackVerde
      Just Dinamite -> getOr (wormGreenDinamite sprites) fallbackVerde
      Just Mina -> getOr (wormGreenMina sprites) fallbackVerde
      Just Escavadora -> getOr (wormGreenEscavadora sprites) fallbackVerde
      Just Jetpack -> getOr (wormGreenJetpack sprites) fallbackVerde
      Nothing -> escolherAnimacaoVerde
    
    -- ^ Sprites da minhoca azul (sempre minhoca, mesmo em VsBot)
    escolherSpriteAzul = case armaEquipada of
      Just Bazuca -> getOr (wormBlueBazuca sprites) fallbackAzul
      Just Dinamite -> getOr (wormBlueDinamite sprites) fallbackAzul
      Just Mina -> getOr (wormBlueMina sprites) fallbackAzul
      Just Escavadora -> getOr (wormBlueEscavadora sprites) fallbackAzul
      Just Jetpack -> getOr (wormBlueJetpack sprites) fallbackAzul
      Nothing -> escolherAnimacaoAzul
    
    -- ^ Animações da minhoca verde
    escolherAnimacaoVerde
      | frameCount == 0 = getOr (minhocaVerdeIdle sprites) fallbackVerde
      | even (frameCount `div` 10) = getOr (minhocaVerdeWalk1 sprites) fallbackVerde
      | otherwise = getOr (minhocaVerdeWalk2 sprites) fallbackVerde
    
    -- ^ Animações da minhoca azul
    escolherAnimacaoAzul
      | frameCount == 0 = getOr (minhocaAzulIdle sprites) fallbackAzul
      | even (frameCount `div` 10) = getOr (minhocaAzulWalk1 sprites) fallbackAzul
      | otherwise = getOr (minhocaAzulWalk2 sprites) fallbackAzul
    
    -- ^ Fallbacks
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
  Viva hp -> hp >= 1  -- Vida tem que ser pelo menos 1
  Morta -> False

-- * Renderização de objetos do jogo

desenharObjetos :: Assets -> Mapa -> [Objeto] -> Picture
desenharObjetos assets mapa objetos = Pictures [desenharObjeto assets mapa obj | obj <- objetos]

desenharObjeto :: Assets -> Mapa -> Objeto -> Picture
desenharObjeto assets mapa (Barril (l, c) prestes) =
 let
      numLinhas  = length mapa
      numColunas = if null mapa then 0 else length (head mapa)

      larguraMapa = fromIntegral numColunas * tamanhoBloco
      alturaMapa  = fromIntegral numLinhas * tamanhoBloco

      offsetX = -larguraMapa / 2
      offsetY =  alturaMapa / 2

      x = fromIntegral c * tamanhoBloco
      y = -fromIntegral l * tamanhoBloco

      sprite = getPicOr (barrilSprite $ objetoAssets assets) fallback
      fallback = Color (makeColorI 139 69 19 255) $ rectangleSolid 20 28
      aviso = if prestes then Color (makeColorI 255 0 0 150) $ Circle 18 else Blank
  in
      Translate offsetX offsetY $
      Translate x y $
      Pictures [sprite, aviso]

desenharObjeto assets mapa (Disparo (l, c) dir arma tempo_ _) =
 let
      numLinhas  = length mapa
      numColunas = if null mapa then 0 else length (head mapa)

      larguraMapa = fromIntegral numColunas * tamanhoBloco
      alturaMapa  = fromIntegral numLinhas * tamanhoBloco

      offsetX = -larguraMapa / 2
      offsetY =  alturaMapa / 2

      x = fromIntegral c * tamanhoBloco
      y = -fromIntegral l * tamanhoBloco

      sprite = desenharDisparo assets arma dir tempo_
  in
      Translate offsetX offsetY $
      Translate x y $
      sprite

desenharDisparo :: Assets -> TipoArma -> Direcao -> Maybe Int -> Picture
desenharDisparo assets tipo dir tempo =
  let angulo = anguloParaDirecao dir
      -- ~ Mina pulsa quando tempo <= 3
      escala = case (tipo, tempo) of
        (Mina, Just t) | t <= 3 -> 1.0 + 0.3 * abs (sin (fromIntegral t * 5))  -- ^ Pulsa!
        _ -> 1.0
  in Rotate angulo $ Scale escala escala $ getIconeArma assets tipo

anguloParaDirecao :: Direcao -> Float
anguloParaDirecao Norte = 90
anguloParaDirecao Sul = -90
anguloParaDirecao Este = 0
anguloParaDirecao Oeste = 180
anguloParaDirecao Nordeste = 45
anguloParaDirecao Sudeste = -45
anguloParaDirecao Noroeste = 135
anguloParaDirecao Sudoeste = -135

-- * Sistema de animações

desenharAnimacoes :: Assets -> Mapa -> [AnimacaoAtiva] -> Picture
desenharAnimacoes assets  mapa anims = Pictures [desenharAnimacao assets mapa anim | anim <- anims]

desenharAnimacao :: Assets -> Mapa -> AnimacaoAtiva -> Picture
desenharAnimacao assets mapa (AnimExplosao (l, c) _ frame) =
  let
      numLinhas  = length mapa
      numColunas = if null mapa then 0 else length (head mapa)

      larguraMapa = fromIntegral numColunas * tamanhoBloco
      alturaMapa  = fromIntegral numLinhas * tamanhoBloco

      offsetX = -larguraMapa / 2
      offsetY =  alturaMapa / 2

      x = fromIntegral c * tamanhoBloco
      y = -fromIntegral l * tamanhoBloco
  in
      Translate offsetX offsetY $
      Translate x y $
      escolherFrameExplosao assets frame
desenharAnimacao _ _ (AnimDano _ _ _) = Blank
desenharAnimacao _ _ (AnimMovimento _ _ _ _) = Blank

escolherFrameExplosao :: Assets -> Int -> Picture
escolherFrameExplosao assets frame
  | frame == 0 = Scale 1.5 1.5 $ getPicOr (explosao1 $ objetoAssets assets) (Color red $ circleSolid 45)
  | frame == 1 = getPicOr (explosao2 $ objetoAssets assets) (Color orange $ circleSolid 60)
  | otherwise = getPicOr (explosao3 $ objetoAssets assets) (Color yellow $ circleSolid 45)

-- * Telas de vitória e game over
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

-- * Tela de tutorial
desenharTutorial :: Assets -> EstadoTutorial -> Picture
desenharTutorial assets estado = desenharTutorialCompleto assets estado

-- * Sobreposição de pausa
desenharPausaSeNecessario :: Bool -> Picture
desenharPausaSeNecessario False = Blank
desenharPausaSeNecessario True = Pictures
  [ Color (makeColorI 0 0 0 200) $ rectangleSolid 1920 1200
  , Color white $ Scale 0.6 0.6 $ Text "PAUSA"
  , Translate (-200) (-80) $ Color (greyN 0.8) $ Scale 0.25 0.25 $ Text "P - Continuar"
  , Translate (-200) (-120) $ Color (greyN 0.8) $ Scale 0.25 0.25 $ Text "X - VOLTAR"
  ]

-- * Desenha ecrã de seleção de mapa
desenharSelecaoMapa :: Assets -> EstadoSelecaoMapa -> Picture
desenharSelecaoMapa assets estado = Pictures
  [ -- ^ Background
    case mapSelectionBackground (backgroundAssets assets) of
      Just bg -> bg
      Nothing -> Color (makeColorI 50 50 80 255) $ rectangleSolid 1920 1200
  
  -- ^ Título "ESCOLHE O MAPA"
  , Translate 0 340 $ case textEscolheMapa (mapSelectionAssets assets) of
      Just img -> Scale 1 1 img
      Nothing -> Color white $ Scale 0.3 0.3 $ Text "ESCOLHE O MAPA"
  
  -- ^ Ícones dos mapas
  , desenharIconesMapas assets (mapaSelecionado estado)
  
  -- ^ Botão voltar (SELECIONÁVEL! índice 5)
  , desenharBotaoVoltarMapa assets (mapaSelecionado estado == 5)
  
  -- ^ Instruções (canto superior direito)
  , Translate 720 480 $ case modeInstructions (menuAssets assets) of
      Just img -> Scale 0.5 0.5 img
      Nothing -> Blank
  ]

-- | Botão voltar IGUAL ao do ecrã de modos
desenharBotaoVoltarMapa :: Assets -> Bool -> Picture
desenharBotaoVoltarMapa assets selecionado = Translate (-850) 480 $ Pictures
  [ feedback
  , botao
  ]
  where
    escala = if selecionado then 1.3 else 1.0
    
    botao = case buttonBack (menuAssets assets) of
      Just img -> Scale escala escala img
      Nothing -> Pictures
                   [ Color red $ circleSolid 50
                   , Color white $ Scale 0.3 0.3 $ Text "<"
                   ]
    
    feedback = if selecionado 
               then Color (makeColorI 255 255 255 60) $ circleSolid 80
               else Blank

-- | Desenha os 5 ícones dos mapas lado a lado (SEM números, SEM pulsar)
desenharIconesMapas :: Assets -> Int -> Picture
desenharIconesMapas assets selecionado = Pictures
  [ desenharIconeMapa 0 selecionado (mapIconClassico $ mapSelectionAssets assets)
  , desenharIconeMapa 1 selecionado (mapIconMontanhas $ mapSelectionAssets assets)
  , desenharIconeMapa 2 selecionado (mapIconLago $ mapSelectionAssets assets)
  , desenharIconeMapa 3 selecionado (mapIconPedreira $ mapSelectionAssets assets)
  , desenharIconeMapa 4 selecionado (mapIconIlhas $ mapSelectionAssets assets)
  ]

-- | Desenha um ícone de mapa (SEM número, SEM animação pulsar)
desenharIconeMapa :: Int -> Int -> Maybe Picture -> Picture
desenharIconeMapa idx selecionado mImg =
  let -- ^ Posição X: distribuídos de -500 a 500
      posX = fromIntegral (idx - 2) * 250  -- -500, -250, 0, 250, 500
      
      -- ^  Escala: maior se selecionado (SEM pulsar!)
      estaSelecionado = idx == selecionado
      escalaFinal = if estaSelecionado then 2.6 else 2.1
      
      -- ^ Brilho amarelo se selecionado
      brilho = if estaSelecionado
               then Color (makeColorI 255 255 0 100) $ circleSolid 100
               else Blank
      
      -- ^ Ícone ou fallback
      icone = case mImg of
        Just img -> Scale escalaFinal escalaFinal img
        Nothing -> Scale escalaFinal escalaFinal $ 
          Color (makeColorI 100 150 100 255) $ rectangleSolid 150 150
      
  in Translate posX (-50) $ Pictures [brilho, icone]  -- Mais para baixo!