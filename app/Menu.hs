{-|
Module      : Menu
Description : Lógica do menu principal.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo implementa o menu principal da aplicação.

Funcionalidades:

* Opções navegáveis: Play, Tutorial, Exit
* Feedback visual com animações de escala
* Suporte para teclado (setas, Enter, Espaço, Tab)
* Animação suave do logótipo

O menu serve como ponto central de navegação
para todas as outras funcionalidades do jogo.
-}

module Menu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Assets

-- | Desenha o menu principal
desenharMenu :: Assets -> EstadoMenu -> Picture
desenharMenu assets estado = Pictures
  [ desenharBackground assets
  , desenharLogo assets (animacaoMenu estado)
  , desenharBotoes assets estado
  , desenharInstrucoesMenu assets 
  ]

-- | Desenha o background do menu
desenharBackground :: Assets -> Picture
desenharBackground assets = 
  case menuBackground (menuAssets assets) of
    Just bg -> bg  -- SEM SCALE! Imagem já é 1920x1200!
    Nothing -> Color (makeColorI 60 40 80 255) $ rectangleSolid 1920 1200

-- Desenha o logo com animação de pulsar
desenharLogo :: Assets -> Float -> Picture
desenharLogo assets tempo =
  let -- Pulsar suave: varia entre 0.95 e 1.05
      escala = 1.90 + 0.05 * sin (tempo * 0.5)  -- 1.5 = velocidade (mais baixo = mais lento)
  in case menuLogo (menuAssets assets) of
       Just logo -> Translate 0 320 $ Scale escala escala logo
       Nothing -> Translate 0 320 $ Color white $ Scale 0.5 0.5 $ Text "WORMS"

-- | Desenha os três botões do menu
desenharBotoes :: Assets -> EstadoMenu -> Picture
desenharBotoes assets estado = Pictures
  [ desenharBotao assets OpcaoPlay (opcaoSelecionadaMenu estado == OpcaoPlay) (0, -360)
  , desenharBotao assets OpcaoTutorial (opcaoSelecionadaMenu estado == OpcaoTutorial) (0, -480)
  , desenharBotao assets OpcaoExit (opcaoSelecionadaMenu estado == OpcaoExit) (-850, 480)
  ]

-- | Desenha um botão individual
desenharBotao :: Assets -> OpcaoMenu -> Bool -> (Float, Float) -> Picture
desenharBotao assets opcao selecionado (x, y) = Translate x y botao
  where
    botao = case opcao of
      OpcaoPlay -> desenharBotaoPlay assets selecionado
      OpcaoTutorial -> desenharBotaoTutorial assets selecionado
      OpcaoExit -> desenharBotaoExit assets selecionado

-- | Desenha botão PLAY
-- falta criar button_play_glow.png no Canva com letras brilhantes para versão selecionada !!!
desenharBotaoPlay :: Assets -> Bool -> Picture
desenharBotaoPlay assets selecionado = imagem
  where
    escala = if selecionado then 1.40 else 1.25  -- Aumenta quando selecionado
    
    imagem = case buttonPlay (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> desenharBotaoFallback "PLAY GAME" selecionado (460, 140)

-- | Desenha botão TUTORIAL
--criar button_tutorial_glow.png 
desenharBotaoTutorial :: Assets -> Bool -> Picture
desenharBotaoTutorial assets selecionado = imagem
  where
    escala = if selecionado then 1.25 else 1.1  -- Aumenta quando selecionado
    
    imagem = case buttonTutorial (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> desenharBotaoFallback "TUTORIAL" selecionado (400, 130)

-- | Desenha botão EXIT
--criar button_exit_glow.png 
desenharBotaoExit :: Assets -> Bool -> Picture
desenharBotaoExit assets selecionado = imagem
  where
    escala = if selecionado then 1.60 else 1.4  -- Aumenta quando selecionado
    
    imagem = case buttonExit (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> desenharBotaoFallback "X" selecionado (80, 80)

-- | Desenha botão alternativo (se imagem não carregar)
desenharBotaoFallback :: String -> Bool -> (Float, Float) -> Picture
desenharBotaoFallback texto selecionado (w, h) = Pictures
  [ Color (if selecionado then cyan else greyN 0.3) $ 
    rectangleSolid w h
  , Color white $ 
    Translate (-w/4) (-10) $ 
    Scale 0.2 0.2 $ 
    Text texto
  ]

-- | Processa input do teclado no menu
eventoMenu :: Event -> EstadoMenu -> EstadoJogo
eventoMenu evento estado = case evento of
  -- Seta para baixo - próxima opção
  EventKey (SpecialKey KeyDown) Down _ _ -> 
    Menu (estado { opcaoSelecionadaMenu = proximaOpcao (opcaoSelecionadaMenu estado) })
  
  -- Seta para cima - opção anterior
  EventKey (SpecialKey KeyUp) Down _ _ -> 
    Menu (estado { opcaoSelecionadaMenu = opcaoAnterior (opcaoSelecionadaMenu estado) })
  
  -- Tab - alterna opções
  EventKey (SpecialKey KeyTab) Down _ _ ->
    Menu (estado { opcaoSelecionadaMenu = proximaOpcao (opcaoSelecionadaMenu estado) })
  
  -- Enter - seleciona opção
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    selecionarOpcao (opcaoSelecionadaMenu estado)
  
  -- Espaço - seleciona opção
  EventKey (SpecialKey KeySpace) Down _ _ ->
    selecionarOpcao (opcaoSelecionadaMenu estado)
  
  -- Outros eventos - ignora
  _ -> Menu estado

-- | Avança para a próxima opção do menu
proximaOpcao :: OpcaoMenu -> OpcaoMenu
proximaOpcao OpcaoPlay = OpcaoTutorial
proximaOpcao OpcaoTutorial = OpcaoExit
proximaOpcao OpcaoExit = OpcaoPlay

-- | Volta para a opção anterior do menu
opcaoAnterior :: OpcaoMenu -> OpcaoMenu
opcaoAnterior OpcaoPlay = OpcaoExit
opcaoAnterior OpcaoTutorial = OpcaoPlay
opcaoAnterior OpcaoExit = OpcaoTutorial

-- | Executa ação da opção selecionada
selecionarOpcao :: OpcaoMenu -> EstadoJogo
selecionarOpcao OpcaoPlay = SelecaoModo (EstadoSelecao DoisJogadores 0.0)
selecionarOpcao OpcaoTutorial = Tutorial (EstadoTutorial 0 False)
selecionarOpcao OpcaoExit = Sair 


-- | Atualiza animação do menu (efeito glow pulsante)
atualizarMenu :: Float -> EstadoMenu -> EstadoJogo
atualizarMenu dt estado = Menu (estado { animacaoMenu = animacaoMenu estado + dt * 5 })

-- | Desenha instruções discretas no canto superior direito
desenharInstrucoesMenu :: Assets -> Picture
desenharInstrucoesMenu assets =
  case modeInstructions (menuAssets assets) of
    Just img -> Translate 720 480 $ Scale 0.5 0.5 $ img  -- Canto superior direito, pequeno
    Nothing -> Blank