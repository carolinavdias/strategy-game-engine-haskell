{-|
Module      : SelecaoModo
Description : Seleção do modo de jogo.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Tela para escolher entre 2 Jogadores, VS Bot ou Treino.
-}

module SelecaoModo where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Assets
import Labs2025

-- Desenha a tela de seleção de modo
desenharSelecao :: Assets -> EstadoSelecao -> Picture
desenharSelecao assets estado = Pictures
  [ desenharBackgroundSelecao assets
  , desenharTituloSelecao assets
  , desenharModos assets estado   
  , desenharInstrucoesSelecao assets 
  ]

-- Background escurecido
desenharBackgroundSelecao :: Assets -> Picture
desenharBackgroundSelecao assets =
  case modeBackground (menuAssets assets) of
    Just bg -> bg
    Nothing -> Color (makeColorI 40 30 60 200) $ rectangleSolid 1920 1200

-- Título "ESCOLHE UM MODO"
desenharTituloSelecao :: Assets -> Picture
desenharTituloSelecao assets =
  case modeTitle (menuAssets assets) of
    Just title -> Translate 0 360 title
    Nothing -> Translate 0 400 $ 
               Color white $ 
               Scale 0.4 0.4 $ 
               Text "ESCOLHE UM MODO"

-- Desenha os 3 modos mais botão voltar como opção navegável
desenharModos :: Assets -> EstadoSelecao -> Picture
desenharModos assets estado = Pictures
  [ desenharModo2P assets (modoSelecionado estado == DoisJogadores) (-450, -60)
  , desenharModoBot assets (modoSelecionado estado == VsBot) (0, -60)
  , desenharModoTraining assets (modoSelecionado estado == Treino) (450, -60)
  , desenharBotaoVoltarSelecionavel assets (modoSelecionado estado == Voltar)
  ]

-- Modo 2 Jogadores
desenharModo2P :: Assets -> Bool -> (Float, Float) -> Picture
desenharModo2P assets selecionado (x, y) = Translate x y $ Pictures
  [ feedback selecionado
  , personagens
  ]
  where
    escala = if selecionado then 1.2 else 0.9
    
    personagens = case modeButton2P (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> Color green $ circleSolid 100
    
    feedback True = Translate 0 0 $
                    Color (makeColorI 255 255 255 40) $ 
                    rectangleSolid 420 520
    feedback False = Blank

-- Modo VS Bot
desenharModoBot :: Assets -> Bool -> (Float, Float) -> Picture
desenharModoBot assets selecionado (x, y) = Translate x y $ Pictures
  [ feedback selecionado
  , personagens
  ]
  where
    escala = if selecionado then 1.2 else 0.9
    
    personagens = case modeButtonBot (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> Color magenta $ circleSolid 100
    
    feedback True = Translate 0 0 $
                    Color (makeColorI 255 255 255 40) $ 
                    rectangleSolid 420 520
    feedback False = Blank

-- Modo Treino
desenharModoTraining :: Assets -> Bool -> (Float, Float) -> Picture
desenharModoTraining assets selecionado (x, y) = Translate x y $ Pictures
  [ feedback selecionado
  , personagens
  ]
  where
    escala = if selecionado then 1.2 else 0.9
    
    personagens = case modeButtonTraining (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> Color orange $ circleSolid 100
    
    feedback True = Translate 0 0 $
                    Color (makeColorI 255 255 255 40) $ 
                    rectangleSolid 420 520
    feedback False = Blank

-- Botão voltar como opção selecionável e navegável
desenharBotaoVoltarSelecionavel :: Assets -> Bool -> Picture
desenharBotaoVoltarSelecionavel assets selecionado = Translate (-850) 520 $ Pictures
  [ feedback selecionado
  , botao
  ]
  where
    escala = if selecionado then 1.3 else 1
    
    botao = case buttonBack (menuAssets assets) of
      Just img -> Scale escala escala $ img
      Nothing -> Pictures
                   [ Color red $ circleSolid 50
                   , Color white $ Scale 0.3 0.3 $ Text "<"
                   ]
    
    feedback True = Color (makeColorI 255 255 255 60) $ circleSolid 80
    feedback False = Blank

-- Instruções discretas no canto superior direito
desenharInstrucoesSelecao :: Assets -> Picture
desenharInstrucoesSelecao assets =
  case modeInstructions (menuAssets assets) of
    Just img -> Translate 720 520 $ Scale 0.5 0.5 $ img
    Nothing -> Blank

-- Processa input na seleção de modo
eventoSelecao :: Event -> EstadoSelecao -> EstadoJogo
eventoSelecao evento estado = case evento of
  EventKey (SpecialKey KeyDown) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyUp) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = modoAnterior (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyRight) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = modoAnterior (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyTab) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    iniciarJogo (modoSelecionado estado)
  
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    Menu (EstadoMenu OpcaoPlay 0.0)
  
  _ -> SelecaoModo estado

-- Avança para o próximo modo
proximoModo :: ModoJogo -> ModoJogo
proximoModo DoisJogadores = VsBot
proximoModo VsBot = Treino
proximoModo Treino = Voltar
proximoModo Voltar = DoisJogadores

-- Volta para o modo anterior
modoAnterior :: ModoJogo -> ModoJogo
modoAnterior DoisJogadores = Voltar
modoAnterior VsBot = DoisJogadores
modoAnterior Treino = VsBot
modoAnterior Voltar = Treino

-- Inicia uma partida no modo selecionado ou volta ao menu
iniciarJogo :: ModoJogo -> EstadoJogo
iniciarJogo Voltar = Menu (EstadoMenu OpcaoPlay 0.0)
iniciarJogo modo = Jogando (criarPartida modo estadoInicialWorms)
  where
    mapaExemplo = 
      [ [Pedra | _ <- [1..34]] ] ++
      [ [if c == 1 || c == 34 then Pedra else Ar | c <- [1..34]] | _ <- [1..9] ] ++
      [ [if c == 1 || c == 34 then Pedra else Terra | c <- [1..34]] | _ <- [1..8] ] ++
      [ [if c == 1 || c == 34 then Pedra else Agua | c <- [1..34]] | _ <- [1..2] ]
    
    barrisExemplo = 
      [ Barril (9, 8) False
      , Barril (9, 13) False
      , Barril (9, 17) False
      , Barril (9, 24) False
      ]
    
    minhocasExemplo = 
      [ Minhoca (Just (8, 6)) (Viva 100) 2 3 5 2 3
      , Minhoca (Just (8, 26)) (Viva 100) 2 3 5 2 3
      ]
    
    estadoInicialWorms = Estado mapaExemplo barrisExemplo minhocasExemplo

-- Atualiza animação da seleção
atualizarSelecao :: Float -> EstadoSelecao -> EstadoJogo
atualizarSelecao dt estado = SelecaoModo (estado { animacaoGlowSelecao = animacaoGlowSelecao estado + dt * 5 })