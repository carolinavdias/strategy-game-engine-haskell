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

-- | Desenha a tela de seleção de modo
desenharSelecao :: Assets -> EstadoSelecao -> Picture
desenharSelecao assets estado = Pictures
  [ desenharBackgroundSelecao assets
  , desenharTitulo
  , desenharOpcoesModo assets estado
  , desenharInstrucoes
  ]

-- | Background da seleção (usa mesmo do menu)
desenharBackgroundSelecao :: Assets -> Picture
desenharBackgroundSelecao assets =
  case menuBackground (menuAssets assets) of
    Just bg -> bg
    Nothing -> Color (makeColorI 80 40 120 255) $ rectangleSolid 1400 800

-- | Título da tela
desenharTitulo :: Picture
desenharTitulo = Translate (-250) 300 $ Pictures
  [ Color white $ Scale 0.4 0.4 $ Text "ESCOLHE O MODO"
  , Translate 0 (-50) $ Color (greyN 0.7) $ Scale 0.2 0.2 $ Text "Seleciona com as setas"
  ]

-- | Desenha as 3 opções de modo
desenharOpcoesModo :: Assets -> EstadoSelecao -> Picture
desenharOpcoesModo assets estado = Pictures
  [ desenharOpcaoModo DoisJogadores (modoSelecionado estado == DoisJogadores) (0, 100)
  , desenharOpcaoModo VsBot (modoSelecionado estado == VsBot) (0, -50)
  , desenharOpcaoModo Treino (modoSelecionado estado == Treino) (0, -200)
  ]

-- | Desenha uma opção de modo individual
desenharOpcaoModo :: ModoJogo -> Bool -> (Float, Float) -> Picture
desenharOpcaoModo modo selecionado (x, y) = Translate x y $ Pictures
  [ efeitoGlow selecionado
  , fundo selecionado
  , icone modo
  , texto modo selecionado
  ]
  where
    -- Glow quando selecionado
    efeitoGlow True = Color (makeColorI 0 255 255 80) $ 
                      rectangleSolid 520 120
    efeitoGlow False = Blank
    
    -- Fundo do botão
    fundo True = Color (makeColorI 0 200 200 200) $ rectangleSolid 500 100
    fundo False = Color (makeColorI 50 50 80 200) $ rectangleSolid 500 100

-- | Ícone do modo (emoji simples)
icone :: ModoJogo -> Picture
icone modo = Translate (-200) 0 $ Color white $ Scale 0.5 0.5 $ Text (iconeTexto modo)
  where
    iconeTexto DoisJogadores = "2P"
    iconeTexto VsBot = "VS"
    iconeTexto Treino = "T"

-- | Texto descritivo do modo
texto :: ModoJogo -> Bool -> Picture
texto modo selecionado = Translate (-100) (-10) $ 
                         Color (if selecionado then white else greyN 0.7) $ 
                         Scale 0.25 0.25 $ 
                         Text (textoModo modo)
  where
    textoModo DoisJogadores = "2 JOGADORES"
    textoModo VsBot = "VS BOT"
    textoModo Treino = "TREINO"

-- | Instruções na parte inferior
desenharInstrucoes :: Picture
desenharInstrucoes = Translate (-200) (-350) $ Pictures
  [ Color (greyN 0.6) $ Scale 0.15 0.15 $ Text "ENTER - Selecionar"
  , Translate 0 (-30) $ Color (greyN 0.6) $ Scale 0.15 0.15 $ Text "ESC - Voltar"
  ]

-- | Processa input na seleção de modo
eventoSelecao :: Event -> EstadoSelecao -> EstadoJogo
eventoSelecao evento estado = case evento of
  -- Seta para baixo - próximo modo
  EventKey (SpecialKey KeyDown) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  -- Seta para cima - modo anterior
  EventKey (SpecialKey KeyUp) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = modoAnterior (modoSelecionado estado) })
  
  -- Tab - alterna modos
  EventKey (SpecialKey KeyTab) Down _ _ ->
    SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  -- Enter - inicia jogo no modo selecionado
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    iniciarJogo (modoSelecionado estado)
  
  -- ESC - volta ao menu
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    Menu (EstadoMenu OpcaoPlay 0.0)
  
  -- Outros eventos
  _ -> SelecaoModo estado

-- | Avança para o próximo modo
proximoModo :: ModoJogo -> ModoJogo
proximoModo DoisJogadores = VsBot
proximoModo VsBot = Treino
proximoModo Treino = DoisJogadores

-- | Volta para o modo anterior
modoAnterior :: ModoJogo -> ModoJogo
modoAnterior DoisJogadores = Treino
modoAnterior VsBot = DoisJogadores
modoAnterior Treino = VsBot

-- | Inicia uma partida no modo selecionado
iniciarJogo :: ModoJogo -> EstadoJogo
iniciarJogo modo = Jogando (criarPartida modo estadoInicialWorms)
  where
    -- Estado inicial do Worms (mapa de exemplo)
    estadoInicialWorms = Estado mapaExemplo [] minhocasExemplo
    
    -- Mapa de exemplo (30x20)
    mapaExemplo = 
      [ [Pedra | _ <- [1..30]] ] ++
      [ [if c == 1 || c == 30 then Pedra else Ar | c <- [1..30]] | _ <- [1..15] ] ++
      [ [if c == 1 || c == 30 then Pedra 
           else if l >= 18 then Terra 
           else Ar 
         | c <- [1..30]] 
      | l <- [16..19] ] ++
      [ [Pedra | _ <- [1..30]] ]
    
    -- Minhocas de exemplo
    minhocasExemplo = 
      [ Minhoca (Just (5, 5)) (Viva 100) 2 3 5 2 3    -- Verde
      , Minhoca (Just (5, 25)) (Viva 100) 2 3 5 2 3   -- Azul
      ]

-- | Atualiza animação da seleção
atualizarSelecao :: Float -> EstadoSelecao -> EstadoSelecao
atualizarSelecao dt estado = estado { animacaoGlowSelecao = novo }
  where
    novo = animacaoGlowSelecao estado + dt * 2