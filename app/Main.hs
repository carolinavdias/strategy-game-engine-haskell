{-|
Module      : Main
Description : Ponto de entrada do jogo Worms.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Carrega todos os assets (imagens) e inicia o loop principal do jogo com Gloss.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import System.Directory (doesFileExist)
import Control.Monad (forM)

import Assets
import EstadoJogo
import Menu
import SelecaoModo

-- | Configuração da janela do jogo
janela :: Display
janela = FullScreen

-- | Cor de fundo
corFundo :: Color
corFundo = makeColorI 20 20 40 255

-- | FPS do jogo
fps :: Int
fps = 60

-- | Função principal - ponto de entrada
main :: IO ()
main = do
  putStrLn "🎮 Iniciando WORMS..."
  putStrLn "📁 A carregar assets..."
  
  -- Carregar todos os assets
  assets <- carregarAssets
  
  putStrLn "✅ Assets carregados!"
  putStrLn "🚀 A iniciar jogo...\n"
  
  -- Estado inicial (começa no menu)
  let estadoInicial' = estadoInicial assets
  
  -- Iniciar loop do jogo com Gloss
  play janela corFundo fps 
       (assets, estadoInicial')  -- Estado: (Assets, EstadoJogo)
       desenhar                   -- Desenhar
       evento                     -- Eventos
       atualizar                  -- Atualizar

-- | Carrega todos os assets do jogo
carregarAssets :: IO Assets
carregarAssets = do
  -- Menu
  putStrLn "  📋 Carregando menu..."
  menuBg <- carregarImagem "assets/menu/background.png" "Menu Background"
  menuLg <- carregarImagem "assets/menu/logo.png" "Menu Logo"
  btnPlay <- carregarImagem "assets/menu/button_play.png" "Botão Play"
  btnTutorial <- carregarImagem "assets/menu/button_tutorial.png" "Botão Tutorial"
  btnExit <- carregarImagem "assets/menu/button_exit.png" "Botão Exit"
  
  putStrLn "  🎮 Carregando seleção de modo..."
  modeBg <- carregarImagem "assets/menu/mode_background.png" "Mode Background"
  modeTitle <- carregarImagem "assets/menu/mode_title.png" "Mode Title"
  mode2P <- carregarImagem "assets/menu/character_2players.png" "Character 2P"
  modeBot <- carregarImagem "assets/menu/character_vsbot.png" "Character VS Bot"
  modeTraining <- carregarImagem "assets/menu/character_training.png" "Character Training"
  modeInstr <- carregarImagem "assets/menu/mode_instructions.png" "Mode Instructions"
  btnBack <- carregarImagem "assets/menu/button_back.png" "Button Back"
  
  -- Sprites
  putStrLn "  🐛 Carregando sprites..."
  mvIdle <- carregarImagem "assets/sprites/minhoca_verde_idle.png" "Minhoca Verde Idle"
  mvWalk1 <- carregarImagem "assets/sprites/minhoca_verde_walk1.png" "Minhoca Verde Walk1"
  mvWalk2 <- carregarImagem "assets/sprites/minhoca_verde_walk2.png" "Minhoca Verde Walk2"
  mvHurt <- carregarImagem "assets/sprites/minhoca_verde_hurt.png" "Minhoca Verde Hurt"
  maIdle <- carregarImagem "assets/sprites/minhoca_azul_idle.png" "Minhoca Azul Idle"
  maWalk1 <- carregarImagem "assets/sprites/minhoca_azul_walk1.png" "Minhoca Azul Walk1"
  maWalk2 <- carregarImagem "assets/sprites/minhoca_azul_walk2.png" "Minhoca Azul Walk2"
  maHurt <- carregarImagem "assets/sprites/minhoca_azul_hurt.png" "Minhoca Azul Hurt"
  
  -- Objetos
  putStrLn "  💣 Carregando objetos..."
  barril <- carregarImagem "assets/objetos/barril.png" "Barril"
  exp1 <- carregarImagem "assets/objetos/explosao1.png" "Explosão 1"
  exp2 <- carregarImagem "assets/objetos/explosao2.png" "Explosão 2"
  exp3 <- carregarImagem "assets/objetos/explosao3.png" "Explosão 3"
  jetpack <- carregarImagem "assets/objetos/jetpack.png" "Jetpack"
  escavadora <- carregarImagem "assets/objetos/escavadora.png" "Escavadora"
  bazuca <- carregarImagem "assets/objetos/bazuca.png" "Bazuca"
  mina <- carregarImagem "assets/objetos/mina.png" "Mina"
  dinamite <- carregarImagem "assets/objetos/dinamite.png" "Dinamite"
  
  -- UI
  putStrLn "  🎨 Carregando UI..."
  heart <- carregarImagem "assets/ui/heart_icon.png" "Heart Icon"
  slot <- carregarImagem "assets/ui/weapon_slot.png" "Weapon Slot"
  
  -- Backgrounds
  putStrLn "  🖼️  Carregando backgrounds..."
  gameBg <- carregarImagem "assets/backgrounds/game_background.png" "Game Background"
  
  return $ Assets
    { menuAssets = MenuAssets menuBg menuLg btnPlay btnTutorial btnExit modeBg modeTitle mode2P modeBot modeTraining modeInstr btnBack
    , spriteAssets = SpriteAssets mvIdle mvWalk1 mvWalk2 mvHurt maIdle maWalk1 maWalk2 maHurt
    , objetoAssets = ObjetoAssets barril exp1 exp2 exp3 jetpack escavadora bazuca mina dinamite
    , uiAssets = UIAssets heart slot
    , backgroundAssets = BackgroundAssets gameBg
    }

-- | Carrega uma imagem individual (com fallback se não existir)
carregarImagem :: FilePath -> String -> IO (Maybe Picture)
carregarImagem caminho nome = do
  existe <- doesFileExist caminho
  if existe
    then do
      resultado <- loadJuicy caminho
      case resultado of
        Just img -> do
          putStrLn $ "    ✅ " ++ nome
          return (Just img)
        Nothing -> do
          putStrLn $ "    ⚠️  " ++ nome ++ " - erro ao carregar"
          return Nothing
    else do
      putStrLn $ "    ℹ️  " ++ nome ++ " - ficheiro não encontrado"
      return Nothing

-- | Desenha o estado atual do jogo
desenhar :: (Assets, EstadoJogo) -> Picture
desenhar (assets, estado) = case estado of
  Menu estadoMenu -> desenharMenu assets estadoMenu
  SelecaoModo estadoSelecao -> desenharSelecao assets estadoSelecao
  Jogando _ -> desenharJogoPlaceholder
  GameOver _ -> desenharGameOverPlaceholder
  Victory _ -> desenharVictoryPlaceholder
  Tutorial _ -> desenharTutorialPlaceholder

-- | Placeholder para o jogo (ainda não implementado)
desenharJogoPlaceholder :: Picture
desenharJogoPlaceholder = Pictures
  [ Color (makeColorI 50 100 50 255) $ rectangleSolid 1920 1200  -- Era 1400 800
  , Color white $ Scale 0.3 0.3 $ Text "JOGO AQUI"
  , Translate (-200) (-100) $ Color white $ Scale 0.15 0.15 $ Text "(implementar Desenhar.hs)"
  ]

-- | Placeholder para Game Over
desenharGameOverPlaceholder :: Picture
desenharGameOverPlaceholder = Pictures
  [ Color (makeColorI 100 20 20 255) $ rectangleSolid 1920 1200
  , Color white $ Scale 0.4 0.4 $ Text "GAME OVER"
  ]

-- | Placeholder para Victory
desenharVictoryPlaceholder :: Picture
desenharVictoryPlaceholder = Pictures
  [ Color (makeColorI 20 100 20 255) $ rectangleSolid 1920 1200
  , Color white $ Scale 0.4 0.4 $ Text "VICTORY!"
  ]

-- | Placeholder para Tutorial
desenharTutorialPlaceholder :: Picture
desenharTutorialPlaceholder = Pictures
  [ Color (makeColorI 20 20 100 255) $ rectangleSolid 1920 1200
  , Color white $ Scale 0.3 0.3 $ Text "TUTORIAL"
  ]

-- | Processa eventos (teclado, mouse)
evento :: Event -> (Assets, EstadoJogo) -> (Assets, EstadoJogo)
evento e (assets, estado) = (assets, novoEstado)
  where
    novoEstado = case estado of
      Menu estadoMenu -> eventoMenu e estadoMenu
      SelecaoModo estadoSelecao -> eventoSelecao e estadoSelecao
      Jogando _ -> estado  -- TODO: implementar
      GameOver _ -> estado  -- TODO: implementar
      Victory _ -> estado   -- TODO: implementar
      Tutorial _ -> estado  -- TODO: implementar

-- | Atualiza estado do jogo (animações, física, etc)
atualizar :: Float -> (Assets, EstadoJogo) -> (Assets, EstadoJogo)
atualizar dt (assets, estado) = (assets, novoEstado)
  where
    novoEstado = case estado of
      Menu estadoMenu -> Menu (atualizarMenu dt estadoMenu)
      SelecaoModo estadoSelecao -> SelecaoModo (atualizarSelecao dt estadoSelecao)
      Jogando _ -> estado  -- TODO: implementar Tempo.hs
      _ -> estado