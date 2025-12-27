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
import Desenhar
import Eventos
import Tempo  -- NOVO! Importa física e tempo
import Labs2025  -- NOVO! Para VidaMinhoca, Minhoca, etc

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
  putStrLn "📦 A carregar assets..."
  
  -- Carregar todos os assets
  assets <- carregarAssets
  
  putStrLn "✅ Assets carregados!"
  putStrLn "🚀 A iniciar jogo...\n"
  
  -- Estado inicial (começa no menu)
  let estadoInicial' = estadoInicial assets
  
  -- Iniciar loop do jogo com Gloss
  play janela corFundo fps 
       (assets, estadoInicial')  -- Estado: (Assets, EstadoJogo)
       desenharEstado             -- Desenhar (ATUALIZADO!)
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

  -- Seleção de modo
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
  bazuca <- carregarImagem "assets/objetos/bazuka.png" "Bazuca"
  mina <- carregarImagem "assets/objetos/mina.png" "Mina"
  dinamite <- carregarImagem "assets/objetos/dinamite.png" "Dinamite"
  
  -- UI
  putStrLn "  🎨 Carregando UI..."
  heart <- carregarImagem "assets/ui/heart_icon.png" "Heart Icon"
  slot <- carregarImagem "assets/ui/weapon_slot.png" "Weapon Slot"
  
  -- Backgrounds
  putStrLn "  🖼️ Carregando backgrounds..."
  gameBg <- carregarImagem "assets/backgrounds/game_background.png" "Game Background"
  
  return $ Assets
    { menuAssets = MenuAssets
      { menuBackground = menuBg
      , menuLogo = menuLg
      , buttonPlay = btnPlay
      , buttonTutorial = btnTutorial
      , buttonExit = btnExit
      , modeBackground = modeBg
      , modeTitle = modeTitle
      , modeButton2P = mode2P
      , modeButtonBot = modeBot
      , modeButtonTraining = modeTraining
      , modeInstructions = modeInstr
      , buttonBack = btnBack
      }
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
          putStrLn $ "    ⚠️ " ++ nome ++ " - erro ao carregar"
          return Nothing
    else do
      putStrLn $ "    ℹ️ " ++ nome ++ " - ficheiro não encontrado"
      return Nothing

--------------------------------------------------------------------------------
-- * DESENHO (ATUALIZADO COM DESENHAR.HS!)

-- | Desenha o estado atual do jogo
desenharEstado :: (Assets, EstadoJogo) -> Picture
desenharEstado (assets, estado) = case estado of
  Menu estadoMenu -> desenharMenu assets estadoMenu
  SelecaoModo estadoSelecao -> desenharSelecao assets estadoSelecao
  Jogando estadoPartida -> desenha assets estadoPartida  -- USA DESENHAR.HS!
  GameOver estadoFinal -> desenharGameOver assets estadoFinal
  Victory estadoFinal -> desenharVictory assets estadoFinal
  Tutorial estadoTutorial -> desenharTutorial assets estadoTutorial

--------------------------------------------------------------------------------
-- * EVENTOS

-- | Processa eventos (teclado, mouse)
evento :: Event -> (Assets, EstadoJogo) -> (Assets, EstadoJogo)
evento e (assets, estado) = (assets, novoEstado)
  where
    novoEstado = case estado of
      Menu estadoMenu -> eventoMenu e estadoMenu
      SelecaoModo estadoSelecao -> eventoSelecao e estadoSelecao
      Jogando estadoPartida -> eventoJogo e estadoPartida  -- USA EVENTOS.HS!
      GameOver estadoFinal -> eventoGameOver e estadoFinal
      Victory estadoFinal -> eventoVictory e estadoFinal
      Tutorial estadoTutorial -> eventoTutorial e estadoTutorial

--------------------------------------------------------------------------------
-- * ATUALIZAÇÃO

-- | Atualiza estado do jogo (animações, física, etc)
atualizar :: Float -> (Assets, EstadoJogo) -> (Assets, EstadoJogo)
atualizar dt (assets, estado) = (assets, novoEstado)
  where
    novoEstado = case estado of
      Menu estadoMenu -> Menu (atualizarMenu dt estadoMenu)
      SelecaoModo estadoSelecao -> SelecaoModo (atualizarSelecao dt estadoSelecao)
      Jogando estadoPartida -> atualizarEstadoJogando dt estadoPartida  -- USA TEMPO.HS!
      _ -> estado

-- | Atualiza estado quando está jogando (usa Tempo.hs)
atualizarEstadoJogando :: Float -> EstadoPartida -> EstadoJogo
atualizarEstadoJogando dt partida =
  let partidaAtualizada = atualizarPartidaCompleta dt partida
      -- Verifica se jogo terminou
      minhocas = minhocasEstado (estadoWorms partidaAtualizada)
      verdesVivas = length [ m | (i, m) <- zip [0..] minhocas, even i, minhocaEstaViva m ]
      azuisVivas = length [ m | (i, m) <- zip [0..] minhocas, odd i, minhocaEstaViva m ]
      pontos = turnoAtual partidaAtualizada * 10
  in if verdesVivas == 0 && azuisVivas > 0
       then Victory (EstadoFinal pontos Restart)  -- Azul vence
     else if azuisVivas == 0 && verdesVivas > 0
       then Victory (EstadoFinal pontos Restart)  -- Verde vence
     else if verdesVivas == 0 && azuisVivas == 0
       then GameOver (EstadoFinal pontos Restart)  -- Empate
     else Jogando partidaAtualizada  -- Continua
