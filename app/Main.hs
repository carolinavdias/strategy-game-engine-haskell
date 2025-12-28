{-|
Module      : Main
Description : Ponto de entrada do jogo Worms
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Codec.Picture.Png (decodePng)
import qualified Data.ByteString as B
import Graphics.Gloss.Juicy (fromDynamicImage)
import System.Directory (doesFileExist)

import EstadoJogo
import Assets
import Desenhar
import Eventos
import Tempo
import Menu
import SelecaoModo

-- Configuração da janela do jogo
window :: Display
window = FullScreen

-- Cor de fundo
background :: Color
background = makeColorI 135 206 235 255

-- Taxa de atualização
fps :: Int
fps = 60

-- Função principal
main :: IO ()
main = do
  putStrLn "Iniciando Worms..."
  assets <- carregarAssets
  
  let estadoInicial = Menu (EstadoMenu OpcaoPlay 0.0)
  
  play window background fps estadoInicial
       (desenhar assets)
       (evento assets)
       (atualizar assets)

-- Carregamento de todos os assets do jogo
carregarAssets :: IO Assets
carregarAssets = do
  putStrLn "\nCarregando assets..."
  
  putStrLn "\n  Menu:"
  menuBg <- carregarImagem "assets/menu/background.png" "Background menu"
  menuLg <- carregarImagem "assets/menu/logo.png" "Logo"
  btnPlay <- carregarImagem "assets/menu/button_play.png" "Botao Play"
  btnTut <- carregarImagem "assets/menu/button_tutorial.png" "Botao Tutorial"
  btnExit <- carregarImagem "assets/menu/button_exit.png" "Botao Exit"
  modeBg <- carregarImagem "assets/menu/mode_background.png" "Background modos"
  modeTitle <- carregarImagem "assets/menu/mode_title.png" "Titulo modos"
  mode2P <- carregarImagem "assets/menu/character_2players.png" "2 Jogadores"
  modeBot <- carregarImagem "assets/menu/character_vsbot.png" "VS Bot"
  modeTraining <- carregarImagem "assets/menu/character_training.png" "Treino"
  modeInstr <- carregarImagem "assets/menu/mode_instructions.png" "Instrucoes"
  btnBack <- carregarImagem "assets/menu/button_back.png" "Botao Voltar"
  
  putStrLn "\n  Backgrounds:"
  gameBg <- carregarImagem "assets/backgrounds/game_background.png" "Background jogo"
  victoryG <- carregarImagem "assets/backgrounds/victory_green.png" "Vitoria verde"
  victoryB <- carregarImagem "assets/backgrounds/victory_blue.png" "Vitoria azul"
  
  putStrLn "\n  Sprites minhocas:"
  verdeIdle <- carregarImagem "assets/sprites/minhoca_verde_idle.png" "Verde idle"
  verdeWalk1 <- carregarImagem "assets/sprites/minhoca_verde_walk1.png" "Verde walk1"
  verdeWalk2 <- carregarImagem "assets/sprites/minhoca_verde_walk2.png" "Verde walk2"
  azulIdle <- carregarImagem "assets/sprites/minhoca_azul_idle.png" "Azul idle"
  azulWalk1 <- carregarImagem "assets/sprites/minhoca_azul_walk1.png" "Azul walk1"
  azulWalk2 <- carregarImagem "assets/sprites/minhoca_azul_walk2.png" "Azul walk2"
  wormGreenBig <- carregarImagem "assets/sprites/worm_green_big.png" "Minhoca verde grande"
  wormBlueBig <- carregarImagem "assets/sprites/worm_blue_big.png" "Minhoca azul grande"
  
  putStrLn "  Sprites com armas:"
  greenBazuca <- carregarImagem "assets/sprites/worm_green_bazuca.png" "Verde bazuca"
  greenDinamite <- carregarImagem "assets/sprites/worm_green_dinamite.png" "Verde dinamite"
  greenMina <- carregarImagem "assets/sprites/worm_green_mina.png" "Verde mina"
  greenEscavadora <- carregarImagem "assets/sprites/worm_green_escavadora.png" "Verde escavadora"
  greenJetpack <- carregarImagem "assets/sprites/worm_green_jetpack.png" "Verde jetpack"
  blueBazuca <- carregarImagem "assets/sprites/worm_blue_bazuca.png" "Azul bazuca"
  blueDinamite <- carregarImagem "assets/sprites/worm_blue_dinamite.png" "Azul dinamite"
  blueMina <- carregarImagem "assets/sprites/worm_blue_mina.png" "Azul mina"
  blueEscavadora <- carregarImagem "assets/sprites/worm_blue_escavadora.png" "Azul escavadora"
  blueJetpack <- carregarImagem "assets/sprites/worm_blue_jetpack.png" "Azul jetpack"

  putStrLn "\n  Objetos e armas:"
  barril <- carregarImagem "assets/objetos/barril.png" "Barril"
  bazuca <- carregarImagem "assets/objetos/bazuka.png" "Bazuca"
  dinamite <- carregarImagem "assets/objetos/dinamite.png" "Dinamite"
  mina <- carregarImagem "assets/objetos/mina.png" "Mina"
  escavadora <- carregarImagem "assets/objetos/escavadora.png" "Escavadora"
  jetpack <- carregarImagem "assets/objetos/jetpack.png" "Jetpack"
  exp1 <- carregarImagem "assets/objetos/explosao1.png" "Explosao 1"
  exp2 <- carregarImagem "assets/objetos/explosao2.png" "Explosao 2"
  exp3 <- carregarImagem "assets/objetos/explosao3.png" "Explosao 3"
  
  putStrLn "\n  Interface (UI):"
  hp0 <- carregarImagem "assets/ui/hp_bar_0.png" "Barra vida 0"
  hp20 <- carregarImagem "assets/ui/hp_bar_20.png" "Barra vida 20"
  hp40 <- carregarImagem "assets/ui/hp_bar_40.png" "Barra vida 40"
  hp60 <- carregarImagem "assets/ui/hp_bar_60.png" "Barra vida 60"
  hp80 <- carregarImagem "assets/ui/hp_bar_80.png" "Barra vida 80"
  hp100 <- carregarImagem "assets/ui/hp_bar_100.png" "Barra vida 100"
  textP1 <- carregarImagem "assets/ui/text_player1.png" "Texto Jogador 1"
  textP2 <- carregarImagem "assets/ui/text_player2.png" "Texto Jogador 2"
  textCtrl <- carregarImagem "assets/ui/text_controls.png" "Texto controlos"
  timerG <- carregarImagem "assets/ui/timer_green.png" "Timer verde"
  timerB <- carregarImagem "assets/ui/timer_blue.png" "Timer azul"
  btnWeapG <- carregarImagem "assets/ui/button_weapons_green.png" "Botao armas verde"
  btnWeapB <- carregarImagem "assets/ui/button_weapons_blue.png" "Botao armas azul"
  heart <- carregarImagemOpcional "assets/ui/heart_icon.png" "Icone coracao"
  weapSlot <- carregarImagemOpcional "assets/ui/weapon_slot.png" "Slot arma"
  btnRestart <- carregarImagem "assets/ui/button_restart.png" "Botao restart"
  btnMenu <- carregarImagem "assets/ui/button_menu.png" "Botao menu"
  
  putStrLn "\n  Molduras:"
  stoneF <- carregarImagem "assets/frames/stone_frame.png" "Moldura de pedra"
  
  putStrLn "\nAssets carregados com sucesso!\n"
  
  return $ Assets
    { menuAssets = MenuAssets menuBg menuLg btnPlay btnTut btnExit
                              modeBg modeTitle mode2P modeBot modeTraining
                              modeInstr btnBack
    , backgroundAssets = BackgroundAssets gameBg victoryG victoryB
    , spriteAssets = SpriteAssets verdeIdle verdeWalk1 verdeWalk2
                                  azulIdle azulWalk1 azulWalk2
                                  wormGreenBig wormBlueBig 
                                  greenBazuca greenDinamite greenMina greenEscavadora greenJetpack
                                  blueBazuca blueDinamite blueMina blueEscavadora blueJetpack
    , objetoAssets = ObjetoAssets barril bazuca dinamite mina escavadora
                                  jetpack exp1 exp2 exp3
    , uiAssets = UIAssets hp0 hp20 hp40 hp60 hp80 hp100
                          textP1 textP2 textCtrl
                          timerG timerB btnWeapG btnWeapB
                          heart weapSlot btnRestart btnMenu 
    , frameAssets = FrameAssets stoneF
    }

-- Carrega uma imagem PNG do disco
carregarImagem :: FilePath -> String -> IO (Maybe Picture)
carregarImagem path nome = do
  existe <- doesFileExist path
  if not existe
    then do
      putStrLn $ "    Aviso: " ++ nome ++ " nao encontrado (" ++ path ++ ")"
      return Nothing
    else do
      putStr $ "    Carregando " ++ nome ++ "..."
      conteudo <- B.readFile path
      case decodePng conteudo of
        Left err -> do
          putStrLn $ " Erro: " ++ err
          return Nothing
        Right img -> do
          putStrLn " OK"
          return (fromDynamicImage img)

-- Carrega imagem opcional sem mostrar erro
carregarImagemOpcional :: FilePath -> String -> IO (Maybe Picture)
carregarImagemOpcional path nome = do
  existe <- doesFileExist path
  if not existe
    then return Nothing
    else carregarImagem path nome

-- Desenha o estado atual do jogo
desenhar :: Assets -> EstadoJogo -> Picture
desenhar assets estado = case estado of
  Menu estadoMenu -> desenharMenu assets estadoMenu
  SelecaoModo estadoSel -> desenharSelecao assets estadoSel
  Jogando partida -> Desenhar.desenha assets partida
  GameOver estadoFinal -> desenharGameOver assets estadoFinal
  Victory estadoFinal -> desenharVictory assets estadoFinal
  Tutorial estadoTut -> desenharTutorial assets estadoTut

-- Processa eventos de input
evento :: Assets -> Event -> EstadoJogo -> EstadoJogo
evento assets ev estado = case estado of
  Menu estadoMenu -> eventoMenu ev estadoMenu
  SelecaoModo estadoSel -> eventoSelecao ev estadoSel
  Jogando partida -> eventoJogo ev partida
  GameOver estadoFinal -> eventoGameOver ev estadoFinal
  Victory estadoFinal -> eventoVictory ev estadoFinal
  Tutorial estadoTut -> eventoTutorial ev estadoTut

-- Atualiza o estado do jogo ao longo do tempo
atualizar :: Assets -> Float -> EstadoJogo -> EstadoJogo
atualizar assets dt estado = case estado of
  Menu estadoMenu -> atualizarMenu dt estadoMenu
  SelecaoModo estadoSel -> atualizarSelecao dt estadoSel
  Jogando partida -> atualizarPartidaCompleta dt partida
  _ -> estado