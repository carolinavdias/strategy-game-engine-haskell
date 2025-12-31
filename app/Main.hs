{-|
Module      : Main
Description : Ponto de entrada do jogo Worms
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo contém a função principal do jogo.

Responsabilidades principais:

* Inicialização do estado inicial do jogo
* Carregamento dos assets gráficos
* Arranque do ciclo principal do Gloss
* Ligação entre lógica, desenho e eventos

Este módulo não contém lógica de jogo, funcionando apenas
como orquestrador dos vários subsistemas.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Codec.Picture.Png (decodePng)
import qualified Data.ByteString as B
import Graphics.Gloss.Juicy (fromDynamicImage)
import System.Directory (doesFileExist)
import Data.IORef
import System.Exit (exitSuccess)

import EstadoJogo
import Assets
import Desenhar
import Eventos
import Tempo
import Menu
import SelecaoModo
import Mapas
import Tutorial

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
  
  contadorMapas <- newIORef (0 :: Int)

  let estadoInicial = Menu (EstadoMenu OpcaoPlay 0.0)
  
  playIO window background fps estadoInicial
       (\estado -> return (desenhar assets estado))
       (eventoComContador assets contadorMapas)
       (\dt estado -> return (atualizar assets dt estado))

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
  mapSelBg <- carregarImagem "assets/backgrounds/map_selection_background.png" "Background selecao mapa"
  
  putStrLn "\n  Sprites minhocas:"
  verdeIdle <- carregarImagem "assets/sprites/minhoca_verde_idle.png" "Verde idle"
  verdeWalk1 <- carregarImagem "assets/sprites/minhoca_verde_walk1.png" "Verde walk1"
  verdeWalk2 <- carregarImagem "assets/sprites/minhoca_verde_walk2.png" "Verde walk2"
  verdesalta <- carregarImagem "assets/sprites/minhoca_verde_salta.png" "Verde Salta"
  azulIdle <- carregarImagem "assets/sprites/minhoca_azul_idle.png" "Azul idle"
  azulWalk1 <- carregarImagem "assets/sprites/minhoca_azul_walk1.png" "Azul walk1"
  azulWalk2 <- carregarImagem "assets/sprites/minhoca_azul_walk2.png" "Azul walk2"
  azulsalta <- carregarImagem "assets/sprites/minhoca_azul_salta.png" "Azul Salta"
  wormGreenBig <- carregarImagem "assets/sprites/worm_green_big.png" "Minhoca verde grande"
  wormBlueBig <- carregarImagem "assets/sprites/worm_blue_big.png" "Minhoca azul grande"
  wormTrainingBig <- carregarImagem "assets/sprites/worm_training_big.png" "Minhoca treino grande"
  
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

  putStrLn "\n  Sprites robô (Bot):"
  robotBigImg <- carregarImagem "assets/sprites/worm_robot_big.png" "Robo grande"
  robotIdleImg <- carregarImagemOpcional "assets/sprites/robot_idle.png" "Robo idle"
  robotWalk1Img <- carregarImagemOpcional "assets/sprites/robot_walk1.png" "Robo walk1"
  robotWalk2Img <- carregarImagemOpcional "assets/sprites/robot_walk2.png" "Robo walk2"
  robotBazucaImg <- carregarImagemOpcional "assets/sprites/robot_bazuca.png" "Robo bazuca"
  robotDinamiteImg <- carregarImagemOpcional "assets/sprites/robot_dinamite.png" "Robo dinamite"
  robotMinaImg <- carregarImagemOpcional "assets/sprites/robot_mina.png" "Robo mina"
  robotEscavadoraImg <- carregarImagemOpcional "assets/sprites/robot_escavadora.png" "Robo escavadora"
  robotJetpackImg <- carregarImagemOpcional "assets/sprites/robot_jetpack.png" "Robo jetpack"

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

  putStrLn " Carregar terreno ..."
  pedra <- carregarImagem "assets/terreno/pedra.png" "Pedra"
  agua <- carregarImagem "assets/terreno/agua.png" "Água"
  terra <- carregarImagem "assets/terreno/terra.png" "Terra"
  
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
  btnWeapR <- carregarImagem "assets/ui/button_weapons_robot.png" "Botao armas robo"
  heart <- carregarImagemOpcional "assets/ui/heart_icon.png" "Icone coracao"
  weapSlot <- carregarImagemOpcional "assets/ui/weapon_slot.png" "Slot arma"
  btnRestart <- carregarImagem "assets/ui/button_restart.png" "Botao restart"
  btnMenu <- carregarImagem "assets/ui/button_menu.png" "Botao menu"
  textTreinoImg <- carregarImagem "assets/ui/text_treino.png" "Texto Treino"
  
  putStrLn "\n  Seleção de Mapa:"
  textEscolheMapaImg <- carregarImagem "assets/ui/text_escolhe_mapa.png" "Texto Escolhe Mapa"
  mapIconClassicoImg <- carregarImagem "assets/ui/map_icon_classico.png" "Icone Classico"
  mapIconMontanhasImg <- carregarImagem "assets/ui/map_icon_montanhas.png" "Icone Montanhas"
  mapIconLagoImg <- carregarImagem "assets/ui/map_icon_lago.png" "Icone Lago"
  mapIconPedreiraImg <- carregarImagem "assets/ui/map_icon_pedreira.png" "Icone Pedreira"
  mapIconIlhasImg <- carregarImagem "assets/ui/map_icon_ilhas.png" "Icone Ilhas"
  
  putStrLn "\n  Molduras:"
  stoneF <- carregarImagem "assets/frames/stone_frame.png" "Moldura de pedra"

  putStrLn "\n Tutorial"
  im1 <- carregarImagem "assets/tutorial/im1.png" "1 pagina"
  im2 <- carregarImagem "assets/tutorial/im2.png" "2 pagina"
  im3 <- carregarImagem "assets/tutorial/im3.png" "3 pagina"
  im4 <- carregarImagem "assets/tutorial/im4.png" "4 pagina"
  im5 <- carregarImagem "assets/tutorial/im5.png" "5 pagina"

  
  putStrLn "\nAssets carregados com sucesso!\n"
  
  return $ Assets
    { menuAssets = MenuAssets menuBg menuLg btnPlay btnTut btnExit
                              modeBg modeTitle mode2P modeBot modeTraining
                              modeInstr btnBack
    , backgroundAssets = BackgroundAssets gameBg victoryG victoryB mapSelBg
    , spriteAssets = SpriteAssets verdeIdle verdeWalk1 verdeWalk2 verdesalta
                                  azulIdle azulWalk1 azulWalk2 azulsalta
                                  wormGreenBig wormBlueBig wormTrainingBig
                                  greenBazuca greenDinamite greenMina greenEscavadora greenJetpack
                                  blueBazuca blueDinamite blueMina blueEscavadora blueJetpack
                                  robotBigImg robotIdleImg robotWalk1Img robotWalk2Img
                                  robotBazucaImg robotDinamiteImg robotMinaImg 
                                  robotEscavadoraImg robotJetpackImg
    , objetoAssets = ObjetoAssets barril bazuca dinamite mina escavadora
                                  jetpack exp1 exp2 exp3
    , terrenoAsserts = TerrenoAsserts pedra agua terra
    , uiAssets = UIAssets hp0 hp20 hp40 hp60 hp80 hp100
                          textP1 textP2 textCtrl
                          timerG timerB btnWeapG btnWeapB btnWeapR
                          heart weapSlot btnRestart btnMenu textTreinoImg
    , frameAssets = FrameAssets stoneF
    , mapSelectionAssets = MapSelectionAssets textEscolheMapaImg 
                                              mapIconClassicoImg mapIconMontanhasImg
                                              mapIconLagoImg mapIconPedreiraImg mapIconIlhasImg
    , tutorialAssets = TutorialAssets im1 im2 im3 im4 im5
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
  SelecaoMapaTreino estadoSel -> desenharSelecaoMapa assets estadoSel  
  Jogando partida -> Desenhar.desenha assets partida
  GameOver estadoFinal -> desenharGameOver assets estadoFinal
  Victory estadoFinal -> desenharVictory assets estadoFinal
  Tutorial estadoTut -> desenharTutorial assets estadoTut
  Sair -> Blank 

-- EVENTOS COM CONTADOR
eventoComContador :: Assets -> IORef Int -> Event -> EstadoJogo -> IO EstadoJogo
eventoComContador assets contadorRef ev estado = case estado of
  Menu estadoMenu -> return $ eventoMenu ev estadoMenu
  SelecaoModo estadoSel -> eventoSelecaoContador contadorRef ev estadoSel
  SelecaoMapaTreino estadoSel -> return $ eventoSelecaoMapa ev estadoSel  -- NOVO!
  Jogando partida -> return $ eventoJogo ev partida
  GameOver estadoFinal -> eventoVictoryContador contadorRef ev estadoFinal
  Victory estadoFinal -> eventoVictoryContador contadorRef ev estadoFinal
  Tutorial estadoTut -> return $ eventoTutorial ev estadoTut
  Sair -> exitSuccess 

-- EVENTOS NA SELEÇÃO COM CONTADOR
eventoSelecaoContador :: IORef Int -> Event -> EstadoSelecao -> IO EstadoJogo
eventoSelecaoContador contadorRef evento estado = case evento of
  EventKey (SpecialKey KeyDown) Down _ _ ->
    return $ SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyUp) Down _ _ ->
    return $ SelecaoModo (estado { modoSelecionado = modoAnterior (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyRight) Down _ _ ->
    return $ SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    return $ SelecaoModo (estado { modoSelecionado = modoAnterior (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyTab) Down _ _ ->
    return $ SelecaoModo (estado { modoSelecionado = proximoModo (modoSelecionado estado) })
  
  EventKey (SpecialKey KeyEnter) Down _ _ -> do
    case modoSelecionado estado of
      Treino -> return $ SelecaoMapaTreino (EstadoSelecaoMapa 0 0.0)  -- NOVO: Vai para seleção de mapa!
      Voltar -> return $ Menu (EstadoMenu OpcaoPlay 0.0)
      modo -> do
        contador <- readIORef contadorRef
        let novoContador = contador + 1
        writeIORef contadorRef novoContador
        
        let mapaEscolhido = selecionarMapa novoContador
        putStrLn $ "Jogo " ++ show novoContador ++ " - Mapa: " ++ mapaNome mapaEscolhido
        
        return $ iniciarComMapa modo mapaEscolhido novoContador
  
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    return $ Menu (EstadoMenu OpcaoPlay 0.0)
  
  _ -> return $ SelecaoModo estado

-- EVENTOS NA SELEÇÃO DE MAPA (TREINO)
-- mapaSelecionado: 0-4 = mapas, 5 = voltar
eventoSelecaoMapa :: Event -> EstadoSelecaoMapa -> EstadoJogo
eventoSelecaoMapa evento estado = case evento of
  -- Navegação com setas esquerda/direita (só entre mapas)
  EventKey (SpecialKey KeyRight) Down _ _ ->
    if mapaSelecionado estado == 5
    then SelecaoMapaTreino estado  -- No voltar, não faz nada
    else SelecaoMapaTreino (estado { mapaSelecionado = (mapaSelecionado estado + 1) `mod` 5 })
  
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    if mapaSelecionado estado == 5
    then SelecaoMapaTreino estado  -- No voltar, não faz nada
    else SelecaoMapaTreino (estado { mapaSelecionado = (mapaSelecionado estado - 1 + 5) `mod` 5 })
  
  -- Seta para CIMA: vai para o botão voltar
  EventKey (SpecialKey KeyUp) Down _ _ ->
    SelecaoMapaTreino (estado { mapaSelecionado = 5 })
  
  -- Seta para BAIXO: vai para os mapas (meio = 2)
  EventKey (SpecialKey KeyDown) Down _ _ ->
    if mapaSelecionado estado == 5
    then SelecaoMapaTreino (estado { mapaSelecionado = 2 })  -- Vai para o mapa do meio
    else SelecaoMapaTreino estado  -- Já está nos mapas, não faz nada
  
  -- Enter confirma seleção
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    if mapaSelecionado estado == 5
    then SelecaoModo (EstadoSelecao Treino 0.0)  -- Voltar!
    else iniciarTreinoComMapa (mapaSelecionado estado)
  
  -- Escape ou X volta para seleção de modo
  EventKey (SpecialKey KeyEsc) Down _ _ ->
    SelecaoModo (EstadoSelecao Treino 0.0)
  
  EventKey (Char 'x') Down _ _ ->
    SelecaoModo (EstadoSelecao Treino 0.0)
  
  EventKey (Char 'X') Down _ _ ->
    SelecaoModo (EstadoSelecao Treino 0.0)
  
  _ -> SelecaoMapaTreino estado

-- Inicia treino com mapa específico
iniciarTreinoComMapa :: Int -> EstadoJogo
iniciarTreinoComMapa idx = 
  let mapa = selecionarMapa idx
  in Jogando (criarPartidaComMapa Treino (criarEstadoTreino mapa) idx)

-- EVENTOS DE VITÓRIA COM CONTADOR
eventoVictoryContador :: IORef Int -> Event -> EstadoFinal -> IO EstadoJogo
eventoVictoryContador contadorRef (EventKey (Char 'r') Down _ _) estado = do
  contador <- readIORef contadorRef
  let novoContador = contador + 1
  writeIORef contadorRef novoContador
  
  let mapaEscolhido = selecionarMapa novoContador
      modo = modoJogoFinal estado
  putStrLn $ "Reinício " ++ show novoContador ++ " - Mapa: " ++ mapaNome mapaEscolhido ++ " - Modo: " ++ show modo
  
  return $ iniciarComMapa modo mapaEscolhido novoContador

eventoVictoryContador contadorRef (EventKey (Char 'R') Down m p) estado = 
  eventoVictoryContador contadorRef (EventKey (Char 'r') Down m p) estado

eventoVictoryContador _ (EventKey (Char 'x') Down _ _) _ = 
  return $ Menu (EstadoMenu OpcaoPlay 0.0)

eventoVictoryContador _ (EventKey (Char 'X') Down _ _) _ = 
  return $ Menu (EstadoMenu OpcaoPlay 0.0)

eventoVictoryContador _ (EventKey (SpecialKey KeyUp) Down _ _) estado = 
  return $ Victory (estado { opcaoFinal = Restart })

eventoVictoryContador _ (EventKey (SpecialKey KeyDown) Down _ _) estado = 
  return $ Victory (estado { opcaoFinal = VoltarMenu })

eventoVictoryContador contadorRef (EventKey (SpecialKey KeyEnter) Down _ _) estado =
  case opcaoFinal estado of
    Restart -> do
      contador <- readIORef contadorRef
      let novoContador = contador + 1
      writeIORef contadorRef novoContador
      
      let mapaEscolhido = selecionarMapa novoContador
          modo = modoJogoFinal estado
      putStrLn $ "Restart " ++ show novoContador ++ " - Mapa: " ++ mapaNome mapaEscolhido ++ " - Modo: " ++ show modo
      
      return $ iniciarComMapa modo mapaEscolhido novoContador
    
    VoltarMenu -> return $ Menu (EstadoMenu OpcaoPlay 0.0)

eventoVictoryContador _ _ estado = return $ Victory estado

-- FUNÇÃO AUXILIAR: Inicia jogo com mapa específico
iniciarComMapa :: ModoJogo -> MapaCompleto -> Int -> EstadoJogo
iniciarComMapa Voltar _ _ = Menu (EstadoMenu OpcaoPlay 0.0)
iniciarComMapa Treino mapa idx = Jogando (criarPartidaComMapa Treino (criarEstadoTreino mapa) idx)
iniciarComMapa modo mapa idx = Jogando (criarPartidaComMapa modo (criarEstadoDoMapa mapa) idx)


-- Atualiza o estado do jogo ao longo do tempo
atualizar :: Assets -> Float -> EstadoJogo -> EstadoJogo
atualizar assets dt estado = case estado of
  Menu estadoMenu -> atualizarMenu dt estadoMenu
  SelecaoModo estadoSel -> atualizarSelecao dt estadoSel
  SelecaoMapaTreino estadoSel -> SelecaoMapaTreino (estadoSel { animacaoSelecaoMapa = animacaoSelecaoMapa estadoSel + dt })
  Jogando partida -> atualizarPartidaCompleta dt partida
  Sair -> exitSuccess `seq` Sair 
  _ -> estado