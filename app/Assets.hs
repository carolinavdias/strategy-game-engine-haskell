{-|
Module      : Assets
Description : Gestão de recursos gráficos
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Estrutura para armazenar todas as imagens e recursos do jogo.
-}

module Assets where

import Graphics.Gloss

-- Container principal de todos os assets
data Assets = Assets
  { menuAssets :: MenuAssets
  , backgroundAssets :: BackgroundAssets
  , spriteAssets :: SpriteAssets
  , objetoAssets :: ObjetoAssets
  , terrenoAsserts :: TerrenoAsserts
  , uiAssets :: UIAssets
  , frameAssets :: FrameAssets
  , mapSelectionAssets :: MapSelectionAssets  
  , tutorialAssets :: TutorialAssets
  } deriving (Show)

-- Assets do menu principal
data MenuAssets = MenuAssets
  { menuBackground :: Maybe Picture
  , menuLogo :: Maybe Picture
  , buttonPlay :: Maybe Picture
  , buttonTutorial :: Maybe Picture
  , buttonExit :: Maybe Picture
  , modeBackground :: Maybe Picture
  , modeTitle :: Maybe Picture
  , modeButton2P :: Maybe Picture
  , modeButtonBot :: Maybe Picture
  , modeButtonTraining :: Maybe Picture
  , modeInstructions :: Maybe Picture
  , buttonBack :: Maybe Picture
  } deriving (Show)

-- Assets de backgrounds
data BackgroundAssets = BackgroundAssets
  { gameBackground :: Maybe Picture
  , victoryGreen :: Maybe Picture
  , victoryBlue :: Maybe Picture
  , mapSelectionBackground :: Maybe Picture  -- NOVO!
  } deriving (Show)

-- Assets de sprites das minhocas e robô
data SpriteAssets = SpriteAssets
  { -- Minhoca Verde
    minhocaVerdeIdle :: Maybe Picture
  , minhocaVerdeWalk1 :: Maybe Picture
  , minhocaVerdeWalk2 :: Maybe Picture
  , minhocaVerdeSalta :: Maybe Picture 
  
  -- Minhoca Azul
  , minhocaAzulIdle :: Maybe Picture
  , minhocaAzulWalk1 :: Maybe Picture
  , minhocaAzulWalk2 :: Maybe Picture
  , minhocaAzulSalta :: Maybe Picture
  
  -- Minhocas grandes (laterais)
  , wormGreenBig :: Maybe Picture
  , wormBlueBig :: Maybe Picture
  , wormTrainingBig :: Maybe Picture
  
  -- Minhoca Verde com armas
  , wormGreenBazuca :: Maybe Picture
  , wormGreenDinamite :: Maybe Picture
  , wormGreenMina :: Maybe Picture
  , wormGreenEscavadora :: Maybe Picture
  , wormGreenJetpack :: Maybe Picture
  
  -- Minhoca Azul com armas
  , wormBlueBazuca :: Maybe Picture
  , wormBlueDinamite :: Maybe Picture
  , wormBlueMina :: Maybe Picture
  , wormBlueEscavadora :: Maybe Picture
  , wormBlueJetpack :: Maybe Picture
  
  -- ROBÔ (para modo VsBot)
  , robotBig :: Maybe Picture
  , robotIdle :: Maybe Picture
  , robotWalk1 :: Maybe Picture
  , robotWalk2 :: Maybe Picture
  , robotBazuca :: Maybe Picture
  , robotDinamite :: Maybe Picture
  , robotMina :: Maybe Picture
  , robotEscavadora :: Maybe Picture
  , robotJetpack :: Maybe Picture
  } deriving (Show)

-- Assets de objetos e armas
data ObjetoAssets = ObjetoAssets
  { barrilSprite :: Maybe Picture
  , bazucaIcon :: Maybe Picture
  , dinamiteIcon :: Maybe Picture
  , minaIcon :: Maybe Picture
  , escavadoraIcon :: Maybe Picture
  , jetpackIcon :: Maybe Picture
  , explosao1 :: Maybe Picture
  , explosao2 :: Maybe Picture
  , explosao3 :: Maybe Picture
  } deriving (Show)

-- | Terreno 
data TerrenoAsserts = TerrenoAsserts
  { pedra :: Maybe Picture
  , agua :: Maybe Picture
  , terra :: Maybe Picture
  } deriving (Show)

-- Assets da interface do jogo
data UIAssets = UIAssets
  { hpBar0 :: Maybe Picture
  , hpBar20 :: Maybe Picture
  , hpBar40 :: Maybe Picture
  , hpBar60 :: Maybe Picture
  , hpBar80 :: Maybe Picture
  , hpBar100 :: Maybe Picture
  , textPlayer1 :: Maybe Picture
  , textPlayer2 :: Maybe Picture
  , textControls :: Maybe Picture
  , timerGreen :: Maybe Picture
  , timerBlue :: Maybe Picture
  , buttonWeaponsGreen :: Maybe Picture
  , buttonWeaponsBlue :: Maybe Picture
  , buttonWeaponsRobot :: Maybe Picture
  , heartIcon :: Maybe Picture
  , weaponSlot :: Maybe Picture
  , buttonRestart :: Maybe Picture
  , buttonMenu :: Maybe Picture
  , textTreino :: Maybe Picture  
  } deriving (Show)

-- Assets de molduras
data FrameAssets = FrameAssets
  { stoneFrame :: Maybe Picture
  } deriving (Show)


--  Assets para seleção de mapa
data MapSelectionAssets = MapSelectionAssets
  { textEscolheMapa :: Maybe Picture
  , mapIconClassico :: Maybe Picture
  , mapIconMontanhas :: Maybe Picture
  , mapIconLago :: Maybe Picture
  , mapIconPedreira :: Maybe Picture
  , mapIconIlhas :: Maybe Picture
  } deriving (Show)


-- Assets do Tutorial 
data TutorialAssets = TutorialAssets
  { tutorialPagina1 :: Maybe Picture
  , tutorialPagina2 :: Maybe Picture
  , tutorialPagina3 :: Maybe Picture
  , tutorialPagina4 :: Maybe Picture
  , tutorialPagina5 :: Maybe Picture
  } deriving (Show)


-- Assets vazios para inicialização
assetsVazios :: Assets
assetsVazios = Assets
  { menuAssets = MenuAssets Nothing Nothing Nothing Nothing Nothing 
                            Nothing Nothing Nothing Nothing Nothing 
                            Nothing Nothing
  , backgroundAssets = BackgroundAssets Nothing Nothing Nothing Nothing
  , spriteAssets = SpriteAssets 
      Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  , objetoAssets = ObjetoAssets Nothing Nothing Nothing Nothing Nothing 
                                Nothing Nothing Nothing Nothing
  , terrenoAsserts = TerrenoAsserts Nothing Nothing Nothing
  , uiAssets = UIAssets Nothing Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                        Nothing  -- textTreino
  , frameAssets = FrameAssets Nothing
  , mapSelectionAssets = MapSelectionAssets Nothing Nothing Nothing Nothing Nothing Nothing
  , tutorialAssets = TutorialAssets Nothing Nothing Nothing Nothing Nothing
  }