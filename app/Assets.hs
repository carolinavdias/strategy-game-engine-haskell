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
  , uiAssets :: UIAssets
  , frameAssets :: FrameAssets
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
  } deriving (Show)

-- Assets de sprites das minhocas
data SpriteAssets = SpriteAssets
  { minhocaVerdeIdle :: Maybe Picture
  , minhocaVerdeWalk1 :: Maybe Picture
  , minhocaVerdeWalk2 :: Maybe Picture
  , minhocaVerdeSalta :: Maybe Picture 
  , minhocaAzulIdle :: Maybe Picture
  , minhocaAzulWalk1 :: Maybe Picture
  , minhocaAzulWalk2 :: Maybe Picture
  , minhocaAzulSalta :: Maybe Picture
  , wormGreenBig :: Maybe Picture
  , wormBlueBig :: Maybe Picture
  , wormGreenBazuca :: Maybe Picture
  , wormGreenDinamite :: Maybe Picture
  , wormGreenMina :: Maybe Picture
  , wormGreenEscavadora :: Maybe Picture
  , wormGreenJetpack :: Maybe Picture
  , wormBlueBazuca :: Maybe Picture
  , wormBlueDinamite :: Maybe Picture
  , wormBlueMina :: Maybe Picture
  , wormBlueEscavadora :: Maybe Picture
  , wormBlueJetpack :: Maybe Picture
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
  , heartIcon :: Maybe Picture
  , weaponSlot :: Maybe Picture
  , buttonRestart :: Maybe Picture
  , buttonMenu :: Maybe Picture
  } deriving (Show)

-- Assets de molduras
data FrameAssets = FrameAssets
  { stoneFrame :: Maybe Picture
  } deriving (Show)

-- Assets vazios para inicialização
assetsVazios :: Assets
assetsVazios = Assets
  { menuAssets = MenuAssets Nothing Nothing Nothing Nothing Nothing 
                            Nothing Nothing Nothing Nothing Nothing 
                            Nothing Nothing
  , backgroundAssets = BackgroundAssets Nothing Nothing Nothing
  , spriteAssets = SpriteAssets 
      Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing
      Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing
  , objetoAssets = ObjetoAssets Nothing Nothing Nothing Nothing Nothing 
                                Nothing Nothing Nothing Nothing
  , uiAssets = UIAssets Nothing Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing Nothing Nothing Nothing Nothing
  , frameAssets = FrameAssets Nothing
  }