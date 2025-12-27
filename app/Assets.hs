{-|
Module      : Assets
Description : Estrutura de dados para assets (imagens) do jogo.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Define as estruturas para guardar todas as imagens carregadas do jogo.
-}

module Assets where

import Graphics.Gloss

-- | Todos os assets do jogo organizados por categoria
data Assets = Assets
  { menuAssets :: MenuAssets
  , spriteAssets :: SpriteAssets
  , objetoAssets :: ObjetoAssets
  , uiAssets :: UIAssets
  , backgroundAssets :: BackgroundAssets
  } deriving (Show)

-- | Assets do menu principal
data MenuAssets = MenuAssets
  { menuBackground :: Maybe Picture
  , menuLogo :: Maybe Picture
  , buttonPlay :: Maybe Picture
  , buttonTutorial :: Maybe Picture
  , buttonExit :: Maybe Picture
  , modeBackground :: Maybe Picture      -- Background escurecido
  , modeTitle :: Maybe Picture           -- "ESCOLHE UM MODO"
  , modeButton2P :: Maybe Picture        -- Personagens 2 jogadores
  , modeButtonBot :: Maybe Picture       -- Personagens VS Bot
  , modeButtonTraining :: Maybe Picture 
  , modeInstructions :: Maybe Picture    -- Instruções
  , buttonBack :: Maybe Picture -- Personagens Treino
  } deriving (Show)

-- | Sprites das minhocas (verde e azul)
data SpriteAssets = SpriteAssets
  { minhocaVerdeIdle :: Maybe Picture
  , minhocaVerdeWalk1 :: Maybe Picture
  , minhocaVerdeWalk2 :: Maybe Picture
  , minhocaVerdeHurt :: Maybe Picture
  , minhocaAzulIdle :: Maybe Picture
  , minhocaAzulWalk1 :: Maybe Picture
  , minhocaAzulWalk2 :: Maybe Picture
  , minhocaAzulHurt :: Maybe Picture
  } deriving (Show)

-- | Objetos do jogo (armas, barril, explosões)
data ObjetoAssets = ObjetoAssets
  { barrilSprite :: Maybe Picture
  , explosao1 :: Maybe Picture
  , explosao2 :: Maybe Picture
  , explosao3 :: Maybe Picture
  , jetpackIcon :: Maybe Picture
  , escavadoraIcon :: Maybe Picture
  , bazucaIcon :: Maybe Picture
  , minaIcon :: Maybe Picture
  , dinamiteIcon :: Maybe Picture
  } deriving (Show)

-- | Assets da interface (UI)
data UIAssets = UIAssets
  { heartIcon :: Maybe Picture
  , weaponSlot :: Maybe Picture
  } deriving (Show)

-- | Backgrounds do jogo
data BackgroundAssets = BackgroundAssets
  { gameBackground :: Maybe Picture
  } deriving (Show)

-- | Cria estrutura vazia de assets (antes de carregar)
assetsVazios :: Assets
assetsVazios = Assets
  { menuAssets = MenuAssets Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  , spriteAssets = SpriteAssets Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  , objetoAssets = ObjetoAssets Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  , uiAssets = UIAssets Nothing Nothing
  , backgroundAssets = BackgroundAssets Nothing
  }