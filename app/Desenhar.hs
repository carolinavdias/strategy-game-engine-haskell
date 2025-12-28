{-|
Module      : Desenhar
Description : Renderização visual do jogo Worms.
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Módulo responsável por desenhar todos os elementos do jogo:
mapa, minhocas, objetos, UI, e efeitos visuais.
-}

module Desenhar 
  ( desenha
  , desenharMenu
  , desenharSelecao
  , desenharGameOver
  , desenharVictory
  , desenharTutorial
  ) where

import Graphics.Gloss
import EstadoJogo
import Assets
import Labs2025
import Menu (desenharMenu)
import SelecaoModo (desenharSelecao)

--------------------------------------------------------------------------------
-- * FUNÇÃO PRINCIPAL DE DESENHO

-- | Desenha o estado completo de uma partida (CÂMERA FIXA!)
desenha :: Assets -> EstadoPartida -> Picture
desenha assets partida = Pictures
  [ desenharBackground assets
  , desenharMapa (mapaEstado $ estadoWorms partida)
  , desenharObjetos assets (objetosEstado $ estadoWorms partida)
  , desenharMinhocasAnimadas assets (minhocasEstado $ estadoWorms partida) (frameAnimacao partida)
  , desenharAnimacoes assets (animacoes partida)
  , desenharUIDoisJogadores assets partida  
  , desenharPausaSeNecessario (pausado partida)
  ]

--------------------------------------------------------------------------------
-- * TELAS ADICIONAIS

-- | Desenha tela de Game Over
desenharGameOver :: Assets -> EstadoFinal -> Picture
desenharGameOver assets estadoFinal = Pictures
  [ Color (makeColorI 100 20 20 255) $ rectangleSolid 1920 1200
  , Color white $ Translate (-250) 200 $ Scale 0.5 0.5 $ Text "GAME OVER"
  , Color (greyN 0.8) $ Translate (-300) 50 $ Scale 0.2 0.2 $ 
    Text ("PONTUACAO: " ++ show (pontuacaoFinal estadoFinal))
  , desenharOpcoesFinais (opcaoFinal estadoFinal)
  ]

-- | Desenha tela de Vitória
desenharVictory :: Assets -> EstadoFinal -> Picture
desenharVictory assets estadoFinal = Pictures
  [ Color (makeColorI 20 100 20 255) $ rectangleSolid 1920 1200
  , Color white $ Translate (-200) 200 $ Scale 0.5 0.5 $ Text "VITORIA!"
  , Color yellow $ Translate (-300) 50 $ Scale 0.2 0.2 $ 
    Text ("PONTUACAO: " ++ show (pontuacaoFinal estadoFinal))
  , desenharOpcoesFinais (opcaoFinal estadoFinal)
  ]

-- | Desenha opções das telas finais
desenharOpcoesFinais :: OpcaoFinal -> Picture
desenharOpcoesFinais opcao = Pictures
  [ desenharOpcaoFinal Restart (opcao == Restart) (0, -100)
  , desenharOpcaoFinal VoltarMenu (opcao == VoltarMenu) (0, -180)
  ]

desenharOpcaoFinal :: OpcaoFinal -> Bool -> (Float, Float) -> Picture
desenharOpcaoFinal opcaoTipo selecionado (x, y) = Translate x y $ Pictures
  [ Color corFundo $ rectangleSolid 400 60
  , Color corTexto $ Translate (-150) (-10) $ Scale 0.2 0.2 $ Text texto
  ]
  where
    corFundo = if selecionado 
               then makeColorI 0 200 200 200 
               else makeColorI 50 50 80 200
    corTexto = if selecionado then white else greyN 0.7
    texto = case opcaoTipo of
      Restart -> "JOGAR NOVAMENTE"
      VoltarMenu -> "MENU PRINCIPAL"

-- | Desenha tutorial
desenharTutorial :: Assets -> EstadoTutorial -> Picture
desenharTutorial assets tutorial = Pictures
  [ Color (makeColorI 20 20 100 255) $ rectangleSolid 1920 1200
  , Color white $ Translate (-150) 400 $ Scale 0.4 0.4 $ Text "TUTORIAL"
  , desenharPaginaTutorial (paginaTutorial tutorial)
  , Color (greyN 0.6) $ Translate (-200) (-500) $ Scale 0.15 0.15 $ 
    Text "SETAS - Navegar | ESC - Voltar"
  ]

-- | Desenha conteúdo da página do tutorial
desenharPaginaTutorial :: Int -> Picture
desenharPaginaTutorial pagina = case pagina of
  0 -> Pictures
    [ Color white $ Translate (-400) 200 $ Scale 0.2 0.2 $ Text "BEM-VINDO AO WORMS!"
    , Color (greyN 0.8) $ Translate (-450) 100 $ Scale 0.15 0.15 $ 
      Text "Destrua as minhocas inimigas antes que destruam voce!"
    , Color (greyN 0.8) $ Translate (-400) 50 $ Scale 0.15 0.15 $ 
      Text "Use armas estrategicamente e destrua o terreno."
    ]
  1 -> Pictures
    [ Color white $ Translate (-250) 200 $ Scale 0.2 0.2 $ Text "CONTROLOS"
    , Color (greyN 0.8) $ Translate (-300) 100 $ Scale 0.15 0.15 $ 
      Text "WASD ou SETAS - Mover minhoca"
    , Color (greyN 0.8) $ Translate (-300) 50 $ Scale 0.15 0.15 $ 
      Text "1-5 - Selecionar arma"
    , Color (greyN 0.8) $ Translate (-300) 0 $ Scale 0.15 0.15 $ 
      Text "ESPACO - Disparar arma"
    , Color (greyN 0.8) $ Translate (-300) (-50) $ Scale 0.15 0.15 $ 
      Text "P - Pausar | ESC - Menu"
    ]
  _ -> Pictures
    [ Color white $ Translate (-150) 200 $ Scale 0.2 0.2 $ Text "ARMAS"
    , Color (greyN 0.8) $ Translate (-400) 100 $ Scale 0.15 0.15 $ 
      Text "BAZUCA - Explosao ao impacto"
    , Color (greyN 0.8) $ Translate (-400) 50 $ Scale 0.15 0.15 $ 
      Text "DINAMITE - Explosao com temporizador"
    , Color (greyN 0.8) $ Translate (-400) 0 $ Scale 0.15 0.15 $ 
      Text "MINA - Explode ao detetar inimigos"
    , Color (greyN 0.8) $ Translate (-400) (-50) $ Scale 0.15 0.15 $ 
      Text "ESCAVADORA - Destroi terra"
    , Color (greyN 0.8) $ Translate (-400) (-100) $ Scale 0.15 0.15 $ 
      Text "JETPACK - Movimento especial"
    ]

--------------------------------------------------------------------------------
-- * CÂMERA

-- | Aplica transformação da câmera ao conteúdo do jogo
aplicarCamera :: Camera -> Picture -> Picture
aplicarCamera cam conteudo = 
  let (x, y) = posCamera cam
      zoom = zoomCamera cam
  in Translate x y $ Scale zoom zoom conteudo

--------------------------------------------------------------------------------
-- * BACKGROUND

-- | Desenha o fundo do jogo
desenharBackground :: Assets -> Picture
desenharBackground assets =
  case gameBackground (backgroundAssets assets) of
    Just bg -> bg
    Nothing -> Color (makeColorI 135 206 235 255) $ rectangleSolid 1920 1200

--------------------------------------------------------------------------------
-- * MAPA

-- | Tamanho de cada bloco do mapa em pixels
tamanhoBloco :: Float
tamanhoBloco = 32.0

-- | Desenha o mapa completo
desenharMapa :: Mapa -> Picture
desenharMapa mapa = Pictures $ concat
  [ [ desenharBloco terreno (l, c)
    | (c, terreno) <- zip [0..] linha
    ]
  | (l, linha) <- zip [0..] mapa
  ]

-- | Desenha um bloco individual do mapa (COM AS TUAS IMAGENS!)
desenharBloco :: Terreno -> Posicao -> Picture
desenharBloco Ar _ = Blank  -- Ar é invisível
desenharBloco terreno (l, c) = 
  Translate x y $ Pictures
    [ corFundo terreno
    , textura terreno  -- Adiciona padrão visual
    ]
  where
    x = fromIntegral c * tamanhoBloco - 480
    y = -fromIntegral l * tamanhoBloco + 300
    
    -- Cor base do terreno
    corFundo Agua = Color (makeColorI 30 144 255 180) $ rectangleSolid tamanhoBloco tamanhoBloco
    corFundo Terra = Color (makeColorI 139 69 19 255) $ rectangleSolid tamanhoBloco tamanhoBloco
    corFundo Pedra = Color (makeColorI 105 105 105 255) $ rectangleSolid tamanhoBloco tamanhoBloco
    corFundo _ = Blank
    
    -- Textura/padrão para dar profundidade
    textura Agua = Color (makeColorI 255 255 255 40) $ 
                   Translate 0 (sin (fromIntegral c * 0.5) * 2) $ 
                   rectangleSolid (tamanhoBloco - 4) 2
    textura Terra = Pictures
      [ Color (makeColorI 101 51 10 100) $ Translate dx dy $ circleSolid 2
      | dx <- [-8, 0, 8], dy <- [-8, 0, 8]
      ]
    textura Pedra = Color (makeColorI 80 80 80 150) $ 
                    rectangleSolid (tamanhoBloco - 6) (tamanhoBloco - 6)
    textura _ = Blank

--------------------------------------------------------------------------------
-- * MINHOCAS (COM ANIMAÇÃO WALK!)

-- | Desenha todas as minhocas com animação
desenharMinhocasAnimadas :: Assets -> [Minhoca] -> Int -> Picture
desenharMinhocasAnimadas assets minhocas frame = Pictures
  [ desenharMinhocaAnimada assets i minhoca frame
  | (i, minhoca) <- zip [0..] minhocas
  , posicaoMinhoca minhoca /= Nothing
  ]

-- | Desenha uma minhoca individual com animação walk
desenharMinhocaAnimada :: Assets -> Int -> Minhoca -> Int -> Picture
desenharMinhocaAnimada assets numMinhoca minhoca frame =
  case posicaoMinhoca minhoca of
    Nothing -> Blank
    Just (l, c) ->
      let x = fromIntegral c * tamanhoBloco - 480
          y = -fromIntegral l * tamanhoBloco + 300
          sprite = escolherSpriteMinhocaAnimado assets numMinhoca (vidaMinhoca minhoca) frame
      in Translate x y $ Pictures
           [ sprite
           , desenharBarraVida (vidaMinhoca minhoca)
           ]

-- | Escolhe o sprite correto com animação (idle, walk1, walk2)
escolherSpriteMinhocaAnimado :: Assets -> Int -> VidaMinhoca -> Int -> Picture
escolherSpriteMinhocaAnimado assets numMinhoca vida frame
  | vida == Morta = Blank
  | even numMinhoca =  -- VERDE
      case frame of
        0 -> case minhocaVerdeIdle (spriteAssets assets) of
               Just sprite -> Scale 1.2 1.2 sprite
               Nothing -> fallbackVerde
        1 -> case minhocaVerdeWalk1 (spriteAssets assets) of
               Just sprite -> Scale 1.2 1.2 sprite
               Nothing -> fallbackVerde
        _ -> case minhocaVerdeWalk2 (spriteAssets assets) of
               Just sprite -> Scale 1.2 1.2 sprite
               Nothing -> fallbackVerde
  | otherwise =  -- AZUL
      case frame of
        0 -> case minhocaAzulIdle (spriteAssets assets) of
               Just sprite -> Scale 1.2 1.2 sprite
               Nothing -> fallbackAzul
        1 -> case minhocaAzulWalk1 (spriteAssets assets) of
               Just sprite -> Scale 1.2 1.2 sprite
               Nothing -> fallbackAzul
        _ -> case minhocaAzulWalk2 (spriteAssets assets) of
               Just sprite -> Scale 1.2 1.2 sprite
               Nothing -> fallbackAzul
  where
    fallbackVerde = Color (makeColorI 0 200 0 255) $ Pictures
      [ circleSolid 12, Translate 0 8 $ circleSolid 8
      , Color black $ Translate (-3) 10 $ circleSolid 2
      , Color black $ Translate 3 10 $ circleSolid 2
      ]
    fallbackAzul = Color (makeColorI 0 150 255 255) $ Pictures
      [ circleSolid 12, Translate 0 8 $ circleSolid 8
      , Color black $ Translate (-3) 10 $ circleSolid 2
      , Color black $ Translate 3 10 $ circleSolid 2
      ]

-- | Desenha barra de vida acima da minhoca
desenharBarraVida :: VidaMinhoca -> Picture
desenharBarraVida Morta = Blank
desenharBarraVida (Viva vida) = Translate 0 20 $ Pictures
  [ Color black $ rectangleSolid 32 6  -- Fundo
  , Color vermelho $ rectangleSolid largura 4  -- Vida
  ]
  where
    proporcao = fromIntegral vida / 100.0
    largura = 30 * proporcao
    vermelho = if vida > 50 
               then makeColorI 0 255 0 255  -- Verde
               else if vida > 25
                    then makeColorI 255 255 0 255  -- Amarelo
                    else makeColorI 255 0 0 255  -- Vermelho

--------------------------------------------------------------------------------
-- * OBJETOS

-- | Desenha todos os objetos (barris e disparos)
desenharObjetos :: Assets -> [Objeto] -> Picture
desenharObjetos assets objetos = Pictures
  [ desenharObjeto assets obj
  | obj <- objetos
  ]

-- | Desenha um objeto individual (USA AS TUAS IMAGENS!)
desenharObjeto :: Assets -> Objeto -> Picture
desenharObjeto assets (Barril (l, c) prestes) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300
      -- IMAGEM 1: Barril
      sprite = case barrilSprite (objetoAssets assets) of
        Just img -> Scale 1.0 1.0 img
        Nothing -> Color (makeColorI 139 69 19 255) $ Pictures
          [ rectangleSolid 20 28  -- Corpo
          , Color (makeColorI 80 40 10 255) $ 
            Translate 0 8 $ rectangleSolid 20 3  -- Aro
          , Color (makeColorI 80 40 10 255) $ 
            Translate 0 (-8) $ rectangleSolid 20 3  -- Aro
          ]
      aviso = if prestes 
              then Pictures
                [ Color (makeColorI 255 0 0 150) $ Circle 18
                , Color (makeColorI 255 255 0 150) $ Circle 22
                ]
              else Blank
  in Translate x y $ Pictures [sprite, aviso]

desenharObjeto assets (Disparo (l, c) dir tipo _ _) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300
      sprite = desenharDisparo assets tipo dir
  in Translate x y sprite

-- | Desenha sprite de um disparo (USA AS TUAS IMAGENS!)
desenharDisparo :: Assets -> TipoArma -> Direcao -> Picture
desenharDisparo assets Bazuca dir =
  let angulo = anguloParaDirecao dir
      -- IMAGEM 2: Bazuca
      sprite = case bazucaIcon (objetoAssets assets) of
        Just img -> Rotate angulo $ Scale 0.8 0.8 img
        Nothing -> Rotate angulo $ Pictures
          [ Color yellow $ rectangleSolid 16 6  -- Míssil
          , Color red $ Translate (-8) 0 $ circleSolid 4  -- Chama
          ]
  in sprite

desenharDisparo assets Mina _ =
  -- IMAGEM 9: Mina
  case minaIcon (objetoAssets assets) of
    Just img -> Scale 0.9 0.9 img
    Nothing -> Color black $ Pictures
      [ circleSolid 10
      , Color red $ circleSolid 6
      , Pictures [Rotate (45 * i) $ rectangleSolid 2 16 | i <- [0..3]]
      ]

desenharDisparo assets Dinamite _ =
  -- IMAGEM 3: Dinamite
  case dinamiteIcon (objetoAssets assets) of
    Just img -> Scale 0.9 0.9 img
    Nothing -> Pictures
      [ Color red $ rectangleSolid 8 24
      , Color orange $ Translate 0 12 $ rectangleSolid 10 4
      , Color yellow $ Translate 2 18 $ rectangleSolid 2 8  -- Pavio
      ]

desenharDisparo assets Escavadora dir =
  let angulo = anguloParaDirecao dir
      -- IMAGEM 4: Escavadora
      sprite = case escavadoraIcon (objetoAssets assets) of
        Just img -> Rotate angulo $ Scale 0.8 0.8 img
        Nothing -> Rotate angulo $ Color yellow $ Pictures
          [ rectangleSolid 20 12
          , Translate 8 0 $ rectangleSolid 8 8  -- Broca
          ]
  in sprite

desenharDisparo _ _ _ = Blank

-- | Converte direção em ângulo para rotação
anguloParaDirecao :: Direcao -> Float
anguloParaDirecao Norte = 90
anguloParaDirecao Sul = -90
anguloParaDirecao Este = 0
anguloParaDirecao Oeste = 180
anguloParaDirecao Nordeste = 45
anguloParaDirecao Sudeste = -45
anguloParaDirecao Noroeste = 135
anguloParaDirecao Sudoeste = -135

--------------------------------------------------------------------------------
-- * ANIMAÇÕES

-- | Desenha todas as animações ativas
desenharAnimacoes :: Assets -> [AnimacaoAtiva] -> Picture
desenharAnimacoes assets animacoes = Pictures
  [ desenharAnimacao assets anim
  | anim <- animacoes
  ]

-- | Desenha uma animação individual
desenharAnimacao :: Assets -> AnimacaoAtiva -> Picture
desenharAnimacao assets (AnimExplosao (l, c) _ frame) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300
      explosao = escolherFrameExplosao assets frame
  in Translate x y explosao

desenharAnimacao _ (AnimDano (l, c) dano tempo) =
  let x = fromIntegral c * tamanhoBloco - 480
      y = -fromIntegral l * tamanhoBloco + 300 + tempo * 20
      alpha = round (tempo * 255)
  in Translate x y $ 
     Color (makeColorI 255 0 0 alpha) $ 
     Scale 0.15 0.15 $ 
     Text ("-" ++ show dano)

desenharAnimacao _ (AnimMovimento _ _ _ _) = Blank

-- | Escolhe frame de explosão (USA AS TUAS IMAGENS 5-7!)
escolherFrameExplosao :: Assets -> Int -> Picture
escolherFrameExplosao assets frame
  | frame == 0 = 
      -- IMAGEM 5: Explosão inicial (grande)
      case explosao1 (objetoAssets assets) of
        Just img -> Scale 2.0 2.0 img
        Nothing -> Color red $ circleSolid 30
  | frame == 1 = 
      -- IMAGEM 6: Explosão média
      case explosao2 (objetoAssets assets) of
        Just img -> Scale 2.2 2.2 img
        Nothing -> Color orange $ circleSolid 40
  | otherwise = 
      -- IMAGEM 7: Explosão final (dissipando)
      case explosao3 (objetoAssets assets) of
        Just img -> Scale 2.0 2.0 img
        Nothing -> Color yellow $ circleSolid 30

--------------------------------------------------------------------------------
-- * UI (INTERFACE PARA DOIS JOGADORES!)

-- | Desenha UI dos dois jogadores lado a lado
desenharUIDoisJogadores :: Assets -> EstadoPartida -> Picture
desenharUIDoisJogadores assets partida = Pictures
  [ desenharUIJogador1 assets partida  -- Esquerda
  , desenharUIJogador2 assets partida  -- Direita
  , desenharModoJogo (modoPartida partida)  -- Centro topo
  , desenharControlesDoisJogadores  -- Centro baixo
  ]

-- | UI do Jogador 1 (Verde) - lado esquerdo
desenharUIJogador1 :: Assets -> EstadoPartida -> Picture
desenharUIJogador1 assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasVerdes = [(i, m) | (i, m) <- zip [0..] minhocas, even i]
  in case minhocasVerdes of
       ((_, minhoca):_) -> Translate (-750) 0 $ Pictures
         [ Color (makeColorI 0 0 0 180) $ rectangleSolid 350 1100  -- Fundo maior
         , Translate 0 520 $ Color (makeColorI 0 255 0 255) $ Scale 0.25 0.25 $ Text "JOGADOR 1"
         , Translate 0 480 $ Color (makeColorI 0 255 0 255) $ Scale 0.2 0.2 $ Text "VERDE"
         , desenharVidaJogador assets minhoca (0, 400)
         , desenharArmasJogador assets minhoca (armaSelecionadaP1 partida) (0, 150)
         ]
       [] -> Blank

-- | UI do Jogador 2 (Azul) - lado direito
desenharUIJogador2 :: Assets -> EstadoPartida -> Picture
desenharUIJogador2 assets partida =
  let minhocas = minhocasEstado (estadoWorms partida)
      minhocasAzuis = [(i, m) | (i, m) <- zip [0..] minhocas, odd i]
  in case minhocasAzuis of
       ((_, minhoca):_) -> Translate 750 0 $ Pictures
         [ Color (makeColorI 0 0 0 180) $ rectangleSolid 350 1100  -- Fundo maior
         , Translate 0 520 $ Color (makeColorI 0 150 255 255) $ Scale 0.25 0.25 $ Text "JOGADOR 2"
         , Translate 0 480 $ Color (makeColorI 0 150 255 255) $ Scale 0.2 0.2 $ Text "AZUL"
         , desenharVidaJogador assets minhoca (0, 400)
         , desenharArmasJogador assets minhoca (armaSelecionadaP2 partida) (0, 150)
         ]
       [] -> Blank

-- | Desenha vida de um jogador com ícone
desenharVidaJogador :: Assets -> Minhoca -> (Float, Float) -> Picture
desenharVidaJogador assets minhoca (x, y) = Translate x y $ Pictures
  [ iconeVida
  , Translate 60 0 $ desenharNumeroVida (vidaMinhoca minhoca)
  ]
  where
    iconeVida = case heartIcon (uiAssets assets) of
      Just img -> Translate (-80) 0 $ Scale 0.8 0.8 $ img
      Nothing -> Color red $ circleSolid 20

desenharNumeroVida :: VidaMinhoca -> Picture
desenharNumeroVida Morta = Color red $ Scale 0.3 0.3 $ Text "0 HP"
desenharNumeroVida (Viva v) = Color cor $ Scale 0.3 0.3 $ Text (show v ++ " HP")
  where
    cor | v > 70 = makeColorI 0 255 0 255
        | v > 30 = makeColorI 255 255 0 255
        | otherwise = makeColorI 255 0 0 255

-- | Desenha armas do jogador com slots verticais
desenharArmasJogador :: Assets -> Minhoca -> Maybe TipoArma -> (Float, Float) -> Picture
desenharArmasJogador assets minhoca armaSel (x, y) = Translate x y $ Pictures
  [ Translate 0 (-50) $ Color white $ Scale 0.15 0.15 $ Text "ARMAS"
  , desenharSlotArma assets Bazuca (bazucaMinhoca minhoca) (armaSel == Just Bazuca) (0, -120)
  , desenharSlotArma assets Dinamite (dinamiteMinhoca minhoca) (armaSel == Just Dinamite) (0, -220)
  , desenharSlotArma assets Mina (minaMinhoca minhoca) (armaSel == Just Mina) (0, -320)
  , desenharSlotArma assets Escavadora (escavadoraMinhoca minhoca) (armaSel == Just Escavadora) (0, -420)
  , desenharSlotArma assets Jetpack (jetpackMinhoca minhoca) (armaSel == Just Jetpack) (0, -520)
  ]

-- | Desenha um slot de arma: weapon_slot.png como fundo, ícone no centro, número embaixo
desenharSlotArma :: Assets -> TipoArma -> Int -> Bool -> (Float, Float) -> Picture
desenharSlotArma assets tipo qtd selecionada (x, y) = Translate x y $ Pictures
  [ fundoSlot  -- weapon_slot.png
  , molduraSelecionada  -- Moldura amarela se selecionada
  , iconeArma  -- Ícone da arma no centro
  , textoQuantidade  -- "x5" embaixo
  ]
  where
    -- Fundo: weapon_slot.png
    fundoSlot = case weaponSlot (uiAssets assets) of
      Just img -> Scale 1.2 1.2 $ img  -- Escala para ficar maior
      Nothing -> Color (greyN 0.3) $ rectangleSolid 90 90
    
    -- Moldura amarela se selecionada
    molduraSelecionada = if selecionada
                         then Color yellow $ rectangleWire 95 95
                         else Blank
    
    -- Ícone da arma no CENTRO do slot
    iconeArma = Scale 0.7 0.7 $ desenharIconeArma assets tipo
    
    -- Quantidade EMBAIXO do slot
    textoQuantidade = Translate 0 (-55) $ 
                      Color corTexto $ 
                      Scale 0.2 0.2 $ 
                      Text ("x" ++ show qtd)
    
    corTexto = if qtd > 0 then white else greyN 0.5

-- | Desenha ícone de uma arma
desenharIconeArma :: Assets -> TipoArma -> Picture
desenharIconeArma assets Bazuca =
  case bazucaIcon (objetoAssets assets) of
    Just img -> img
    Nothing -> Color yellow $ rectangleSolid 20 8
desenharIconeArma assets Dinamite =
  case dinamiteIcon (objetoAssets assets) of
    Just img -> img
    Nothing -> Color red $ rectangleSolid 10 24
desenharIconeArma assets Mina =
  case minaIcon (objetoAssets assets) of
    Just img -> img
    Nothing -> Color black $ circleSolid 12
desenharIconeArma assets Escavadora =
  case escavadoraIcon (objetoAssets assets) of
    Just img -> img
    Nothing -> Color yellow $ rectangleSolid 24 16
desenharIconeArma assets Jetpack =
  case jetpackIcon (objetoAssets assets) of
    Just img -> img
    Nothing -> Color cyan $ rectangleSolid 16 24

-- | Mostra o modo de jogo atual
desenharModoJogo :: ModoJogo -> Picture
desenharModoJogo modo = Translate 0 560 $ Pictures
  [ Color (makeColorI 0 0 0 150) $ rectangleSolid 300 50
  , Color white $ Scale 0.15 0.15 $ Text textoModo
  ]
  where
    textoModo = case modo of
      DoisJogadores -> "2 JOGADORES"
      VsBot -> "VS BOT"
      Treino -> "MODO TREINO"

-- | Mostra controles dos dois jogadores
desenharControlesDoisJogadores :: Picture
desenharControlesDoisJogadores = Translate 0 (-550) $ Pictures
  [ Color (makeColorI 0 0 0 150) $ rectangleSolid 500 80
  , Color (greyN 0.8) $ Scale 0.12 0.12 $ Text "P - Pausa | ESC - Menu"
  ]

--------------------------------------------------------------------------------
-- * PAUSA

-- | Desenha overlay de pausa
desenharPausaSeNecessario :: Bool -> Picture
desenharPausaSeNecessario False = Blank
desenharPausaSeNecessario True = Pictures
  [ Color (makeColorI 0 0 0 200) $ rectangleSolid 1920 1200  -- Overlay escuro
  , Color white $ Scale 0.5 0.5 $ Text "PAUSA"
  , Translate (-200) (-80) $ Color (greyN 0.8) $ Scale 0.2 0.2 $ Text "P - Continuar"
  , Translate (-200) (-120) $ Color (greyN 0.8) $ Scale 0.2 0.2 $ Text "ESC - Menu"
  ]