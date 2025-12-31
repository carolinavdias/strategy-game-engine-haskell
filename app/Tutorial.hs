{-|
Module      : Tutorial
Description : Sistema de tutorial com múltiplas páginas
Copyright   : Carolina Dias e Leonor Sousa, 2025
License     : GPL-3

Este módulo implementa o tutorial interativo do jogo.

Funcionalidades:

* 5 páginas de instruções visuais
* Navegação horizontal e vertical
* Indicador visual de progresso
* Atalhos numéricos para acesso direto

Permite que novos jogadores aprendam
as mecânicas sem recorrer a documentação externa.

-}

module Tutorial where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import EstadoJogo
import Assets

--------------------------------------------------------------------------------
-- * CONSTANTES

totalPaginas :: Int
totalPaginas = 5

--------------------------------------------------------------------------------
-- * DESENHO DO TUTORIAL

-- | Desenha a tela do tutorial
desenharTutorialCompleto :: Assets -> EstadoTutorial -> Picture
desenharTutorialCompleto assets estado = Pictures
  [ desenharBackgroundTutorial assets
  , desenharPaginaAtual assets (paginaTutorial estado)
  , desenharIndicadorPaginas (paginaTutorial estado)
  , desenharBotoesNavegacao assets (paginaTutorial estado) (menuSelecionado estado)

  ]

-- | Background escuro semitransparente
desenharBackgroundTutorial :: Assets -> Picture
desenharBackgroundTutorial assets =
  case menuBackground (menuAssets assets) of
    Just bg -> Pictures [bg, Color (makeColorI 0 0 0 150) $ rectangleSolid 1920 1200]
    Nothing -> Color (makeColorI 20 20 60 255) $ rectangleSolid 1920 1200

-- | Desenha a página atual do tutorial
desenharPaginaAtual :: Assets -> Int -> Picture
desenharPaginaAtual assets pagina =
  let imagemTutorial = obterImagemPagina assets pagina
  in Translate 0 50 $ case imagemTutorial of
       Just img -> Scale 1.0 1.0 $ img
       Nothing -> desenharPaginaFallback pagina

-- | Página fallback caso a imagem não carregue
desenharPaginaFallback :: Int -> Picture
desenharPaginaFallback pagina = Pictures
  [ Color (greyN 0.3) $ rectangleSolid 1200 700
  , Color white $ Translate (-200) 0 $ Scale 0.3 0.3 $ Text ("Página " ++ show (pagina + 1))
  , Color (greyN 0.7) $ Translate (-400) (-100) $ Scale 0.15 0.15 $ Text "Imagem do tutorial aqui"
  ]

-- | Obtém a imagem da página do tutorial
obterImagemPagina :: Assets -> Int -> Maybe Picture
obterImagemPagina assets 0 = tutorialPagina1 (tutorialAssets assets)
obterImagemPagina assets 1 = tutorialPagina2 (tutorialAssets assets)
obterImagemPagina assets 2 = tutorialPagina3 (tutorialAssets assets)
obterImagemPagina assets 3 = tutorialPagina4 (tutorialAssets assets)
obterImagemPagina assets 4 = tutorialPagina5 (tutorialAssets assets)
obterImagemPagina _ _ = Nothing

-- | Indicador visual de páginas (bolinhas)
desenharIndicadorPaginas :: Int -> Picture
desenharIndicadorPaginas paginaAtual = 
  Translate 0 (-480) $ Pictures
    [ desenharBolinha i (i == paginaAtual)
    | i <- [0..totalPaginas-1]
    ]
  where
    desenharBolinha idx ativo =
      let posX = fromIntegral (idx - 2) * 50  -- Centraliza as 5 bolinhas
          cor = if ativo then white else greyN 0.5
          tamanho = if ativo then 12 else 8
      in Translate posX 0 $ Color cor $ circleSolid tamanho

-- | Botões de navegação
desenharBotoesNavegacao :: Assets -> Int -> Bool -> Picture
desenharBotoesNavegacao assets paginaAtual menuSel = Pictures
  [ desenharBotaoAnterior assets paginaAtual
  , desenharBotaoProximo assets paginaAtual
  , desenharBotaoMenu assets menuSel
  ]

-- | Botão ANTERIOR (seta esquerda)
desenharBotaoAnterior :: Assets -> Int -> Picture
desenharBotaoAnterior assets paginaAtual =
  if paginaAtual == 0
  then Blank  -- Não mostra na primeira página
  else Translate (-600) (-450) $ 
       case buttonBack (menuAssets assets) of
         Just img -> Scale 1.0 1.0  $ img  -- Roda para apontar esquerda
         Nothing -> Pictures
           [ Color (makeColorI 70 130 180 255) $ circleSolid 40
           , Color white $ Translate (-15) (-10) $ Scale 0.25 0.25 $ Text "<"
           ]

-- | Botão PRÓXIMO (seta direita)
desenharBotaoProximo :: Assets -> Int -> Picture
desenharBotaoProximo assets paginaAtual =
  if paginaAtual == totalPaginas - 1
  then Blank  -- Não mostra na última página
  else Translate 600 (-450) $ 
       case buttonBack (menuAssets assets) of
         Just img -> Scale 1.0 1.0 $ Rotate 180 $ img
         Nothing -> Pictures
           [ Color (makeColorI 70 130 180 255) $ circleSolid 40
           , Color white $ Translate (-10) (-10) $ Scale 0.25 0.25 $ Text ">"
           ]

-- | Botão MENU (navegável com setas)
desenharBotaoMenu :: Assets -> Bool -> Picture
desenharBotaoMenu assets selecionado =
  Translate (-750) 480 $
  let escala = if selecionado then 1.5 else 1.2
      brilho = if selecionado 
               then Color (makeColorI 255 255 100 200) $ rectangleSolid 200 80
               else Blank
  in Pictures
       [ brilho  -- Destaque quando selecionado
       , case buttonMenu (uiAssets assets) of
           Just img -> Scale escala escala $ img
           Nothing -> Pictures
             [ Color (makeColorI 200 50 50 255) $ rectangleSolid 120 50
             , Color white $ Translate (-40) (-10) $ Scale 0.2 0.2 $ Text "MENU"
             ]
       ]


--------------------------------------------------------------------------------
-- * EVENTOS DO TUTORIAL

-- | Processa eventos no tutorial
eventoTutorialCompleto :: Event -> EstadoTutorial -> EstadoJogo
eventoTutorialCompleto evento estado = case evento of
  -- Seta para CIMA - Seleciona botão Menu
  EventKey (SpecialKey KeyUp) Down _ _ ->
    Tutorial (estado { menuSelecionado = True })
  
  -- Seta para BAIXO - Desseleciona botão Menu (volta para navegação)
  EventKey (SpecialKey KeyDown) Down _ _ ->
    Tutorial (estado { menuSelecionado = False })
  
  -- ENTER - Executa ação baseada na seleção
  EventKey (SpecialKey KeyEnter) Down _ _ ->
    if menuSelecionado estado
    then Menu (EstadoMenu OpcaoPlay 0.0)  -- Volta ao menu
    else if paginaTutorial estado < totalPaginas - 1
         then Tutorial (estado { paginaTutorial = paginaTutorial estado + 1 })  -- Próxima página
         else Menu (EstadoMenu OpcaoPlay 0.0)  -- Última página, volta ao menu
  
  -- Atalho X - Volta direto ao menu
  EventKey (Char 'x') Down _ _ -> Menu (EstadoMenu OpcaoPlay 0.0)
  EventKey (Char 'X') Down _ _ -> Menu (EstadoMenu OpcaoPlay 0.0)
  EventKey (SpecialKey KeyEsc) Down _ _ -> Menu (EstadoMenu OpcaoPlay 0.0)
  
  -- Página anterior (só se menu não estiver selecionado)
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    if not (menuSelecionado estado) && paginaTutorial estado > 0
    then Tutorial (estado { paginaTutorial = paginaTutorial estado - 1, menuSelecionado = False })
    else Tutorial estado
  
  -- Próxima página (só se menu não estiver selecionado)
  EventKey (SpecialKey KeyRight) Down _ _ ->
    if not (menuSelecionado estado) && paginaTutorial estado < totalPaginas - 1
    then Tutorial (estado { paginaTutorial = paginaTutorial estado + 1, menuSelecionado = False })
    else Tutorial estado
  
  -- Atalhos numéricos (1-5 para ir direto à página)
  EventKey (Char '1') Down _ _ -> Tutorial (estado { paginaTutorial = 0, menuSelecionado = False })
  EventKey (Char '2') Down _ _ -> Tutorial (estado { paginaTutorial = 1, menuSelecionado = False })
  EventKey (Char '3') Down _ _ -> Tutorial (estado { paginaTutorial = 2, menuSelecionado = False })
  EventKey (Char '4') Down _ _ -> Tutorial (estado { paginaTutorial = 3, menuSelecionado = False })
  EventKey (Char '5') Down _ _ -> Tutorial (estado { paginaTutorial = 4, menuSelecionado = False })
  
  -- Ignora outros eventos
  _ -> Tutorial estado

--------------------------------------------------------------------------------
-- * ATUALIZAÇÃO (animações futuras)

-- | Atualiza estado do tutorial (para animações)
atualizarTutorial :: Float -> EstadoTutorial -> EstadoJogo
atualizarTutorial dt estado = 
  Tutorial estado  -- Sem animações por agora