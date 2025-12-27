import Graphics.Gloss

main :: IO ()
main= display window white ex4
-- desnhar em fundo branco

window:: Display
window = InWindow "Gloss" (800,600) (0,0)
-- numa janela cm 800x600 pixeels

ex1::Picture
ex1 = circleSolid 100
--um círculo cheio com 100 pixels de raio

{-
circleSolid :: Float-> Picture -- círculo dado o raio
rectangleSolid :: Float-> Float-> Picture -- rectangulo dada largura, altura
line :: Path-> Picture --linha poligonal
polygon :: Path-> Picture -- polígono cheio
type Path = [Point] --percurso
type Point = (Float,Float) -- coordenada x,y

color :: Color-> Picture-> Picture
Color: red, green, blue, yellow, cyan, magenta, ...

Podemos sobrepor várias figuras numa só:
pictures :: [Picture]-> Picture
-}

ex2 = pictures [color red (circleSolid 100), color white (rectangleSolid 100 50)]

{-
Por omissão as figuras são desenhadas na origem (coordenadas (0,0)).

Para desenhar noutro ponto basta fazer uma translação:
translate :: Float-> Float-> Picture-> Picture —translação por dx, dy

Também podemos fazer rotações por um ângulo (em graus):
rotate :: Float-> Picture-> Picture
-}

ex3 = pictures [translate 100 100 (circleSolid 50), rotate 45 (rectangleSolid 100 50)]

{- 
Podemos também ampliar ou reduzir figuras.
scale :: Float-> Float-> Picture-> Picture —mudar aescala dados factores x, y
-}

ex4 = pictures [color blue(circleSolid 50), color green (translate 0 100 (scale 1 0.5 (circleSolid 50)))]

{-
—posição e velocidade
type Ball = (Point, Vector)
—definidos na biblioteca Gloss
type Point = (Float,Float)
type Vector = (Float,Float)
-}
