import Text.Printf
import System.Random
import System.IO.Unsafe  -- be careful!                                         


type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Color     = (Int,Int,Int)


-------------------------------------------------------------------------------
-- Retângulos e cores
-------------------------------------------------------------------------------

-- Lista de retângulos (constante)


listOfRects :: [Rect]
listOfRects = take 500 [ ((x,y),z,t) | x <- [unsafePerformIO (getStdRandom (randomR (0, 500))),unsafePerformIO (getStdRandom (randomR (0, 700)))], y <- [unsafePerformIO (getStdRandom (randomR (0, 25))),unsafePerformIO (getStdRandom (randomR (0, 300)))], z <- [unsafePerformIO (getStdRandom (randomR (0, 600))),unsafePerformIO (getStdRandom (randomR (0, 253)))], t <- [unsafePerformIO (getStdRandom (randomR (0, 55))),unsafePerformIO (getStdRandom (randomR (0, 400)))]]
   

-- Lista de cores (constante) 


listOfColors :: [Color]
listOfColors = take 500 [ (r,g,b) | r <-  [unsafePerformIO (getStdRandom (randomR (0, 255))), unsafePerformIO (getStdRandom (randomR (0, 255))), unsafePerformIO (getStdRandom (randomR (0, 255)))], g <- [unsafePerformIO (getStdRandom (randomR (0, 255))), unsafePerformIO (getStdRandom (randomR (0, 255))), unsafePerformIO (getStdRandom (randomR (0, 255)))], b <-  [unsafePerformIO (getStdRandom (randomR (0, 255))), unsafePerformIO (getStdRandom (randomR (0, 255))), unsafePerformIO (getStdRandom (randomR (0, 255)))] ]
 
-- Obs.: Criar listas "hard-coded" não é muito legal. Imagine uma lista com 100 retângulos: inviável!
-- Tem um jeito muito melhor de fazer isso: programando uma função geradora!!!!
-- Em Haskell, podemos gerar listas facilmente usando list comprehension
-- https://wiki.haskell.org/List_comprehension
--http://zvon.org/other/haskell/Outputsyntax/listQcomprehension_reference.html


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensoes do retângulo e uma string com atributos de estilo
svgRect :: Rect -> Color -> String 
svgRect ((x,y),w,h) color = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style
  where style = svgStyle color
  
-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: Color -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b


-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
    
  writeFile "rects.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = concat (zipWith svgRect listOfRects listOfColors)
        (w,h) = (1500,500) -- width, height da imagem SVG


