-- Autor: Gabriel Di Domenico
-- Data: 01/04/2020
--
import Data.List
-- 1. Crie uma função anoIdade :: Int -> (Int, Int) que, dado o ano de nascimento de uma pessoa, retorne uma tupla contendo o próprio ano de nascimento e a idade da pessoa no ano atual.
--
anoIdade :: Int -> (Int, Int)
anoIdade x = (x, 2020-x)
--
-- 2. Crie uma função selectName :: (String,Int,Int) -> String que receba uma tupla contendo nome, ano de nascimento e idade de uma pessoa, e retorne seu nome.
--
selectName :: (String,Int,Int) -> String
selectName (x, y, n) = x
--
-- 3. Crie uma funç� allNames :: [(String,Int,Int)] -> [String] que receba uma lista com dados de pessoas (nome, ano de nascimento e idade) e retorne uma lista conte
-- ndo todos os nomes. Resolva este exercício usando uma função de alta ordem e a função selectName.
--
allNames :: [(String,Int,Int)] -> [String]
allNames list = 
           map selectName list
--allNames list = map (\(x,y,z)-> x) list
--
-- 4. Resolva o exercício anterior usando uma função anônima que substitua a funselectName
--
-- 5. Crie uma função distance :: (Float, Float) -> (Float, Float) -> Float que receba as coordenadas de 2 pontos num espaço bidimensional e calcule a distância entre eles usando o teorema de pitágoras.
--
distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) =  sqrt(((x2-x1)**2)+((y2-y1)**2)) 
--
-- 6.Você tem uma lista de tuplas contendo, cada uma, um nome de empresa e a URL de seu site web. Escreva uma função que receba a lista de tuplas e retorne uma lista de links em HTML,
--
htmlLists :: [(String, String)] -> [String]
htmlLists x = map (\x -> "<a href=\""++(snd x)++"\">"++(fst x)++"</a>") x
--
-- 7. Suponha que uma tupla (Float, Float, Float, Float) represente um retângulo (x,y,w,h) com vértice superior esquerdo posicionado no ponto x,y e com largura e altura dados por w e h. Considerando isso, crie uma função que receba uma lista de retângulos e retorne outra lista ([Float]) com a área de cada um. Use uma função auxiliar (com nome) para calcular a área de um retângulo.
--
calculaArea :: (Float, Float, Float, Float) -> Float
calculaArea (x,y,w,h) = (w*h)/2

calculaAreaLista :: [(Float, Float, Float, Float)] -> [(Float)]
--calculaAreaLista list = map calculaArea list
calculaAreaLista list = map (\(x,y,w,h) -> (w*h)/2) list 

-- 9. O módulo Data.List do Haskell tem várias funções muito úteis para manipular listas. Para usar as funções deste módulo, é preciso importá-lo no início do programa ou no interpretador interativo (import Data.List). Neste exercício, você irá testar a função sort, disponível neste módulo. Para isso, crie as funções testSort1 :: [Int] -> [Int] e testSort2 :: [String] -> [String], aplicando a função sort respectivamente a uma lista de inteiros e a uma lista de strings.
--
