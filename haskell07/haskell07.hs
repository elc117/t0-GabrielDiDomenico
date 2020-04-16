-- Autor: Gabriel Di Domenico
-- Data: 15/04/2020

-- 1. Escreva uma função ends :: [Int] -> [Int] que receba uma lista e retorne outra lista contendo o primeiro e o último elementos da primeira lista. Crie 2 versões dessa função:
-- uma usando o operador de construção de lista (:) e outra usando apenas a notação de lista entre colchetes.
ends :: [Int] -> [Int]
ends list = head list : [last list]
--ends list = [head list, last list]

-- 2. Reescreva a função deduzame do exercício 4 (acima) usando a notação (x:xs) para representar a lista lst. Ajuste o restante do código da função. Você verá que o código ficará
-- mais enxuto, mais legível.

deduzame :: [Int] -> [Int]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame (xs)

-- 3. Reescreva também a função deduzame2 do exercício 5, usando a notação (x:xs) para representar a lista lst.

deduzame2 :: [Int] -> [Int]
deduzame2 [] = []
deduzame2 (x:xs) = if (x) > 2
  then x : deduzame2 (xs) 
  else deduzame2 (xs)

-- 4. Usando recursão, escreva uma função geraTabela :: Int -> [(Int,Int)] que produza uma lista com n tuplas, cada tupla com números de n a 1 e seus respectivos quadrados. 

geraTabela :: Int -> [(Int,Int)]
geraTabela 1 = [(1,1)]
geraTabela 0 = []
geraTabela num = (num,num*num) : geraTabela (num-1)

-- 5. Defina uma função recursiva que verifique se um dado caracter está contido numa string.

contido :: Char -> String -> Bool
contido char [] = False
contido char str 
    | (head str) == char = True
    | otherwise          = contido char (tail str)

-- 6. Defina uma função recursiva que receba uma lista de coordenadas de pontos 2D e desloque esses pontos em 2 unidades.

elevate2 :: (Float,Float) -> (Float,Float)
elevate2 (x,y) = (x+2,y+2)

translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate list = (elevate2 (head list)) : translate(tail list)

-- 7. Defina uma função recursiva que receba uma lista de palavras e retorne a quantidade dessas palavras que tenham mais de 5 caracteres.

countLongs :: [String] ->  Int
countLongs [] = 0
countLongs list
    | length (head list)>5 = 1 + countLongs (tail list) 
    | otherwise            = 0 + countLongs (tail list)

