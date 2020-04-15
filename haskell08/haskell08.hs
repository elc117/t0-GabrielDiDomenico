-- Autor: Gabriel Di Domenico
-- Data: 15/04/2020
import Data.List
-- 1. Escreva uma função recursiva isBin :: String -> Bool para verificar se uma dada String representa um número binário, ou seja, contém apenas caracteres '0' ou '1'.
-- As únicas funções pré-definidas autorizadas aqui são head e tail.

checaNum :: Char -> Bool
checaNum num
    | num == '1' || num == '0' = False
    | otherwise = True

isBin :: String -> Bool
isBin "1" = True
isBin "0" = True
isBin "" = False
isBin str
    | checaNum(head str) = False
    | otherwise          = isBin (tail str)

-- 2. Reescreva a função acima de forma não-recursiva. Dê outro nome para ela, por exemplo isBin'. Aqui você pode usar quaisquer funções auxiliares pré-definidas em Haskell.

isBin' :: String -> Bool
isBin' str = if length(filter (\x -> x=='1' || x=='0') str) == length str && str /= "" then True else False

-- 3. Encontra-se abaixo a definição parcial da função bin2dec :: [Int] -> Int, que converte uma lista de 0's e 1's (representando um número binário), em seu equivalente em decimal.
--
--      bin2dec :: [Int] -> Int
--      bin2dec [] = undefined
--      bin2dec bits = auxBin2Dec bits ((length bits)-1)
--
-- Observe que:
--
--  Usou-se undefined para o caso em que a função não tem resultado definido.
--  Usou-se uma função auxiliar (auxBin2Dec) que recebe, como segundo argumento, o expoente que deverá multiplicar o primeiro elemento da lista.
--
-- Implemente a função auxBin2Dec de forma recursiva, para que bin2dec funcione corretamente.

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec [0] 0= 0
auxBin2Dec [1] 0= 1
auxBin2Dec list exp
    | head list == 1 = round(2^exp) + auxBin2Dec(tail list) (exp-1)
    | otherwise      = 0 + auxBin2Dec(tail list) (exp-1)

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

-- 4. Reescreva a função do exercício anterior de forma não-recursiva, usando funções pré-definidas em Haskell. Dê outro nome para a função (por exemplo, bin2dec').

auxBin2Dec' :: [Int] -> Int
auxBin2Dec' list = sum (map (\x -> 2^x) (elemIndices 1 (reverse list)))

bin2dec' :: [Int] -> Int
bin2dec' [] = undefined
bin2dec' bits = auxBin2Dec' bits

-- 5. Crie uma função recursiva dec2bin :: Int -> [Int] que receba um número inteiro positivo e retorne sua representação em binário, sob forma de uma lista de 0's e 1's.
-- As funções auxiliares autorizadas aqui são mod, div e reverse.

auxDec2Bin :: Int -> [Int]
auxDec2Bin 1 = [1]
auxDec2Bin num = (num `mod` 2) : auxDec2Bin (num `div` 2) 

dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin 1 = [1]
dec2bin num = reverse (auxDec2Bin num)

-- 6. Implemente uma dessas funções: isHex :: String -> Bool ou hex2dec :: String -> Int ou dec2hex :: Int -> String, que são semelhantes às dos exercícios anteriores,
-- porém com números hexadecimais no lugar de números binários. Aqui está tudo liberado: você pode escolher qual das funções irá implementar, sem restrições sobre como 
-- deve fazer isso.

checaNumHex :: Char -> Bool
checaNumHex num
    | elem num "0123456789ABCDEF" = False
    | otherwise = True

isHex :: String -> Bool
isHex "0" = True
isHex "1" = True
isHex "2" = True
isHex "3" = True
isHex "4" = True
isHex "5" = True
isHex "6" = True
isHex "7" = True
isHex "8" = True
isHex "9" = True
isHex "A" = True
isHex "B" = True
isHex "C" = True
isHex "D" = True
isHex "E" = True
isHex "F" = True
isHex ""  = False
isHex str
    | checaNumHex(head str) = False
    | otherwise          = isHex (tail str)
