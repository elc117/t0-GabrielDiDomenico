-- Autor: Gabriel Di Domenico
-- Data: 15/04/2020
import Data.List
import Data.Char
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


auxHex2Dec :: String -> Int -> Int
auxHex2Dec "0" 0= 0
auxHex2Dec "1" 0= 1
auxHex2Dec "2" 0= 2
auxHex2Dec "3" 0= 3
auxHex2Dec "4" 0= 4
auxHex2Dec "5" 0= 5
auxHex2Dec "6" 0= 6
auxHex2Dec "7" 0= 7
auxHex2Dec "8" 0= 8
auxHex2Dec "9" 0= 9
auxHex2Dec "A" 0= 10
auxHex2Dec "B" 0= 11
auxHex2Dec "C" 0= 12
auxHex2Dec "D" 0= 13
auxHex2Dec "E" 0= 14
auxHex2Dec "F" 0= 15
auxHex2Dec list exp = (digitToInt (head list))*16^exp + auxHex2Dec (tail list) (exp-1)

hex2dec :: String -> Int
hex2dec "" = undefined
hex2dec str = auxHex2Dec str ((length str)-1)

charToString :: Char -> String
charToString c = [c]

auxDec2Hex :: Int -> String
auxDec2Hex 0 = "0"
auxDec2Hex 1 = "1"
auxDec2Hex 2 = "2"
auxDec2Hex 3 = "3"
auxDec2Hex 4 = "4"
auxDec2Hex 5 = "5"
auxDec2Hex 6 = "6"
auxDec2Hex 7 = "7"
auxDec2Hex 8 = "8"
auxDec2Hex 9 = "9"
auxDec2Hex 10 = "A"
auxDec2Hex 11 = "B"
auxDec2Hex 12 = "C"
auxDec2Hex 13 = "D"
auxDec2Hex 14 = "E"
auxDec2Hex 15 = "F"
auxDec2Hex num
    | (num `mod` 16)<10 = (charToString(intToDigit(num `mod` 16)))++auxDec2Hex (num `div` 16) 
    | (num `mod` 16) == 10 = "A"++auxDec2Hex (num `div` 16)
    | (num `mod` 16) == 11 = "B"++auxDec2Hex (num `div` 16)
    | (num `mod` 16) == 12 = "C"++auxDec2Hex (num `div` 16)
    | (num `mod` 16) == 13 = "D"++auxDec2Hex (num `div` 16)
    | (num `mod` 16) == 14 = "E"++auxDec2Hex (num `div` 16)
    | (num `mod` 16) == 15 = "F"++auxDec2Hex (num `div` 16)

dec2hex :: Int -> String
dec2hex 0 = "0"
dec2hex 1 = "1"
dec2hex 2 = "2"
dec2hex 3 = "3"
dec2hex 4 = "4"
dec2hex 5 = "5"
dec2hex 6 = "6"
dec2hex 7 = "7"
dec2hex 8 = "8"
dec2hex 9 = "9"
dec2hex 10 = "A"
dec2hex 11 = "B"
dec2hex 12 = "C"
dec2hex 13 = "D"
dec2hex 14 = "E"
dec2hex 15 = "F"
dec2hex num = reverse (auxDec2Hex num)