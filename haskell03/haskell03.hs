-- Prática do dia 25/03/2020 da Disciplina de Paradigmas
-- Autor: Gabriel Di Domenico

import Data.Char
import Data.List

-- 1. Crie uma função isVowel :: Char -> Bool que verifique se um caracter é uma vogal ou não.
isVowel :: Char -> Bool
isVowel x = if x=='a' || x=='A' || 
               x=='e' || x=='E' || 
               x=='i' || x=='I' || 
               x=='o' || x=='O' || 
               x=='u' || x=='U' 
            then True
            else False

-- 2. Escreva uma função addComma, que adicione uma vírgula no final de cada string contida numa lista.
addComma :: [String] -> [String]
addComma z = map (++",") z

-- 3.Crie uma função htmlListItems :: [String] -> [String], que receba uma lista de strings
-- e retorne outra lista contendo as strings formatadas como itens de lista em HTML.
-- Resolva este exercício COM e SEM funções anônimas (lambda).

putListString :: String -> String
putListString y = "<LI>"++y++"<LI>"

htmlListItems :: [String] -> [String]
htmlListItems t = map putListString t
--htmlListItems y = map (\y -> "<LI>"++y++"<LI>") y

-- 4. Defina uma função que receba uma string e produza outra retirando as vogais, conforme os exemplos abaixo.
-- Resolva este exercício COM e SEM funções anônimas (lambda).
isNotVowel :: Char -> Bool
isNotVowel q = if q/='a' && q/='A' && 
               q/='e' && q/='E' && 
               q/='i' && q/='I' && 
               q/='o' && q/='O' && 
               q/='u' && q/='U' 
            then True
            else False
removeVowels :: String -> String
--removeVowels e = filter isNotVowel e 
removeVowels e = filter (\e -> e/='a' && e/='A' && 
                               e/='e' && e/='E' && 
                               e/='i' && e/='I' && 
                               e/='o' && e/='O' && 
                               e/='u' && e/='U') e 

-- 5. Defina uma função que receba uma string, possivelmente contendo espaços, e que retorne outra string
-- substituindoos demais caracteres por '-', mas mantendo os espaços. Resolva este exercício COM e SEM
-- funções anônimas (lambda).

changeChar :: Char -> Char
changeChar x = if x /= ' ' then '-' else ' '

codify :: [Char] -> [Char]
codify x =  map changeChar x
-- codify x = map (\x -> if x /= ' ' then '-' else ' ') x

-- 6. Escreva uma função firstName :: String -> String que, dado o nome completo de uma pessoa, obtenha seu 
-- primeiro nome. Suponha que cada parte do nome seja separada por um espaço e que não existam espaços no
-- início ou fim do nome. Dica: estude funções pré-definidas em Haskell (List operations -> Sublists) em
-- http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#g:18. 

firstName :: String -> String
firstName x =  takeWhile (\x -> x /= ' ') x

-- 7. Escreva uma função isInt :: String -> Bool que verifique se uma dada string só contém dígitos de 0 a 9.

filterLettersAndSpace :: String -> String
filterLettersAndSpace x = filter (\x -> x/='0' && x/='1' && 
                                        x/='2' && x/='3' && 
                                        x/='4' && x/='5' && 
                                        x/='6' && x/='7' && 
                                        x/='8' && x/='9') x 

isInt :: String -> Bool
isInt x = if length(filterLettersAndSpace x) < 1  then True else False

-- 8. Escreva uma função lastName :: String -> String que, dado o nome completo de uma pessoa, obtenha seu
-- último sobrenome. Suponha que cada parte do nome seja separada por um espaço e que não existam espaços no
-- início ou fim do nome.

lastName :: String -> String
lastName x = last (words x)

-- 9. Escreva uma função userName :: String -> String que, dado o nome completo de uma pessoa, crie um nome
-- de usuário (login) da pessoa, formado por: primeira letra do nome seguida do sobrenome, tudo em minúsculas.
-- Dica: estude as funções pré-definidas no módulo Data.Char, para manipulação de maiúsculas e minúsculas.
-- Você precisará carregar este módulo usando import Data.Char no interpretador ou no início do arquivo do
-- programa

userName :: String -> String
userName x = map toLower ((take 1 x) ++ (lastName x))

-- 10. Escreva uma função encodeName :: String -> String que substitua vogais em uma string, conforme o esquema
-- a seguir: a = 4, e = 3, i = 2, o = 1, u = 0.

encodeNameAux :: Char -> Char
encodeNameAux char
    | char == 'a' = '4'
    | char == 'e' = '3'
    | char == 'i' = '2'
    | char == 'o' = '1'
    | char == 'u' = '0'
    | char == 'A' = '4'
    | char == 'E' = '3'
    | char == 'I' = '2'
    | char == 'O' = '1'
    | char == 'U' = '0'
    | otherwise = char

encodeName :: String -> String
encodeName x = map encodeNameAux x

-- 11. Escreva uma função betterEncodeName :: String -> String que substitua vogais em uma string, conforme este
-- esquema: a = 4, e = 3, i = 1, o = 0, u = 00.

betterSwitcherOfStuff :: Char -> String
betterSwitcherOfStuff c
    | c == 'A' = "4"
    | c == 'E' = "3"
    | c == 'I' = "1"
    | c == 'O' = "0"
    | c == 'U' = "00"
    | c == 'a' = "4"
    | c == 'e' = "3"
    | c == 'i' = "1"
    | c == 'o' = "0"
    | c == 'u' = "00"
    | otherwise = [c]

betterEncodeName :: String -> String
betterEncodeName string = concatMap (\c -> betterSwitcherOfStuff (c)) string

-- 12. Dada uma lista de strings, produzir outra lista com strings de 10 caracteres, usando o seguinte esquema:
-- strings de entrada com mais de 10 caracteres são truncadas, strings com até 10 caracteres são completadas com
-- '.' até ficarem com 10 caracteres.

addDot :: String -> String
addDot x
    | (length x) == 9 = x++"."
    | otherwise = addDot (x++".")

transformLists :: [String] -> [String]
transformLists x = map (\x -> if(length x > 9) then take 10 x else addDot x) x

