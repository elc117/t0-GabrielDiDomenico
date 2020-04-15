-- Prática do dia 18/03/2020 da Disciplina de Paradigmas
-- Autor: Gabriel Di Domenico

--Exercícios 3.1

-- 1. Crie uma função sumSquares :: Int -> Int -> Int
-- que receba dois números x e y e calcula a soma dos seus quadrados.
sumSquares :: Int -> Int -> Int
sumSquares x y = (x^2) + (y^2)

-- 2. Defina uma função calcExprInt :: Int -> Int que receba um número n e calcule 3*n^2 + 1.
calcExprInt :: Int -> Int
calcExprInt z = (3*(z^2)) + 1

-- 3. Crie uma função isNegative :: Int -> Bool que receba um número n e vefique se ele é negativo.
isNegative :: Int -> Bool
isNegative t = if t >= 0 then False else True

-- 4. Crie uma função calcExprFloat :: Float -> Float que receba um número n e calcule n^2 + n/2 + 1.
calcExprFloat :: Float -> Float
calcExprFloat x = ((x^2) + (x/2)) + 1

-- 5. Defina uma função addPrefix :: String -> String que receba um nome (tipo String) e adicione a string
-- "Mr. " no início do nome. Use o operador ++ para concatenar strings (ou qualquer lista do mesmo tipo).
addPrefix :: String -> String
addPrefix x = "Mr. " ++x

-- 6. Crie uma função addThisPrefix :: String -> String -> String que receba duas strings e concatene-as.
addThisPrefix :: String -> String -> String
addThisPrefix x y = x++y

-- 7. Crie uma função startsWithA :: String -> Bool que receba uma string e verifique se ela inicia
-- com o caracter 'A'.
startsWithA :: String -> Bool
startsWithA x = if take 1 x == ['A'] then True else False

-- 8. Defina uma função isVerb :: String -> Bool que receba uma string e verifique se ela termina com
-- o caracter 'r'. Antes desse exercício, teste no interpretador a função pré-definida last, que retorna
-- o último elemento de uma lista. Dica: conheça também o list monster, do autor Miran Lipovača :-)
isVerb :: String -> Bool
isVerb x = if last x=='r' then True else False

-- 9. Crie uma função isVowel :: Char -> Bool que receba um caracter e verifique se ele é uma vogal minúscula.
isVowel :: Char -> Bool
isVowel x = if x=='a' || x=='e' || x=='i' || x=='o' || x=='u' then True else False

-- 10. Crie uma função hasEqHeads :: [Int] -> [Int] -> Bool que verifique se 2 listas possuem o mesmo primeiro
-- elemento. Use a função head e o operador lógico == para verificar igualdade.
hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads x y = if head x== head y then True else False

-- 12 Use a função elem para criar uma função isVowel2 :: Char -> Bool que verifique se um caracter é uma vogal,
-- tanto maiúscula como minúscula.
isVowel2 :: Char -> Bool
isVowel2 x = elem 'A' [x] || elem 'a' [x] ||
             elem 'E' [x] || elem 'e' [x] ||
             elem 'I' [x] || elem 'i' [x] ||
             elem 'O' [x] || elem 'o' [x] ||
             elem 'U' [x] || elem 'u' [x]

--Exercícios 3.2

-- 1. Crie uma função testAddPrefix :: [String] -> [String] que receba uma lista de nomes e aplique a função
-- addPrefix em cada nome.
testAddPrefix :: [String] -> [String]
testAddPrefix x = map addPrefix x

-- 2. Crie uma função onlyVowels :: String -> String que receba uma string e retorne outra somente contendo
-- suas vogais. Por exemplo: onlyVowels "abracadabra" vai retornar "aaaaa".
onlyVowels :: String -> String
onlyVowels x = filter isVowel2 x 

-- 3. Escreva uma função onlyNegatives :: [Int] -> [Int] que, dada uma lista de números, selecione somente
-- os que forem negativos.
onlyNegatives :: [Int] -> [Int]
onlyNegatives x = filter isNegative x

-- 4. Escreva uma função que, dada uma lista de números, aplique calcExprFloat para cada número da lista.
calcExprFloatList :: [Float] -> [Float]
calcExprFloatList x = map calcExprFloat x

-- 5. Crie uma função onlyLongWords :: [String] -> [String] que receba uma lista de strings e retorne
-- somente as strings longas (use a função isLongWord definida no código de exemplo no início da prática).
isLongWord :: String -> Bool -- isso é o mesmo que: isLongWord :: [Char] -> Bool
isLongWord s = length s > 10
onlyLongWords :: [String] -> [String]
onlyLongWords x =  filter isLongWord x

-- 6. Escreva uma função que receba uma lista de números e retorne somente aqueles que forem pares.
isEvenList :: [Int] -> [Int]
isEvenList x = filter even x

-- 7. Crie uma função que receba uma lista de palavras e retorne outra lista com somente aquelas que
-- terminarem com a letra 'r'.
onlyVerbsList :: [String] -> [String]
onlyVerbsList x = filter isVerb x

-- 8. Escreva uma função que receba uma lista de números e retorne somente os que estiverem entre 1 e 100,
-- inclusive. Você vai ter que criar uma função auxiliar e usar && para expressar o and lógico em Haskell.
betwenOneHundred :: [Float] -> [Float]
betwenOneHundred x = filter (\x -> x>0 && x<100) x

-- 9. Crie uma função que receba uma string e retorne o número de espaços nela contidos. Dica 1: você vai 
-- precisar de uma função que identifica espaços. Dica 2: aplique funções consecutivamente, isto é, use o 
-- resultado de uma função como argumento para outra.
countSpaces :: String -> Int
countSpaces x = length(filter (\x -> x==' ') x)

-- 10. Escreva uma função que, dada uma lista de idades de pessoas no ano atual, retorne uma lista somente
-- com as idades de quem nasceu depois de 1980. Para testar a condição, sua função deverá subtrair a idade
-- do ano atual.

onlyEightys :: [Int] -> [Int]
onlyEightys x = filter (\x -> 2020-x>1980) x

-- 11. Neste exercício, você vai criar uma função equivalente a elem, mas usando uma função de alta ordem.
-- Crie a função charFound :: Char -> String -> Bool que verifique se o caracter (primeiro argumento) está
-- contido na string (segundo argumento).
charFound :: Char -> String -> Bool
charFound x y = if length(filter (\y -> y==x) y) >= 1 then True else False