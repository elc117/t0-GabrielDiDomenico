-- Autor: Gabriel Di Domenico
-- Data: 08/04/2020
--
-- 1. Usando list comprehension, defina uma função add10toall :: [Int] -> [Int], que receba uma lista e adicione o valor 10 a cada
-- elemento dessa lista, produzindo outra lista.
--
add10toall :: [Int] -> [Int]
add10toall list = [x+10 | x <- list]
--
-- 2. Usando list comprehension, defina uma função multN :: Int -> [Int] -> [Int], que receba um número N e uma lista, e multiplique
-- cada elemento da lista por N, produzindo outra lista.
--
multN :: Int -> [Int] -> [Int]
multN mul list = [x*mul | x <- list]
--
-- 3. Usando list comprehension, defina uma função applyExpr :: [Int] -> [Int], que receba uma lista e calcule 3*x+2 para cada
-- elemento x da lista, produzindo outra lista.
--
applyExpr :: [Int] -> [Int]
applyExpr list = [(3*x)+2 | x <- list]
--
-- 4. Usando list comprehension, escreva uma função addSuffix :: String -> [String] -> [String] , para adicionar um dado sufixo
-- às strings contidas numa lista. 
--
addSuffix :: String -> [String] -> [String]
addSuffix suf list = [x++suf | x <- list]
--
-- 5. Usando list comprehension, defina uma função selectgt5 :: [Int] -> [Int], que receba uma lista e selecione somente os
-- valores maiores que 5, produzindo outra lista.
--
selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x>5]
--
-- 6. Usando list comprehension, defina uma função sumOdds :: [Int] -> Int, que receba uma lista e obtenha o somatório dos valores
-- ímpares, produzindo outra lista. Pesquise funções auxiliares que manipulem listas.
--
sumOdds :: [Int] -> Int
sumOdds list = sum [x | x <- list, x `mod` 2==1]
--
-- 7. Usando list comprehension, defina uma função selectExpr :: [Int] -> [Int], que receba uma lista e selecione somente os valores
-- pares entre 20 e 50, produzindo outra lista.
--
selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list, x `mod` 2==0 && x < 50 && x > 20]
--
-- 8. Escreva uma função countShorts :: [String] -> Int, que receba uma lista de palavras e retorne a quantidade de palavras dessa 
-- lista que possuem menos de 5 caracteres. Use list comprehension.
--
countShorts :: [String] -> Int
countShorts list = length [x | x <- list, (length x) < 5]
--
-- 9. Escreva uma função calcExpr :: [Float] -> [Float], que calcule x^2/2 para cada elemento x da lista de entrada e selecione
-- apenas os resultados que forem maiores que 10. Use list comprehension.
--
calcExpr :: [Float] -> [Float]
calcExpr list = [(x**2)/2 | x <- list, (x**2)/2>10]