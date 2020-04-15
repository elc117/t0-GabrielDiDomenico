-- Prática do dia 23/03/2020 da Disciplina de Paradigmas
-- Autor: Gabriel Di Domenico

-- ----------------------- Texto exemplo ------------------------------------

main = do 
  putStrLn "Vamos testar as funcoes abaixo!"
  
square :: Int -> Int
square x = x^2

squareAll :: [Int] -> [Int]
squareAll lis = map square lis 

ficaemcasa :: String -> String
ficaemcasa fulano = fulano ++ ", fica em casa!"

quarentena :: [String] -> [String]
quarentena pessoas = map ficaemcasa pessoas

podesair :: String -> Bool
podesair profissao = profissao == "Medico"

idadeadulta :: Int -> Bool
idadeadulta idade = idade >= 18

-- ----------------------- Texto exemplo ------------------------------------

checaTemperatura :: Float -> Bool
checaTemperatura x = if x>37.8 then True else False

verificaFebre :: [Float] -> [Float]
verificaFebre x = filter checaTemperatura x
--verificaFebre x = filter (\x -> x>37.8) x