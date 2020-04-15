-- Autor: Gabriel Di Domenico
-- Data: 06/04/2020
--
import Data.Char
-- 4. Escreva um programa que leia um nome da entrada padrão e mostre uma mensagem usando este nome.
--
helloYou :: IO()
helloYou = do
         nome <- getLine
         let mensagem = "Hello, "++nome
         putStrLn mensagem
--
-- 5. Escreva um programa que leia uma string da entrada padrão e aplique algum teste a esta string, mostrando uma mensagem para informar se o teste resultou positivo ou negativo.
--
procuraGabriel :: IO()
procuraGabriel = do
               nome <- getLine
               let resultado = if(nome == "Gabriel") then "Encontrei Voce" else "Sai daqui seu nao gabriel"
               putStrLn resultado
--
-- 6. Baixe o programa validaCPF.hs e carregue-o no interpretador. Para executá-lo, chame a função main. Examine o código e note que ele difere do código mostrado nos slides. Que diferenças você notou?
--
-- 7. Modifique o código para incluir a parte de I/O que espera o usuário digitar o CPF (ver versões do código nos slid
--
isCpfOk :: [Int] -> Bool
isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      expr1 = (sum $ zipWith (*) digitos1 [10,9..2]) `mod` 11
      dv1 = if expr1 < 2 then 0 else 11-expr1

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
      expr2 = (sum $ zipWith (*) digitos2 [11,10..2]) `mod` 11
      dv2 = if expr2 < 2 then 0 else 11-expr2
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

validaCpf :: IO()
validaCpf = do
  cpf <- getLine
  let  digitos = (map digitToInt cpf)
       result = isCpfOk digitos
  putStrLn (show result)
--
----------------------------------------- Geracao de SVGs -------------------------------------------------
--
-- Baixe o programa svgRects.hs e execute-o chamando a função main. Isso deverá gerar um arquivo chamado rects.svg. Visualize esse arquivo usando o navegador ou outro programa que entenda SVG.
--
