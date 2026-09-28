module Arranhaceu where

-- type para representar o tabuleiro do jogo Arranha-Céu
type Linha = [Int]
type Tabuleiro = [Linha]

-- As dicas das bordas
type Dicas = ([Int], [Int], [Int], [Int])

-- recebe uma lista de Int, um Int, e retorna um Int
ContaPredio :: [Int] -> Int -> Int

-- Caso base: Lista vazia []. O caractere '_' é um "coringa" isolado
-- que ignora o parâmetro maiorVisto, já que não importa.
ContaPredio [] _ = 0

-- Caso recursivo: Separamos o primeiro prédio (a) do resto (b)
ContaPredio (a:b) maiorVisto
  | (a > maiorVisto) = 1 + (ContaPredio b a)
  | otherwise        = ContaPredio b maiorVisto

linhasValidas :: [[Int]] -> Int -> [[Int]]
linhasValidas todasLinhas dica = [linha | linha <- todasLinhas, (ContaPredio linha 0) == dica]