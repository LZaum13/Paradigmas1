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

-- Função para medir a lista
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (_:b) = 1 + (comprimento b)

-- Equivalente a verificar se 'a' existe em 'b'
semRepeticao :: [Int] -> Bool
semRepeticao [] = True
semRepeticao (a:b)
  | comprimento [x | x <- b, x == a] > 0 = False
  | otherwise                            = semRepeticao b

-- Retorna o primeiro elemento da lista
cabeca :: [t] -> t
cabeca (a:_) = a

-- Retorna o resto da lista (ignora o primeiro elemento)
cauda :: [t] -> [t]
cauda (_:b) = b

-- Gira a matriz transformando colunas em linhas
transpor :: [[Int]] -> [[Int]]
--se a primeira linha da matriz for vazia
transpor ([]:_) = []
--a cabeça de cada linha (primeira coluna) e concatena com a transposta do resto
transpor matriz = [cabeca linha | linha <- matriz] : transpor [cauda linha | linha <- matriz]

-- Verifica se todas as linhas de uma matriz não têm repetições
validaLinha :: [[Int]] -> Bool
validaLinha [] = True
validaLinha (linha:resto)
  | semRepeticao linha == True = validaLinha resto
  | otherwise                  = False

tabuleiroValido :: [[Int]] -> Bool
tabuleiroValido matriz
  | validaLinha matriz == True && validaLinha (transpor matriz) == True = True
  | otherwise = False

-- remove um elemento específico de uma lista
remover :: Int -> [Int] -> [Int]
remover _ [] = []
remover x (a:b)
  | x == a    = b
  | otherwise = a : remover x b

-- gera todas as permutações possíveis a partir de uma lista
permutacoes :: [Int] -> [[Int]]
permutacoes [] = [[]]
permutacoes lista = [x : resto | x <- lista, resto <- permutacoes (remover x lista)]

-- inverte uma lista para olhar de baixo para cima e da direita para a esquerda
inverte :: [Int] -> [Int]
inverte [] = []
inverte (a:b) = inverte b ++ [a]

-- valida se a linha bate com as dicas das duas pontas
validaDicaLinha :: [Int] -> Int -> Int -> Bool
validaDicaLinha linha dicaEsq dicaDir =
    (ContaPredio linha 0 == dicaEsq) && (ContaPredio (inverte linha) 0 == dicaDir)

-- Recebe as colunas transpostas e as listas de dicas do Norte e do Sul
validaDicaColunas :: [[Int]] -> [Int] -> [Int] -> Bool
validaDicaColunas [] _ _ = True
validaDicaColunas _ [] _ = True
validaDicaColunas (col:restoCols) (dN:restoDN) (dS:restoDS)
  | validaDicaLinha col dN dS == True = validaDicaColunas restoCols restoDN restoDS
  | otherwise = False

-- A função principal que devolve a matriz resolvida com as entradas fornecidas diretamente no código fonte.
resolverArranhaceus :: [Int] -> [Int] -> [Int] -> [Int] -> [[[Int]]]
resolverArranhaceus dicasNorte dicasSul dicasOeste dicasLeste =
  [ [l1, l2, l3, l4, l5, l6] |
      
      -- 1. Gera a primeira linha e testa  as dicas laterais
      l1 <- permutacoes [1..6]
      validaVisibilidadeLinha l1 (dicasOeste !! 0) (dicasLeste !! 0),
      
      -- 2. Gera a segunda linha, testa as dicas laterais e a validação de não repetição
      l2 <- permutacoes [1..6], 
      validaVisibilidadeLinha l2 (dicasOeste !! 1) (dicasLeste !! 1),
      tabuleiroValido (l1:l2:[]),
    
      l3 <- permutacoes [1..6], 
      validaVisibilidadeLinha l3 (dicasOeste !! 2) (dicasLeste !! 2),
      tabuleiroValido (l1:l2:l3:[]),
      
      l4 <- permutacoes [1..6], 
      validaVisibilidadeLinha l4 (dicasOeste !! 3) (dicasLeste !! 3),
      tabuleiroValido (l1:l2:l3:l4:[]),
      
      l5 <- permutacoes [1..6], 
      validaVisibilidadeLinha l5 (dicasOeste !! 4) (dicasLeste !! 4),
      tabuleiroValido (l1:l2:l3:l4:l5:[]),
      
      l6 <- permutacoes [1..6], 
      validaVisibilidadeLinha l6 (dicasOeste !! 5) (dicasLeste !! 5),
      tabuleiroValido (l1:l2:l3:l4:l5:l6:[]),
      
      -- 7. VALIDAÇÃO FINAL: Matriz preenchida. Transpomos para testar Norte e Sul.
      -- O let serve para vincular valores a um nome dentro do bloco.
      let colunas = transpor (l1:l2:l3:l4:l5:l6:[])
      validaVisibilidadeColunas colunas dicasNorte dicasSul
  ]

  main = do
  --dicas para um puzzle 6x6.
  let norte = [2, 1, 3, 4, 2, 2]
  let sul   = [2, 3, 2, 1, 4, 2]
  let oeste = [3, 2, 4, 1, 2, 3]
  let leste = [2, 2, 1, 5, 3, 2]
  
  -- printa o tabuleiro resolvido
  print (resolverWolkenkratzer norte sul oeste leste)