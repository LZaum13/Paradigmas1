-- type para representar o tabuleiro do jogo Arranha-Céu
type Linha = [Int]
type Tabuleiro = [Linha]

-- As dicas das bordas
type Dicas = ([Int], [Int], [Int], [Int])

-- recebe uma lista de Int, um Int, e retorna um Int
contaPredio :: [Int] -> Int -> Int

-- Caso base: Lista vazia []. O caractere '_' é um "coringa" isolado
-- que ignora o parâmetro maiorVisto, já que não importa.
contaPredio [] _ = 0

-- Caso recursivo: Separamos o primeiro prédio (a) do resto (b)
contaPredio (a:b) maiorVisto
  | (a > maiorVisto) = 1 + (contaPredio b a)
  | otherwise        = contaPredio b maiorVisto

linhasValidas :: [[Int]] -> Int -> [[Int]]
linhasValidas todasLinhas dica = [linha | linha <- todasLinhas, (contaPredio linha 0) == dica]

-- Função para medir a lista
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (_:b) = 1 + (comprimento b)

existe :: Int -> [Int] -> Bool
existe _ [] = False
existe n (a:b)
  | n == a    = True
  | otherwise = existe n b

semRepeticao :: [Int] -> Bool
semRepeticao [] = True
semRepeticao (a:b)
  | existe a b == True = False
  | otherwise          = semRepeticao b

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
  -- Só precisamos de testar as colunas (matriz transposta), 
  -- pois as linhas geradas pelas permutações já não têm repetidos.
  | validaLinha (transpor matriz) == True = True
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
inverte :: [t] -> [t]
inverte [] = []
inverte (a:b) = inverte b ++ [a]

-- valida se a linha bate com as dicas das duas pontas
validaDicaLinha :: [Int] -> Int -> Int -> Bool
validaDicaLinha linha dicaEsq dicaDir =
  (dicaEsq == 0 || contaPredio linha 0 == dicaEsq) && 
  (dicaDir == 0 || contaPredio (inverte linha) 0 == dicaDir)

-- Recebe as colunas transpostas e as listas de dicas do Norte e do Sul
validaDicaColunas :: [[Int]] -> [Int] -> [Int] -> Bool
validaDicaColunas [] [] [] = True
validaDicaColunas [] _ _ = False
validaDicaColunas _ [] _ = False
validaDicaColunas _ _ [] = False
validaDicaColunas (col:restoCols) (dN:restoDN) (dS:restoDS)
  | validaDicaLinha col dN dS == True = validaDicaColunas restoCols restoDN restoDS
  | otherwise = False

-- A função principal que devolve a matriz resolvida com as entradas fornecidas diretamente no código fonte.
resolverArranhaceus :: Int -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [[[Int]]]
resolverArranhaceus tamanho numeros dicasNorte dicasSul dicasOeste dicasLeste =
  let 
    todasPerms = permutacoes numeros
    
    linhasValidasPorIndice i = [l | l <- todasPerms, validaDicaLinha l (dicasOeste !! i) (dicasLeste !! i)]
    
    busca linhaAtual matrizParcial
      | linhaAtual == tamanho = 
          let matrizCompleta = inverte matrizParcial
              colunas = transpor matrizCompleta
          in [ matrizCompleta | validaDicaColunas colunas dicasNorte dicasSul == True ]
          
      | otherwise = 
          [ solucaoFinal
          | l <- linhasValidasPorIndice linhaAtual
          , tabuleiroValido (l : matrizParcial)
          , solucaoFinal <- busca (linhaAtual + 1) (l : matrizParcial)
          ]
  in 
    busca 0 []    

main = do

  let tamanho = 4 -- NxN tabuleiro ex: (4x4)
  let numeros = [1, 2, 3, 4] -- números de 1 a 4 para um tabuleiro 4x4

  --dicas para um puzzle 4x4.
  let norte = [4, 0, 0, 0]
  let sul   = [0, 2, 2, 0]
  let oeste = [0, 2, 2, 1]
  let leste = [3, 2, 0, 0]

  -- printa o tabuleiro resolvido
  print (resolverArranhaceus tamanho numeros norte sul oeste leste)