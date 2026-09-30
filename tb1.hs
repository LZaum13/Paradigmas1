-- tipo para representar o tabuleiro do jogo Arranha-Céu
type Linha = [Int]
type Tabuleiro = [Linha]

-- As dicas das bordas
type Dicas = ([Int], [Int], [Int], [Int])

-- recebe uma lista de Int, um Int, e retorna um Int
contaPredio :: [Int] -> Int -> Int

-- Lista Vazia[]: ignora o parâmetro maiorVisto, já que não importa.
contaPredio [] _ = 0

-- Caso recursivo: Separamos o primeiro prédio (a) do resto (b)
contaPredio (a:b) maiorVisto
  | (a > maiorVisto) = 1 + (contaPredio b a)
  | otherwise        = contaPredio b maiorVisto

-- Recebe uma lista de linhas e uma dica, e retorna uma lista de linhas que batem com a dica
linhasValidas :: [[Int]] -> Int -> [[Int]]
linhasValidas todasLinhas dica = [linha | linha <- todasLinhas, (contaPredio linha 0) == dica]

-- Função para medir a lista
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (_:b) = 1 + (comprimento b)

-- Função para verificar se um elemento existe em uma lista
existe :: Int -> [Int] -> Bool
existe _ [] = False
existe n (a:b)
  | n == a    = True
  | otherwise = existe n b

-- Função para verificar se uma lista não tem elementos repetidos
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

novaLinhaValida :: [Int] -> [[Int]] -> Bool
novaLinhaValida [] _ = True
novaLinhaValida (x:xs) colunas =
  not (existe x (map cabeca colunas))
  && novaLinhaValida xs (map cauda colunas)

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

    linhasValidas =
      [ [l | l <- todasPerms,
            validaDicaLinha l (dicasOeste !! i) (dicasLeste !! i)]
      | i <- [0..tamanho-1]
      ]    
    busca linhaAtual matrizParcial
      | linhaAtual == tamanho = 
          let matrizCompleta = inverte matrizParcial
              colunas = transpor matrizCompleta
          in [ matrizCompleta | validaDicaColunas colunas dicasNorte dicasSul == True ]
          
      | otherwise = 
          [ solucaoFinal
          | l <- linhasValidas !! linhaAtual
          , novaLinhaValida l matrizParcial
          , solucaoFinal <- busca (linhaAtual + 1) (l : matrizParcial)
          ]
  in 
    busca 0 []

-- Formatação do tabuleiro para o print
imprimeTabuleiro :: [[Int]] -> IO ()
imprimeTabuleiro [] = return ()
imprimeTabuleiro (linha:resto) = do
  print linha
  imprimeTabuleiro resto

main = do

  let tamanho = 6 -- NxN tabuleiro ex: (6x6)
  let numeros = [1, 2, 3, 4, 5, 6] -- números de 1 a 6 para um tabuleiro 6x6

  --dicas para um puzzle 6x6.
  let norte = [2, 3, 2, 2, 1, 3]
  let sul   = [3, 1, 3, 2, 4, 2]
  let oeste = [5, 1, 3, 2, 4, 2]
  let leste = [2, 3, 2, 1, 2, 2]

  -- printa o tabuleiro resolvido
  let solucoes = resolverArranhaceus tamanho numeros norte sul oeste leste
  case solucoes of
      [] -> putStrLn "Nenhuma solução encontrada."
      (solucao:_) -> imprimeTabuleiro solucao
