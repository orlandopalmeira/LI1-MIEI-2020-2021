{-| 
Module : Tarefa 1
Description : Tarefa 1 do Projeto da unidade curricular de /Laboratórios de informática I/
Copyright : Orlando José da Cunha Palmeira Palmeira <orlandopalmeira51@gmail.com>

== Relatório de execução da Tarefa 1
 
== Introdução
Nesta tarefa pretende-se gerar um labirinto totalmente aleatório.

== Objetivos
Para a tarefa 1, aquilo que mais tentámos fazer foi simplificar e compactar ao máximo as funções para gerar o labirinto e, para isso, 
tentámos priorizar o uso de funções pré-definidas no haskell, nomeadamente, funções de ordem superior.
Para tentar repartir o problema de gerar o labirinto em problemas mais pequenos, criámos funções separadas que permitem 
resolver o problema de forma mais simples. Temos uma função que começa por gerar um labirinto com paredes à sua volta e com
peças aletórias no interior. Seguidamente, criámos uma função que permite "abrir" o túnel no labirinto fornecido (labirinto
referido anteriormente) e, no final, existe uma função que írá colocar a casa dos fantasmas no labirinto com o túnel.

== Discussão e Conclusão
Após vários testes desta tarefa, no final, verificámos que a função principal (generateMaze) consegue, sem qualquer erro, gerar um labirinto totalmente aleatório.
-}
module Tarefa1 where
import System.Random
import Types

{-| I.) A função generateRandoms recebe dois inputs que são, respetivamente, a quantidade de números aleatórios a gerar e a seed que a função irá utilizar para gerar cada número. O output será uma lista com a quantidade de números aleatórios que lhe foi dada no input.
Esta função foi providenciada pelos docentes.
-}
generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed
                         in take n $ randomRs (0,99) gen

{-| II.) A função buildWall recebe um input que é um número inteiro. O output será um corredor apenas constituído por pecas do tipo parede (#) e o comprimento desse corredor é definido pelo número inteiro fornecido no input.
  
Funções auxiliares utilizadas na construção da buildWall:

* replicate (pré-definida)
-}
buildWall :: Int -> Corridor
buildWall c = replicate c Wall


{-| III.) A função buildPiece recebe um input que é um número inteiro. O output será uma peça baseada no número inteiro fornecido no input e nos critérios impostos pelo enunciado da tarefa 1. 

Esses critérios são:

1. Se o input for 3, o output será uma Comida Grande (Food Big: o)
2. Se o input for um número no intervalo [0;70[, excluindo o 3, o output será uma comida pequena (Food Little: .)
3. Se o input for um número no intervalo [70,99], o output será uma parede (Wall: #)
-}
buildPiece :: Int -> Piece
buildPiece n | n == 3 = Food Big
             | n >= 0 && n < 70 = Food Little
             | n >= 70 && n <= 99 = Wall
             | otherwise = Wall

{-| IV.)  A função buildCorridor recebe um input que é uma lista de inteiros e converte cada número da lista na sua respetiva peça seguindo os critérios definidos na função do ponto III.
  
Funções auxiliares utilizadas na construção da buildCorridor:

* map (pré-definida)
* buildPiece - função do ponto III que é aplicada a cada elemento do corredor utilizando a função map
-}
buildCorridor :: [Int] -> Corridor
buildCorridor [] = []
buildCorridor n  = map buildPiece n

{-| V.) A função buildBasicCorridor recebe dois inputs (números inteiros) que são, respetivamente, o comprimento do corredor e a seed (para gerar peças aleatórias). O output será um corredor básico.
  
Conceitos chave:

* Corredor básico: é apenas um corredor contituído por paredes nas extremidades e peças aleatórias no seu interior [Wall,...,Wall]

Funções auxiliares utilizadas na construção da buildBasicCorridor:

* buidCorridor - função do ponto IV
* generateRandoms - função do ponto I
 -}
buildBasicCorridor :: Int -> Int -> Corridor
buildBasicCorridor 0 _ = [Wall]
buildBasicCorridor c s = [Wall] ++ buildCorridor (generateRandoms (c-2) (s+1)) ++ [Wall]

{-| VI.) A função writeBasicCorridors recebe três inputs que são, respetivamente, o número de corredores básicos a gerar, o comprimento dos corredores gerados e a seed para gerar peças aleatórias. O output será uma lista de corredores básicos 
  
Conceitos chave:

* Corredor básico: definido na função do ponto V

Funções auxiliares utilizadas na construção da writeBasicCorridors:

* buildBasicCorridor - função do ponto V
-}
writeBasicCorridors :: Int -> Int -> Int -> [Corridor]
writeBasicCorridors 0 _ _ = []
writeBasicCorridors a c s = buildBasicCorridor c s : writeBasicCorridors (a-1) c (s+1)

{-| VII.) A função buildBasicMaze recebe três inputs que são, respetivamente, o comprimento, a altura do labirinto básico e a seed para gerar peças aleatórias no interior do labirinto.
  
Conceitos chave:

* Labirinto básico: é um labirinto cujo primeiro e último corredores são apenas definidos por paredes e os intermédios são corredores básicos.

Funções auxiliares utilizadas na construção da função buildBasicMaze:

* buildWall - função do ponto II
* writeBasicCorridors - função do ponto VI
 -}
buildBasicMaze :: Int -> Int -> Int -> Maze
buildBasicMaze _ 0 _ = []
buildBasicMaze c a s = [buildWall c] ++ writeBasicCorridors (a-2) c s ++ [buildWall c]

{-| VIII.) A função addEmpty recebe um input que é um corredor e o output será o mesmo corredor cuja única mudança que lhe foi aplicada é que esse corredor passará a ter peças do tipo Empty nas suas extremidades.
  
Funções auxiliares utilizadas na construção da função addEmpty:

* dropFstLst (função definida internamente na addEmpty) - esta função retira as peças das extremidades de um corredor
-}
addEmpty :: Corridor -> Corridor 
addEmpty c = [Empty] ++ dropFstLst c ++ [Empty]
  where dropFstLst c = tail (init c)

{-| IX.) A função partMazeUp recebe dois inputs que são, respetivamente, um número inteiro e um labirinto. O output será uma lista dos n primeiros corredores em que n é o número inteiro definido no input da função. O resultado desta função será utilizado porteriormente para separar a parte superior do labirinto que não terá a casa dos fantasmas e que não terá o túnel.
  
Funções auxiliares utilizadas na construção da função partMazeUp:

* take (pré-definida)
-}
partMazeUp :: Int -> Maze -> [Corridor]
partMazeUp n m = take n m

{-| X.) A função partMazeDown recebe dois inputs que são, respetivamente, um número inteiro e um labirinto. Esta função é muito similar à função partMazeUp (função do ponto IX), a única diferenca é que não devolverá os n primeiros corredores, mas sim os n últimos corredores em que n é o número inteiro definido no input da função. Tal como na função partMazeUp, o resultado desta função será utilizado porteriormente para separar a parte inferior do labirinto que não terá a casa dos fantasmas e que não terá o túnel.
  
Funções auxiliares utilizadas na construção da função partMazeDown:

* reverse (pré-definida)
* partMazeUp - função do ponto IX
 -}
partMazeDown :: Int -> Maze -> [Corridor]
partMazeDown n m = reverse $ partMazeUp n (reverse m)

{-| XI.) A função takeCorridor recebe dois inputs que são, respetivamente, um número inteiro e um labirinto. O output será o n-ésimo corredor do labirinto dado no input. Esta função será utilizada porteriormente para escolher o corredor ou corredores onde o túnel será construído.
  
Funções auxiliares utilizadas na construção da função takeCorridor:

* (!!) (pré-definida)
-}
takeCorridor :: Int -> Maze -> Corridor 
takeCorridor n c = c !! (n-1)

{-| XII.) A função buildTunnelOdd recebe um input que é um labirinto básico de /altura ímpar/ e, conforme as suas dimensões, o output será o mesmo labirinto com o túnel construído num local conveniente.
  
Conceitos chave:

* Labirinto básico: definido na função do ponto VII

Funções auxiliares utilizadas na construção da função buildTunnelOdd:

* quot (pré-definida)
* length (pré-definida)
* addEmpty - função do ponto VIII
* partMazeUp - função do ponto IX
* parmazeDown - função do ponto X
* takeCorridor - função do ponto XI
-}
buildTunnelOdd :: Maze -> Maze
buildTunnelOdd m = partMazeUp (quot (length m) 2 ) m ++
                   [addEmpty (takeCorridor (quot (length m) 2 +1) m)] ++
                   partMazeDown (quot (length m) 2 ) m

{-| XIII.) A função buildTunnelEven recebe um input que é um labirinto básico de /altura par/ e, conforme as suas dimensões, o output será o mesmo labirinto com o túnel construído num local conveniente.
  
Conceitos chave:

* Labirinto básico: definido na função do ponto VII

Funções auxiliares utilizadas na construção da função buildTunnelEven:

* quot (pré-definida)
* length (pré-definida)
* addEmpty - função do ponto VIII
* partMazeUp - função do ponto IX
* parmazeDown - função do ponto X
* takeCorridor - função do ponto XI
-}
buildTunnelEven :: Maze -> Maze
buildTunnelEven m = partMazeUp (quot (length m) 2 - 1) m ++
                    [addEmpty (takeCorridor (quot (length m) 2) m)] ++
                    [addEmpty (takeCorridor (quot (length m) 2 +1) m)] ++
                    partMazeDown (quot (length m) 2 - 1) m

{-| XIV.) A função buildTunnel recebe um input que é um labirinto básico e, conforme a sua altura, irá decidir se utiliza a função buildTunnelOdd ou a função buildTunnelEven para construir o túnel e o seu output será o labirinto fornecido no input com o túnel construído.
  
Conceitos chave:

* Labirinto básico: definido na função do ponto VII

Funções auxiliares utilizadas na construção da função buildTunnel:

* even (pré-definida)
* length (pré-definida)
* buildTunnelOdd - função do ponto XII
* buildTunnelOdd - função do ponto XIII
 -}
buildTunnel :: Maze -> Maze 
buildTunnel m | even (length m) = buildTunnelEven m
              | otherwise = buildTunnelOdd m

{-| XV.) A função houseOdd é constante e o seu output é a casa dos fantasmas quando o comprimento do labirinto for ímpar 
-}
houseOdd :: [Corridor]
houseOdd = [
            replicate 11 Empty,
            [Empty] ++ replicate 3 Wall ++ replicate 3 Empty ++ replicate 3 Wall ++ [Empty],
            [Empty,Wall] ++ replicate 7 Empty ++ [Wall,Empty],
            [Empty] ++ replicate 9 Wall ++ [Empty],
            replicate 11 Empty
           ]             

{-| XVI.) A função houseEven é constante e o seu output é a casa dos fantasmas quando o comprimento do labirinto for par  
-}
houseEven :: [Corridor]
houseEven = [
             replicate 10 Empty,
             [Empty] ++ replicate 3 Wall ++ replicate 2 Empty ++ replicate 3 Wall ++ [Empty],
             [Empty,Wall] ++ replicate 6 Empty ++ [Wall,Empty],
             [Empty] ++ replicate 8 Wall ++ [Empty],
             replicate 10 Empty
            ]

{-| XVII.) A função firsts tem um input que é um corredor e o output será os n primeiros elementos desse corredor em que n é definido pela própria função de modo a obter os primeiros elementos de forma conveniente para posteriormente ser mais fácil inserir a casa dos fantasmas.
  
Funções auxiliares utilizadas na construção da função buildTunnel:

* even (pré-definida)
* length (pré-definida)
* take (pré-definida)
* div (pré definida)
-}
firsts :: Corridor -> [Piece]
firsts c | even (length c) = take (div (length c -10) 2) c
         | otherwise       = take (div (length c -11) 2) c

{-| XVIII.) A função lasts tem um input que é um corredor e o output será os n últimos elementos desse corredor em que n é definido pela própria função de modo a obter os últimos elementos de forma conveniente para posteriormente ser mais fácil inserir a casa dos fantasmas.
  
Funções auxiliares utilizadas na construção da função buildTunnel:

* even (pré-definida)
* length (pré-definida)
* take (pré-definida)
* div (pré definida)
-}
lasts :: Corridor -> [Piece]
lasts c | even (length c) = drop ((div (length c -10) 2) + 10) c
        | otherwise       = drop ((div (length c -11) 2) + 11) c

{-| XIX.) A função coreMaze tem um input que é um labirinto e o output será os corredores onde a casa dos fantasmas será inserida. Esta função seleciona de forma conveniente, de acordo com as dimensões do labirinto, os corredores que terão a casa dos fastasmas.
  
Funções auxiliares utilizadas na construção da função coreMaze:

* coreMaze' (função definida internamente na coreMaze) - através de dois acumuladores, a função coreMaze' irá devolver os corredores onde será inserida a casa dos fantasmas
-}
coreMaze :: Maze -> [Corridor]
coreMaze m = coreMaze' (div (length m - 5) 2) 5 m
  where coreMaze' :: Int -> Int -> Maze -> [Corridor]
        coreMaze' _ 0 _      = []
        coreMaze' _ _ []     = []
        coreMaze' 0 c (x:xs) = x:coreMaze' 0 (c-1) xs
        coreMaze' n c (x:xs) = coreMaze' (n-1) c xs

{-| XX.) A função coreMazeWithHouse insere cada corredor da casa dos fantasmas em cada corredor correspondente pelo que o output será os corredores centrais do labirinto com a casa dos fantasmas inserida. A função irá decidir que tipo de casa dos fantasmas irá inserir de acordo com o comprimento dos corredores do labirinto, isto é, se é par ou ímpar.
  
Funções auxiliares utilizadas na construção da função coreMazeWithHouse:

* even (pré-definida)
* length (pré-definida)
* firsts - função do ponto XVII
* lasts - função do ponto XVIII
* takeCorridor - função do ponto XI

-}
coreMazeWithHouse :: [Corridor] -> [Corridor]
coreMazeWithHouse [c1,c2,c3,c4,c5] | even (length c1) = [firsts c1 ++ takeCorridor 1 houseEven ++ lasts c1,
                                                         firsts c2 ++ takeCorridor 2 houseEven ++ lasts c2,
                                                         firsts c3 ++ takeCorridor 3 houseEven ++ lasts c3,
                                                         firsts c4 ++ takeCorridor 4 houseEven ++ lasts c4,
                                                         firsts c5 ++ takeCorridor 5 houseEven ++ lasts c5]
                                   | otherwise        = [firsts c1 ++ takeCorridor 1 houseOdd ++ lasts c1,
                                                         firsts c2 ++ takeCorridor 2 houseOdd ++ lasts c2,
                                                         firsts c3 ++ takeCorridor 3 houseOdd ++ lasts c3,
                                                         firsts c4 ++ takeCorridor 4 houseOdd ++ lasts c4,
                                                         firsts c5 ++ takeCorridor 5 houseOdd ++ lasts c5]

{-| XXI.) A função insertHouse tem um input que é um labirinto e o output será esse mesmo labirinto com a casa dos fantasmas inserida.
  
Funções auxiliares utilizadas na construção da função coreMazeWithHouse:

* div (pré-definida)
* length (pré-definida)
* partMazeUp - função do ponto IX
* partMazeDown - função do ponto X
* coreMaze - função do ponto XIX
* coreMazeWithHouse - função do ponto XX
-}
insertHouse :: Maze -> Maze
insertHouse m = partMazeUp (div ((length m)-5) 2) m ++
                coreMazeWithHouse (coreMaze m) ++
                partMazeDown (div ((length m)-4) 2) m

{-| XXII.) A função generateMaze tem três inputs que são, respetivamente, o comprimento, a altura e a seed (para gerar peças aleatórias) que são números inteiros. O output será um labirinto válido e totalmente aleatório. Todas as funções deste módulo são auxiliares desta função principal que era o objetivo da Tarefa 1.
  
Funções auxiliares utilizadas na construção da função generateMaze:

* buildBasicMaze - função do ponto VII
* buildTunnel - função do ponto XIV
* insertHouse - função do ponto XXI
-}
generateMaze :: Int -> Int -> Int -> Maze
generateMaze c a s | c < 15 || a < 10 = error "Dimensions too small"
                   | otherwise = insertHouse (buildTunnel (buildBasicMaze c a s))
