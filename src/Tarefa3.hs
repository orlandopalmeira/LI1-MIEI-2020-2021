{-|
Module : Tarefa 3
Description : Tarefa 3 do Projeto da unidade curricular de /Laboratórios de informática I/
Copyright : Orlando José da Cunha Palmeira Palmeira <orlandopalmeira51@gmail.com>
            

== Relatório de execução da Tarefa 3

== Introdução
Nesta tarefa, o objetivo é converter um labirinto numa sequência de instruções de modo a convertê-lo num formato mais 
compacto para leitura.

== Objetivos
Tal como na tarefa 1, o objetivo desta tarefa foi compactar e simplificar tanto quanto possível as funções que permitem o seu 
funcionamento. Para isso, como em todas as outras tarefas, a resolução do problema desta tarefa for reparti-lo em subproblemas
que consigam ser facilmente resolvidos com funções auxiliares.

== Discussão e conclusão
Após os devidos testes desta tarefa, verificámos que o código foi bem sucedido a cumprir com o pretendido no enunciado.
Não há muito a dizer sobre esta tarefa uma vez que a mesma não é das mais relevantes neste projeto.
-}
module Tarefa3 where

import Types

{-| I.) A função compactCorridor tem um input que é um corredor e devolve a instrução relativa a esse corredor.
  
Funções auxiliares utilizadas na construção da compactCorridor:

* convertCorridor (definida internamente na função compactCorridor) - converte um corredor num formato de tuplos com a informação ordenada desse corredor. Por exemplo, [Wall,Wall,Empty,Food Big,Food Little,Wall] = [(1,Wall),(1,Wall),(1,Empty),(1,Food Big),(1,Food Little),(1,Wall)]
* compactCorridorAux (definida internamente na função compactCorridor) - converte uma lista de tuplos com a informação do labirinto num formato mais compacto. Por exemplo, [(1,Wall),(1,Wall),(1,Empty),(1,Food Big),(1,Food Little),(1,Wall)] = [(2,Wall),(1,Wall),(1,Empty),(1,Food Big),(1,Food Little),(1,Wall)]
-}
compactCorridor :: Corridor -> Instruction
compactCorridor [] = Instruct []
compactCorridor c = Instruct (compactCorridorAux (convertCorridor c))
        where
            
            convertCorridor [] = []
            convertCorridor (p:ps) = (1,p):convertCorridor ps
            
            compactCorridorAux [] = []
            compactCorridorAux [(k,p)] = [(k,p)]
            compactCorridorAux ((k1,p1):(k2,p2):t) | p1 == p2 = compactCorridorAux ((k1+k2,p1):t)
                                                   | otherwise = (k1,p1):compactCorridorAux ((k2,p2):t)

{-| II.) A função compactCorridors tem um input que é um labirinto e devolve a lista de instruções relativas a cada corredor desse labirinto.
  
Funções auxiliares utilizadas na construção da compactCorridors:

* map (pré-definida)
* compactCorridor - função do ponto I
-}
compactCorridors :: Maze -> Instructions
compactCorridors l = map compactCorridor l

{-| III.) A função findTheSame tem dois inputs que são, respetivamente, uma instrução e uma lista de instruções. O output será uma instrução no formato Repeat n em que o n é o indice da instrução na lista de instruções dada no input que é igual à instrução também dada no input.
	
Funções auxiliares utilizadas na construção da função findTheSame:

* findTheSameAux (função definida internamente na findTheSame) - esta função faz o que é pretendido na findTheSame mas utiliza um acumulador para a obtenção dos índices
-}
findTheSame :: Instruction -> Instructions -> Instruction
findTheSame _ [] = undefined
findTheSame i1 (i:is) = findTheSameAux i1 (i:is) 0
    where
        findTheSameAux ins [] _ = ins
        findTheSameAux ins (y:ys) n | ins == y = Repeat n
                                    | otherwise = findTheSameAux ins ys (n+1)

{-| IV.) A função makeRepeats tem um input que é uma lista de instruções e o output será uma nova lista de instruções em que essa lista irá conter instruções no formato Repeat n quando dentro da lista de instruções dada no input houver instruções iguais.
  
Funções auxiliares utilizadas na construção da função makeRepeats:

* makeRepeatsAux (função definida internamente na makeRepeats) - esta função faz o que queremos que a makeRepeats faça mas serve de auxiliar e usa acumuladores.
-}
makeRepeats :: Instructions -> Instructions
makeRepeats [] = []
makeRepeats (i:is) = makeRepeatsAux [i] is
    where
        makeRepeatsAux ac [] = ac
        makeRepeatsAux ac (x:xs) | x `elem` ac = makeRepeatsAux (ac ++ [findTheSame x ac]) xs
                                 | otherwise = makeRepeatsAux (ac ++ [x]) xs

{-| V.) A função compactMaze tem um input que é um labirinto e o output será uma lista de instruções em que essa lista irá conter instruções no formato Repeat n quando dentro do labirinto dado no input houver corredores iguais.
  
Funções auxiliares utilizadas na construção da função compactMaze:

* makeRepeats - função do ponto IV
* compactCorridors - função do ponto II
-}
compactMaze :: Maze -> Instructions
compactMaze [] = []
compactMaze m = makeRepeats (compactCorridors m)