{-| 
Module : Tarefa 4
Description : Tarefa 4 do Projeto da unidade curricular de /Laboratórios de informática I/
Copyright : Orlando José da Cunha Palmeira Palmeira <orlandopalmeira51@gmail.com>
            

== Relatório de execução da Tarefa 4

== Introdução
Nesta tarefa pretende-se fazer com que o jogo, conforme o seu estado, reaja de forma deferente à passagem do tempo.
O fator dominante nesta tarefa é a velocidade do jogador uma vez que é a velocidade o critério de movimentação dos jogadores.

== Objetivos
O objetivo que tivemos com esta tarefa foi apenas torná-la simples e de fácil resolução.
Uma vez que esta tarefa é das mais importantes, nomeadamente para verificar o devido funcionamento da tarefa subsequente 
(tarefa 5), nós tentámos terminá-la no mais curto espaço de tempo possível.
Outro objetivo, não tão relevante, foi tentar compactar tanto quanto possível o tamanho e aumentar a simplicidade das funções.
Para cumprir o objetivo pretendido, esta tarefa exige que se efetue um número considerável de verificações e, naturalmente, isto 
traduz-se num aumento do tamanho e complexidade do código e isso foi algo que tentámos reduzir ao máximo.

== Discussões e conclusão
Apesar de ter tido várias correções no sentido de compactar esta tarefa, devemos referir que esta não foi de difícil resolução
e que o caminho para obter um código bem sucedido não foi difícil nem demorado.
Os testes demonstraram uma boa execução da tarefa e o cumprimento dos objetivos referidos no enunciado.
-}

module Tarefa4 where

import Tarefa5 (ghostPlay)
import Tarefa2 ( getGhostMode, getPlayer, play, takePacman ) 
import Types
    ( getPlayerID,
      getPlayerOrientation,
      GhostMode(Dead),
      Play(..),
      Player(Ghost),
      State(playersState) ) 

defaultDelayTime :: Integer
defaultDelayTime = 250 -- 250 ms


{-| I.) A função generatePlays tem um estado como input e uma lista de jogadas como output. 
Esta função recebe a lista de jogadas dos fantasmas da ghostPlay da Tarefa 5 e apenas lhe acrecenta a jogada do pacman

Funções auxiliares na construção da generatePlays:
* ghostPlay (função do módulo Tarefa5)
* takePacman (função do módulo Tarefa2)
* getPlayerID (função do módulo Types)
* getPlayerOrientation (função do módulo Types)
 -}
generatePlays :: State -> [Play]
generatePlays state = ghostsPlays ++ [playpacman]
    where
        ghostsPlays = ghostPlay state
        pacman = takePacman (playersState state)
        playpacman = Move (getPlayerID pacman) (getPlayerOrientation pacman)


{-| II.) A função isGhost tem um player como input, e um bool como output. Esta função só indica se um determinado jogador
 é fantasma ou não. -}
isGhost :: Player -> Bool 
isGhost (Ghost _) = True
isGhost _ = False


{-| III.) A função convertPlays tem três inputs que são, respetivamente, o step do manager, um estado e uma lista de jogadas. 
A lista de jogadas recebida vai ser "filtrada" de modo a se poder visualizar uma possível redução da velocidade do jogador.
Se o step for par, a lista devolvida é igual à lista recebida. Caso contrário, a função irá verificar cada jogada e, 
se o jogador relativo à jogada for PacMan, a função não efetua nenhuma alteração. Se o jogador for fantasma e estiver em modo
Alive, a função também não efetua nenhuma alteração. Caso o fantasma esteja em modo Dead, a jogada relativa a esse fantasma é
eliminada. Assim, a função permite-nos ter jogadas intermitentes para simular um deslocamento mais lento do fantasma caso este
esteja em modo Dead.

Funções auxiliares na construção da convertPlays:
* even (pré-definida)
* isGhost (função do ponto II)
* getGhostMode (função do módulo Tarefa2)
* getPlayer (função do módulo Tarefa2)
-}
convertPlays :: Int -> State -> [Play] -> [Play]
convertPlays _ _ [] = []
convertPlays step state (Move id o : t) | even step = Move id o : t
                                        | otherwise = if isGhost (getPlayer id (playersState state))
                                                      then if getGhostMode (getPlayer id (playersState state)) == Dead 
                                                           then convertPlays step state t
                                                           else Move id o : convertPlays step state t
                                                      else Move id o : convertPlays step state t

{-| IV.) A função passTimeAux tem dois inputs que são, respetivamente, um estado e uma lista de jogadas. Esta função 
será auxiliar da função principal (passTime - ponto V). O resultado será o novo estado em que as jogadas recebidas são aplicadas 
ao estado recebido.

Funções auxiliares na construção da passTimeAux:
* foldr (pré-definida)
-}
passTimeAux :: State -> [Play] -> State 
passTimeAux = foldr play

{-| V.) A função passTime tem dois inputs que são, respetivamente, o step do manager e um estado. Comforme o step recebido, 
a função irá atuar de modo a simular a passagem do tempo nos jogadores conforme as carateristicas inerentes a estes no instante atual.

Funções auxiliares na construção da generatePlays:
* passTimeAux (função do ponto IV)
* convertPlays (função do ponto III)
* generatePlays (função do ponto I)
-}
passTime :: Int -> State -> State
passTime step s = passTimeAux s plays
    where 
        plays = convertPlays step s (generatePlays s)