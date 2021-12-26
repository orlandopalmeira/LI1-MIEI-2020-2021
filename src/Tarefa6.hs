{-| 
Module : Tarefa 6
Description : Tarefa 6 do Projeto da unidade curricular de /Laboratórios de informática I/
Copyright : Orlando José da Cunha Palmeira Palmeira <orlandopalmeira51@gmail.com>
            
            
== Relatório de execução da Tarefa 6

== Introdução 
Esta tarefa tem como objetivo a implementação de um robô que seja capaz de jogar automaticamente, controlando o pacman.

== Objetivos
Como já referido o objetivo desta tarefa foi determinar as melhores jogadas para o pacman possíveis.
Para isso, optamos por uma estratégia de fuga, quando o fantasma esta em modo Alive, e uma estratégia de perseguição,
quando este se encontra em modo Dead. Em ambas as estratégias as jogadas são determinadas com base na posição e modo 
do fantasma mais próximo ao pacman. 
Na primeira, o pacman tenta realizar a jogada de fuga ideal, considerando as possíveis paredes que possa encontrar no 
seu caminho, evitando-as. Apesar de o principal objetivo ser manter-se distante ao fantasma mais próximo, o pacman, 
quando esta distância é considerável, opta pelas jogadas que favorecem a obtenção de comida.
Na segunda, a estratégia adotada passa por tentar alcançar o fantasma, que se encontra mais próximo, o mais rápido 
possível. O pacman poderá dirigir-se diretamente ao fantasma ou dirigir-se primeiro ao túnel mais próximo, caso o 
fantasma que persegue se encontre perto do túnel oposto.

== Discussão e conclusão
Após alguns testes e correções, consideramos que o robô toma as decisões mais acertadas com base no estado do jogo,
cumprindo os objetivos apontados no enunciado.
-}

module Tarefa6 where

import Types
import Tarefa2
import Tarefa5
import Data.List 

{-| I.) A função dist recebe dois jogadores, o pacman e um fantasma, respetivamente. Irá devolver um tuplo em que a 
primeira componente é a distância entre os dois jogadores e a segunda é o fantasma.

Funções auxiliares na construção da dist:
* getPlayerCoords (função do módulo types)
-}
dist :: Player -> Player -> (Int,Player)  
dist p g = ((abs (xp - xg)) + (abs (yp - yg)), g)
    where (xp,yp) = getPlayerCoords p
          (xg,yg) = getPlayerCoords g

{-| II.) A função closestGhost recebe um estado e irá devolver um tuplo cuja primeira componente é a distância do pacman
ao fantasma mais próximo e a segunda componente é esse mesmo fantasma.

Funções auxiliares na construção da closestGhost:
* head (pré-definida)
* map (pré-definida)
* sortOn (Data.List)
* takePacman (função do módulo Tarefa2)
* dropPacman (função do módulo Tarefa2)
-}
closestGhost :: State -> (Int,Player) 
closestGhost s@(State m ps _) = head $ sortOn fst d 
    where p = takePacman ps
          gs = dropPacman ps
          d = map (dist p) gs 

{-| III.) A calcPacNormalAux recebe um labirinto, coordenadas (de um fantasma) e um jogador (pacman). Conforme a posição 
relativa do pacman ao fantasma, a função irá retornar a melhor jogada de fuga possível para o pacman.

Funções auxiliares na construção da calcPacNormalAux:
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* takePiece (função do módulo Tarefa2)
* getRelativePacmanPosition (função do módulo Tarefa5)
-}
calcPacNormalAux :: Maze -> Coords -> Player -> Play 
calcPacNormalAux m (xg,yg) pacman | pospac == IsLeft || pospac == IsLeftUp = if pieceL /= Wall then Move (getPlayerID pacman) L
                                                                             else 
                                                                                 if pieceU /= Wall then Move (getPlayerID pacman) U
                                                                                 else if pieceD /= Wall then Move (getPlayerID pacman) D
                                                                                      else Move (getPlayerID pacman) R
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacNormalAux m (xg,yg) pacman | pospac == IsLeftDown = if pieceD /= Wall then Move (getPlayerID pacman) D
                                                           else 
                                                               if pieceL /= Wall then Move (getPlayerID pacman) L
                                                               else if pieceU /= Wall then Move (getPlayerID pacman) U
                                                                    else Move (getPlayerID pacman) R
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y) ;pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacNormalAux m (xg,yg) pacman | pospac == IsRight || pospac == IsRightUp = if pieceR /= Wall then Move (getPlayerID pacman) R
                                                                               else 
                                                                                   if pieceU /= Wall then Move (getPlayerID pacman) U
                                                                                   else if pieceL /= Wall then Move (getPlayerID pacman) L
                                                                                        else Move (getPlayerID pacman) D
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacNormalAux m (xg,yg) pacman | pospac == IsRightDown = if pieceD /= Wall then Move (getPlayerID pacman) D
                                                            else 
                                                                if pieceR /= Wall then Move (getPlayerID pacman) R
                                                                else if pieceU /= Wall then Move (getPlayerID pacman) U
                                                                     else Move (getPlayerID pacman) L
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y) ;pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacNormalAux m (xg,yg) pacman | pospac == IsUp = if pieceU /= Wall then Move (getPlayerID pacman) U
                                                     else 
                                                         if pieceL /= Wall then Move (getPlayerID pacman) L
                                                         else if pieceR /= Wall then Move (getPlayerID pacman) R
                                                              else Move (getPlayerID pacman) D
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacNormalAux m (xg,yg) pacman | pospac == IsDown = if pieceD /= Wall then Move (getPlayerID pacman) D
                                                       else 
                                                           if pieceR /= Wall then Move (getPlayerID pacman) R
                                                           else if pieceL /= Wall then Move (getPlayerID pacman) L
                                                                else Move (getPlayerID pacman) U
    where
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)

{-| IV.) A função calcPacNormal recebe um labirinto e dois jogadores: o pacman e um fantasma, respetivamente. Esta função
tem o mesmo objetivo que a sua auxiliar, mas esta utiliza os túneis como opção de fuga. 

Funções auxiliares na construção da calcPacNormal:
* even (pré-definida)
* length (pré-definida)
* calcPacNormalAux (função do ponto III)
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* tunnelCoordsOdd (função do módulo Tarefa2)
* tunnelCoordsEven (função do módulo Tarefa2)-}
calcPacNormal :: Maze -> Player -> Player -> Play 
calcPacNormal mz pac gh = if even (length mz) 
                          then if cs == dc || cs == db
                               then Move idPac R
                               else if cs == ec || cs == eb
                                    then Move idPac L
                                    else calcPacNormalAux mz gc pac 
                          else if cs == e 
                               then Move idPac L
                               else if cs == d 
                                   then Move idPac R
                                   else calcPacNormalAux mz gc pac

        where
            idPac = getPlayerID gh
            cs = getPlayerCoords pac
            gc = getPlayerCoords gh
            (e,d) = tunnelCoordsOdd mz
            (ec,dc,eb,db) = tunnelCoordsEven mz

{-| V.) A função goAfterFood recebe um labirinto e dois jogadores (pacman e um fantasma, respetivamente) e, com objetivo 
de ir em direção a comida, devolve a jogada mais adequada.

Funções auxiliares na construção da goAfterFood:
* calcPacNormal (função do ponto IV)
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* getPlayerOrientation (função do módulo types)
* takePiece (função do módulo Tarefa2)
-}
goAfterFood :: Maze -> Player -> Player -> Play 
goAfterFood m p g | getPlayerOrientation p == U = if pieceU == Food Little || pieceU == Food Big then Move pid U
                                                  else if pieceR == Food Little || pieceR == Food Big then Move pid R
                                                       else if pieceL == Food Little || pieceL == Food Big then Move pid L
                                                            else if pieceD == Food Little || pieceD == Food Big then Move pid D
                                                                 else calcPacNormal m p g 
                  | getPlayerOrientation p == D = if pieceD == Food Little || pieceD == Food Big then Move pid D
                                                  else if pieceR == Food Little || pieceR == Food Big then Move pid R
                                                       else if pieceL == Food Little || pieceL == Food Big then Move pid L
                                                            else if pieceU == Food Little || pieceU == Food Big then Move pid U
                                                                 else calcPacNormal m p g
                  | getPlayerOrientation p == L = if pieceL == Food Little || pieceL == Food Big then Move pid L
                                                  else if pieceU == Food Little || pieceU == Food Big then Move pid U
                                                       else if pieceD == Food Little || pieceD == Food Big then Move pid D
                                                            else if pieceR == Food Little || pieceR == Food Big then Move pid U
                                                                 else calcPacNormal m p g
                  | getPlayerOrientation p == R = if pieceR == Food Little || pieceR == Food Big then Move pid R
                                                  else if pieceD == Food Little || pieceD == Food Big then Move pid D
                                                       else if pieceU == Food Little || pieceU == Food Big then Move pid U
                                                            else if pieceL == Food Little || pieceR == Food Big then Move pid L
                                                                 else calcPacNormal m p g
    
    where (x,y) = getPlayerCoords p 
          pid = getPlayerID p 
          pieceL = takePiece m (x,y-1) 
          pieceR = takePiece m (x,y+1)
          pieceU = takePiece m (x-1,y) 
          pieceD = takePiece m (x+1,y)

{-| VI.) A função pacNormal recebe um estado, e decide se utiliza a função calcPacNormal (caso haja um fantasma
relativamente próximo ao pacman) com finalidade em fugir do fantasma mais próximo, ou a função goAfterFood (caso
o pacman esteja a uma distância considerável aos fantasmas) com finalidade em encontrar comida.

Funções auxiliares na construção da PacNormal:
* fst (pré-definida)
* closestGhost (função do ponto II)
* calcPacNormal (função do ponto IV)
* goAfterFood (função do ponto V)
* takePacman (função do módulo Tarefa2)
-}
pacNormal :: State -> Play 
pacNormal s@(State m ps _) | fst cg <= 5 = calcPacNormal m p (snd cg)
                           | otherwise = goAfterFood m p (snd cg)
    where cg = closestGhost s
          p = takePacman ps 


{-| VII.) A calcPacMegaAux recebe um labirinto, as coordenadas de um fantasma e um jogador (pacman). Conforme a posição relativa
do pacman ao fantasma mais próximo, a função irá retornar a melhor jogada de perseguição possível para o pacman. 

Funções auxiliares na construção da calcPacMegaAux:
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* takePiece (função do módulo Tarefa2)
* getRelativePacmanPosition (função do módulo Tarefa5)
-}
calcPacMegaAux:: Maze -> Coords -> Player -> Play
calcPacMegaAux m (xg,yg) pacman | pospac == IsLeft || pospac == IsLeftUp = if pieceR /= Wall then Move (getPlayerID pacman) R
                                                                           else 
                                                                               if pieceD /= Wall then Move (getPlayerID pacman) D
                                                                               else if pieceU /= Wall then Move (getPlayerID pacman) U
                                                                                    else Move (getPlayerID pacman) L
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacMegaAux m (xg,yg) pacman | pospac == IsLeftDown = if pieceR /= Wall then Move (getPlayerID pacman) R
                                                         else 
                                                             if pieceU /= Wall then Move (getPlayerID pacman) U
                                                             else if pieceL /= Wall then Move (getPlayerID pacman) L
                                                                  else Move (getPlayerID pacman) D
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacMegaAux m (xg,yg) pacman | pospac == IsRight || pospac == IsRightUp = if pieceL /= Wall then Move (getPlayerID pacman) L
                                                                             else 
                                                                                 if pieceD /= Wall then Move (getPlayerID pacman) D
                                                                                 else if pieceU /= Wall then Move (getPlayerID pacman) U
                                                                                      else Move (getPlayerID pacman) R
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacMegaAux m (xg,yg) pacman | pospac == IsRightDown = if pieceL /= Wall then Move (getPlayerID pacman) L
                                                          else 
                                                              if pieceU /= Wall then Move (getPlayerID pacman) U
                                                              else if pieceR /= Wall then Move (getPlayerID pacman) R
                                                                   else Move (getPlayerID pacman) D
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacMegaAux m (xg,yg) pacman | pospac == IsUp = if pieceU /= Wall then Move (getPlayerID pacman) D
                                                   else 
                                                       if pieceL /= Wall then Move (getPlayerID pacman) L
                                                       else if pieceR /= Wall then Move (getPlayerID pacman) R
                                                            else Move (getPlayerID pacman) U
    where 
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcPacMegaAux m (xg,yg) pacman | pospac == IsDown = if pieceD /= Wall then Move (getPlayerID pacman) U
                                                     else 
                                                         if pieceR /= Wall then Move (getPlayerID pacman) R
                                                         else if pieceL /= Wall then Move (getPlayerID pacman) L
                                                              else Move (getPlayerID pacman) D
    where
        (x,y) = getPlayerCoords pacman; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (xg,yg) (x,y);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)

{-| VIII.) A função calcChasePlay recebe um labirinto e dois jogadores: o pacman e um fantasma. Esta função tem o mesmo
objetivo da sua auxiliar, mas esta, utilizando a função worthTunnelPlay, verifica se é benéfico para o pacman deslocar-se
diretamente ao fantasma mais próximo ou deslocar-se até ao túnel mais próximo, com o objetivo de alcançar esse fantasma  
mais rapidamente.

Funções auxiliares na construção da choosePlay:
* odd (pré-definida)
* length (pré-definida)
* calcPacMegaAux (função do ponto VII)
* getPlayerCoords (função do módulo types)
* tunnelCoordsOdd (função do módulo Tarefa2)
* tunnelCoordsEven (função do módulo Tarefa2)
* worthTunnelPlay (função do módulo Tarefa5)
* closestTunnel (função do módulo Tarefa5)
-}
choosePlay :: Maze -> Player -> Player -> Play 
choosePlay m p g | worthTunnelPlay m p g = if odd (length m) then calcPacMegaAux m (closestTunnel gc [e,d]) p
                                              else calcPacMegaAux m (closestTunnel gc [ec,dc,eb,db]) p
                 | otherwise             = calcPacMegaAux m gc p 
    where (e,d) = tunnelCoordsOdd m 
          (ec,dc,eb,db) = tunnelCoordsEven m
          gc = getPlayerCoords g
          pc = getPlayerCoords p 


{-| IX.) A função calcPacMega recebe um labirinto e dois jogadores (o pacman e um fantasma). Esta função tem a mesma 
finalidade que a função choosePlay, apenas garante que quando o pacman se encontra num túnel, este irá utilizar-lo
caso seja benéfico para a preseguição do fantasma.

Funções auxiliares na construção da calcPacMega:
* even (pré-definida)
* length (pré-definida)
* choosePlay (função do ponto VIII)
* getPlayerCoords (função do módulo types)
* getPlayerID (função do módulo types)
* tunnelCoordsOdd (função do módulo Tarefa2)
* tunnelCoordsEven (função do módulo Tarefa2)
* worthTunnelPlay (função do módulo Tarefa5)
-}
calcPacMega :: Maze -> Player -> Player -> Play 
calcPacMega mz pac gh | worthTunnelPlay mz pac gh = if even (length mz) 
                                                        then if pc == dc || pc == db
                                                            then Move pid R
                                                            else if pc == ec || pc == eb
                                                                 then Move pid L
                                                                 else choosePlay mz pac gh
                                                        else if pc == e 
                                                            then Move pid L
                                                            else if pc == d 
                                                                then Move pid R
                                                                else choosePlay mz pac gh
                     | otherwise = choosePlay mz pac gh 
        where
            pid = getPlayerID pac
            pc = getPlayerCoords gh
            (e,d) = tunnelCoordsOdd mz
            (ec,dc,eb,db) = tunnelCoordsEven mz

{-| X.) A função pacMega recebe um estado e irá devolver a melhor jogada de perseguição do pacman em relação aos fantasmas.

Funções auxiliares na construção da pacMega:
* snd (pré-definida)
* closestGhost (função do ponto II)
* calcPacMega (função do ponto IX)
* takePacman (função do módulo Tarefa2)
-}
pacMega :: State -> Play 
pacMega s@(State m ps _) = calcPacMega m p (snd cg)
    where cg = closestGhost s
          p = takePacman ps 
 
{-| XI.) A função bot recebe um id e um estado, e irá devolver a jogada mais adequada, dependendo do modo em que os fantasmas
se encontram. 

Funções auxiliares na construção da bot:
* snd (pré-definida)
* takePacman (função do módulo Tarefa2)
* dropPacman (função do módulo Tarefa2)
* closestGhost (função do ponto II)
* pacNormal (função do ponto VI)
* pacMega (função do ponto X)
-}
bot :: Int -> State -> Maybe Play
bot id s@(State m ps _) | gm == Alive = Just (pacNormal s)
                        | gm == Dead = Just (pacMega s) 
    where p = takePacman
          gs = dropPacman ps
          cg = closestGhost s
          getGhostMode (Ghost (GhoState a b )) = b
          gm = getGhostMode (snd cg) 