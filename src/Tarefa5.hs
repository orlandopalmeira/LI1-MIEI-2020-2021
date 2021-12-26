{-| 
Module : Tarefa 5
Description : Tarefa 5 do Projeto da unidade curricular de /Laboratórios de informática I/
Copyright : Orlando José da Cunha Palmeira Palmeira <orlandopalmeira51@gmail.com>
            

== Relatório de execução da Tarefa 5

== Introdução
O objetivo desta tarefa é determinar as melhores jogadas para os fantasmas dependendo da posição deles em relação ao pacman bem
como do seu modo.

== Objetivos
Naturalmente, o objetivo desta função foi tentar obter as melhores jogadas possíveis para os fantasmas.
A estratégia que adotámos nesta tarefa foi de tentar colocar o fantasma o mais próximo possível do pacman caso esteja em
modo Alive e tentar colocá-lo o mais longe possível caso se encontre em modo Dead.
Para isso, através do uso de muitos "if's", a cada iteração, o fantasma tenta executar a melhor jogada possível em primeiro lugar.
Caso ele não se consiga deslocar devido à presença de uma parede, o fantasma tenta a segunda melhor alternativa e assim 
sucessivamente até se conseguir deslocar.
Um dos problemas que nos surgiu no desenvolvimento desta tarefa foi no caso em que os fantasmas se encontram dentro casa. Para
a resolução desse problema, existe uma função (housePlay) que determina as jogadas para o fantasma conseguir sair da casa.
É de referir que, para nos facilitar na obtenção da posição relativa de cada fantasma com o pacman, foi criado um dataType
designado PosPac que tem todas as possibilidades possíveis para a posição relativa.

== Discussão e conclusão
Após os testes desta tarefa que foram visualizados através do uso da função main do módulo main, conseguímos verificar
que os fantasmas têm uma boa estratégia de perseguição e de fuga.
È importante mencionar que, quando os fantasmas estão em fuga, pode parecer que não conseguem fugir tão bem quanto perseguem.
No entanto, isto deve-se ao facto de haver a redução da velocidade dos fantasmas que dá uma maior facilidade ao pacman de os
alcançar e, posteriormente, atacá-los.
-}

module Tarefa5 where

import Tarefa2 (takePiece,dropPacman,takePacman,tunnelCoordsEven,tunnelCoordsOdd,getPlayer,getGhostMode)
import Types
    ( getPlayerCoords,
      getPlayerID,
      Coords,
      GhostMode(Dead),
      Maze,
      Orientation(D, L, R, U),
      Piece(Wall),
      Play(..),
      Player,
      State(State, playersState, maze) )

data PosPac = Equal | IsLeft | IsRight | IsUp | IsDown | IsLeftUp | IsLeftDown | IsRightUp | IsRightDown deriving Eq
-- posição à esquerda,à direita, acima, abaixo,esquerda acima, esquerda abaixo, direita acima, direita abaixo

data Sector = CloseToLeftTunnel | CloseToRightTunnel | FarFromTunnels deriving Eq

{-| I.) A função coordsHouseOdd recebe um labirinto de largura ímpar e devolve as coordenadas das áreas Empty que estão dentro 
da casa dos fantasmas, inclusive a entrada da casa.

Funções auxiliares na construção da coordsHouseOdd:
* odd (pré-definida)
* length (pré-definida)
* div (pré-definida)
* head (pré-definida)
 -}
coordsHouseOdd :: Maze -> [Coords] -- largura impar
coordsHouseOdd maze = if odd lm then [d1,d2,d3,c1,c2,c3,c4,c5,c6,c7] 
                      else [d1',d2',d3',c1',c2',c3',c4',c5',c6',c7']
    where
        lm = length maze; lh = length (head maze)
        -- altura impar
        d1 = (div lm 2 -1, div lh 2 -1); d2 = (div lm 2 -1, div lh 2); d3 = (div lm 2 -1, div lh 2 +1)
        c1 = (div lm 2, div lh 2 -3); c2 = (div lm 2, div lh 2 -2); c3 = (div lm 2, div lh 2 -1); c4 = (div lm 2, div lh 2);
        c5 = (div lm 2, div lh 2 +1); c6 = (div lm 2, div lh 2 +2); c7 = (div lm 2, div lh 2 +3)
        -- altura par
        d1' = (div lm 2 -2, div lh 2 -1); d2' = (div lm 2 -2, div lh 2); d3' = (div lm 2 -2, div lh 2 +1)
        c1' = (div lm 2 -1, div lh 2 -3); c2' = (div lm 2 -1, div lh 2 -2); c3' = (div lm 2 -1, div lh 2 -1); c4' = (div lm 2 -1, div lh 2);
        c5' = (div lm 2 -1, div lh 2 +1); c6' = (div lm 2 -1, div lh 2 +2); c7' = (div lm 2 -1, div lh 2 +3)

{-| II.) A função coordsHouseOdd recebe um labirinto de largura par e devolve as coordenadas das áreas Empty que estão dentro
 da casa dos fantasmas, inclusive a entrada da casa. 

Funções auxiliares na construção da coordsHouseEven:
* even (pré-definida)
* length (pré-definida)
* div (pré-definida)
* head (pré-definida)
 -}
coordsHouseEven :: Maze -> [Coords] -- largura par
coordsHouseEven maze = if even lm then [d1,d2,c1,c2,c3,c4,c5,c6] else [d1',d2',c1',c2',c3',c4',c5',c6']
    where
        lm = length maze; lh = length (head maze)
        -- altura par
        d1 = (div lm 2 -2, div lh 2 -1); d2 = (div lm 2 -2, div lh 2);
        c1 = (div lm 2 -1, div lh 2 -3); c2 = (div lm 2 -1, div lh 2 -2); c3 = (div lm 2 -1, div lh 2 -1);
        c4 = (div lm 2 -1, div lh 2);    c5 = (div lm 2 -1, div lh 2 +1); c6 = (div lm 2 -1, div lh 2 +2)
        -- altura impar
        d1' = (div lm 2, div lh 2 -1); d2' = (div lm 2, div lh 2) 
        c1' = (div lm 2 +1, div lh 2 -3); c2' = (div lm 2 +1, div lh 2 -2); c3' = (div lm 2 +1, div lh 2 -1);
        c4' = (div lm 2 +1, div lh 2); c5' = (div lm 2 +1, div lh 2 +1); c6' = (div lm 2 +1, div lh 2 +2) 

{-| III.) A função topHouseOdd recebe um labirinto de largura ímpar e devolve as coordenadas que estão acima das coordenadas da
entrada da casa dos fantasmas.

Funções auxiliares na construção da topHouseOdd:
* odd (pré-definida)
* length (pré-definida)
* div (pré-definida)
* head (pré-definida)
-}
topHouseOdd :: Maze -> [Coords]
topHouseOdd maze = if odd lm then [c1,c2,c3] else [c1',c2',c3']
    where
        lm = length maze; lh = length (head maze)
        -- altura impar
        c1 = (div lm 2 -2, div lh 2 -1); c2 = (div lm 2 -2, div lh 2); c3 = (div lm 2 -2, div lh 2 +1);
        -- altura par
        c1' = (div lm 2 -3, div lh 2 -1); c2' = (div lm 2 -3, div lh 2); c3' = (div lm 2 -3, div lh 2 +1)
{-| IV.) A função topHouseOdd recebe um labirinto de largura par e devolve as coordenadas que estão acima das coordenadas da
entrada da casa dos fantasmas.

Funções auxiliares na construção da tpoHouseEven:
* odd (pré-definida)
* length (pré-definida)
* div (pré-definida)
* head (pré-definida)
-}
topHouseEven :: Maze -> [Coords]
topHouseEven maze = if odd lm then [c1,c2] else [c1',c2'] 
    where
        lm = length maze; lh = length (head maze);
        -- altura impar
        c1 = (div lm 2 -2, div lh 2 -1); c2 = (div lm 2 -2, div lh 2)
        -- altura par
        c1' = (div lm 2 -3, div lh 2 -1); c2' = (div lm 2 -3, div lh 2)

{-| V.) A função insideHouse recebe um labirinto e um fantasma e verifica se esse fantasma se encontra dentro da casa, 
na entrada da casa ou nas coordenadas acima das da entrada da casa.

Funções auxiliares na construção da insideHouse:
* odd (pré-definida)
* length (pré-definida)
* head (pré-definida)
* getPlayerCoords (função do módulo types)
* coordsHouseOdd (função do ponto I)
* coordsHouseEven (função do ponto II)
* topHouseOdd (função do ponto III)
* topHouseEven (função do ponto IV)
-}
insideHouse :: Maze -> Player -> Bool
insideHouse mz ghost | odd (length (head mz)) = cs `elem` l1
                     | otherwise = cs `elem` l2 
        where
            cs = getPlayerCoords ghost
            l1 = coordsHouseOdd mz ++ topHouseOdd mz
            l2 = coordsHouseEven mz ++ topHouseEven mz

{-| VI.) A função housePlayAux recebe um estado e um fantasma e, caso o fantasma esteja estritamente no interior da casa dos
fantasmas, a função irá retornar a melhor jogada para que o fantasma saia de dentro da casa

Funções auxiliares na construção da housePlayAux:
* odd (pré-definida)
* length (pré-definida)
* head (pré-definida)
* getPlayerCoords (função do módulo types)
* getPlayerID (função do módulo types)
* coordsHouseOdd (função do ponto I)
* coordsHouseEven (função do ponto II)
-}
housePlayAux :: State -> Player -> Play 
housePlayAux state ghost | odd (length (head mz)) = if c == d1 || c == d2 || c == d3 || c == c3 || c == c4 || c == c5
                                                    then Move id U
                                                    else if c == c1 || c == c2 then Move id R else Move id L
                         | otherwise = if c == d1' || c == d2' || c == c3' || c == c4'
                                       then Move id U
                                       else if c == c1' || c == c2' then Move id R else Move id L
    where
        mz = maze state
        c = getPlayerCoords ghost
        id = getPlayerID ghost
        [d1,d2,d3,c1,c2,c3,c4,c5,_,_] = coordsHouseOdd mz
        [d1',d2',c1',c2',c3',c4',_,_] = coordsHouseEven mz

{-| VII.) A função housePlay recebe um estado e um fantasma e o objetivo é o mesmo da função housePlayAux (ponto VI). A única
diferença desta função para a sua auxiliar é que esta verifica se o fantasma se encontra fora da casa mas se ainda se encontra 
nas coordenadas que se encontram acima da entrada da casa e, nesse caso, irá retornar a melhor jogada para o fantasma.

Funções auxiliares na construção da housePlay:
* even (pré-definida)
* length (pré-definida)
* head (pré-definida)
* elem (pré-definida)
* getPlayerCoords (função do módulo types)
* getPlayerID (função do módulo types)
* coordsHouseOdd (função do ponto I)
* coordsHouseEven (função do ponto II)
* getGhostMode (função do módulo Tarefa2)
* getRelativePacmanPosition (função do ponto VII)
* takePacman (função do módulo Tarefa2)
* topHouseOdd (função do ponto III)
* topHouseEven (função do ponto IV)
* housePlayAux (função do ponto VI)
 -}
housePlay :: State -> Player -> Play 
housePlay state ghost | even lh = if c `elem` l1 
                                  then if gMode == Dead 
                                       then if rp == IsLeftUp || rp == IsLeftDown || rp == IsLeft
                                            then Move (getPlayerID ghost) R
                                            else Move (getPlayerID ghost) L
                                       else if rp == IsLeftUp || rp == IsLeftDown || rp == IsLeft
                                            then Move (getPlayerID ghost) L
                                            else Move (getPlayerID ghost) R
                                  else housePlayAux state ghost
                      | otherwise = if c `elem` l2
                                    then if gMode == Dead 
                                         then if rp == IsLeftUp || rp == IsLeftDown || rp == IsLeft
                                              then Move (getPlayerID ghost) R
                                              else Move (getPlayerID ghost) L
                                         else if rp == IsLeftUp || rp == IsLeftDown || rp == IsLeft
                                              then Move (getPlayerID ghost) L
                                              else Move (getPlayerID ghost) R
                                    else housePlayAux state ghost
    where
        gMode = getGhostMode ghost
        rp = getRelativePacmanPosition c (getPlayerCoords pacman);
        c = getPlayerCoords ghost;
        mz = maze state; lh = length (head mz); lm = length mz;
        pacman = takePacman (playersState state);
        l1@[c1,c2] = topHouseEven mz;
        l2@[c1',c2',c3'] = topHouseOdd mz;

{-| VIII.) A função getPlayerSector recebe um jogador e um labirinto e devolve o setor do mapa em que ele se encontra. Isto é,
perto do túnel esquerdo, perto do túnel direito, ou longe de ambos os túneis.-}
getPlayerSector :: Player -> Maze -> Sector
getPlayerSector p m | (x >= c' && x <= (c - c') && y <= l' ) = CloseToLeftTunnel
                    | (x >= c' && x <= (c - c') && y >= (l - l')) = CloseToRightTunnel
                    | otherwise = FarFromTunnels
    where (x,y) = getPlayerCoords p 
          c = (length) m - 1
          l = (length (head m)) - 1
          c' = div c 3 
          l' = div l 3

{-| IX.) A função getRelativePacmanPosition recebe as coordenadas de dois jogadores, o fantasma e o pacman, respetivamente.
A função irá comparar as coordenadas de ambos e irá retornar a posição relativa do pacman recebido em relação ao fantasma
recebido.
-}
getRelativePacmanPosition :: Coords -> Coords -> PosPac
getRelativePacmanPosition (xg,yg) (xp,yp) | xp == xg && yp < yg = IsLeft
                                          | xp == xg && yp > yg = IsRight
                                          | yp == yg && xp < xg = IsUp
                                          | yp == yg && xp > xg = IsDown
                                          | xp < xg && yp < yg  = IsLeftUp
                                          | xp > xg && yp < yg  = IsLeftDown
                                          | xp < xg && yp > yg  = IsRightUp
                                          | xp > xg && yp > yg  = IsRightDown
                                          | xp == xg && yp == yg= Equal

{-| X.) A função worthTunnelPlay recebe um labirinto e dois jogadores, o pacman e o fantasma. Esta irá verificar se
vale a pena para o fantasma utilizar o túnel para ir de encontro ao pacman, isto é, se o fantasma e o pacman se 
encontram perto de túneis opostos.
Funções auxiliares na construção da worthTunnelPlay:
* getPlayerSector (função do ponto VIII)
-}
worthTunnelPlay :: Maze -> Player -> Player -> Bool
worthTunnelPlay m p g = (((getPlayerSector p m) == CloseToLeftTunnel) && ((getPlayerSector g m) == CloseToRightTunnel))
                      || (((getPlayerSector p m) == CloseToRightTunnel) && ((getPlayerSector g m) == CloseToLeftTunnel))

{-| XI.) A função closestTunnel recebe as coordenadas de um jogador e a lista de coordenadas dos túneis e irá devolver 
as coordenadas do túnel mais próximo.-}
closestTunnel :: Coords -> [Coords] -> Coords 
closestTunnel gc cs = snd $ minimum x
    where dist (a,b) (c,d) = ((abs (a - c)) + (abs (b - d)), (c,d))
          x = map (dist gc) cs

{-| XII.) A calcChasePlayAux recebe um labirinto, as coordenadas do pacman e um jogador (fantasma). Conforme a posição relativa
do pacman em relação ao fantasma, a função irá retornar a melhor jogada de perseguição possível para o fantasma recebido. 

Funções auxiliares na construção da calcChasePlayAux:
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* takePiece (função do módulo Tarefa2)
* getRelativePacmanPosition (função do ponto VII)-}
calcChasePlayAux :: Maze -> Coords -> Player -> Play 
calcChasePlayAux m (xp,yp) ghost | pospac == IsLeft || pospac == IsLeftUp = if pieceL /= Wall then Move (getPlayerID ghost) L
                                                                              else 
                                                                                  if pieceU /= Wall then Move (getPlayerID ghost) U
                                                                                  else if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                                       else Move (getPlayerID ghost) D
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (xp,yp) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcChasePlayAux m (xp,yp) ghost | pospac == IsLeftDown = if pieceD /= Wall then Move (getPlayerID ghost) D
                                                          else 
                                                              if pieceL /= Wall then Move (getPlayerID ghost) L
                                                              else if pieceU /= Wall then Move (getPlayerID ghost) U
                                                                   else Move (getPlayerID ghost) R
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (xp,yp);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcChasePlayAux m (xp,yp) ghost | pospac == IsRight || pospac == IsRightUp = if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                               else 
                                                                                   if pieceU /= Wall then Move (getPlayerID ghost) U
                                                                                   else if pieceL /= Wall then Move (getPlayerID ghost) L
                                                                                        else Move (getPlayerID ghost) D
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (xp,yp) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcChasePlayAux m (xp,yp) ghost | pospac == IsRightDown = if pieceD /= Wall then Move (getPlayerID ghost) D
                                                            else 
                                                                if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                else if pieceL /= Wall then Move (getPlayerID ghost) L
                                                                     else Move (getPlayerID ghost) U
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (xp,yp);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcChasePlayAux m (xp,yp) ghost | pospac == IsUp = if pieceU /= Wall then Move (getPlayerID ghost) U
                                                     else 
                                                         if pieceR /= Wall then Move (getPlayerID ghost) R
                                                         else if pieceL /= Wall then Move (getPlayerID ghost) L
                                                              else Move (getPlayerID ghost) D
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (xp,yp);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcChasePlayAux m (xp,yp) ghost | pospac == IsDown = if pieceD /= Wall then Move (getPlayerID ghost) D
                                                       else 
                                                           if pieceL /= Wall then Move (getPlayerID ghost) L
                                                           else if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                else Move (getPlayerID ghost) U
    where
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (xp,yp);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcChasePlayAux _ _ ghost = Move (getPlayerID ghost) R

{-| XIII.) A função calcChasePlay recebe um labirinto e dois jogadores: o pacman e um fantasma. Esta função tem o mesmo
objetivo da sua auxiliar, mas esta, utilizando a função worthTunnelPlay, verifica se o fantasma deve deslocar-se 
diretamente ao pacman ou deslocar-se até ao túnel mais próximo, com o objetivo de alcançar o pacman mais rapidamente.

Funções auxiliares na construção da calcScatterPlayAux:
* odd (pré-definida)
* length (pré-definida)
* calcChasePlayAux (função do ponto XII)
* getPlayerCoords (função do módulo types)
* tunnelCoordsOdd (função do módulo Tarefa2)
* tunnelCoordsEven (função do módulo Tarefa2)
* worthTunnelPlay (função do ponto X)
* closestTunnel (função do ponto XI)-}
calcChasePlay :: Maze -> Player -> Player -> Play 
calcChasePlay m p g | worthTunnelPlay m p g = if odd (length m) then calcChasePlayAux m (closestTunnel gc [e,d]) g
                                              else calcChasePlayAux m (closestTunnel gc [ec,dc,eb,db]) g
                    | otherwise             = calcChasePlayAux m pc g
    where (e,d) = tunnelCoordsOdd m 
          (ec,dc,eb,db) = tunnelCoordsEven m
          gc = getPlayerCoords g
          pc = getPlayerCoords p 

{-| XIV.) A função tunnelPlay recebe um labirinto e dois jogadores (pacman e fantasma, respetivamente) e irá garantir que,
caso o fantasma se encontre no túnel, este o utilize ou não, conforme a sua conveniência. 

Funções auxiliares na construção da tunnelPlay:
* even (pré-definida)
* length (pré-definida)
* calcChasePlay (função do ponto XIII)
* getPlayerCoords (função do módulo types)
* getPlayerID (função do módulo types)
* tunnelCoordsOdd (função do módulo Tarefa2)
* tunnelCoordsEven (função do módulo Tarefa2)
* worthTunnelPlay (função do ponto X)
-}
tunnelPlay :: Maze -> Player -> Player -> Play
tunnelPlay mz pac gh | worthTunnelPlay mz pac gh = if even (length mz) 
                                                       then if cs == dc || cs == db
                                                            then Move idGh R
                                                            else if cs == ec || cs == eb
                                                                 then Move idGh L
                                                                 else calcChasePlay mz pac gh
                                                        else if cs == e 
                                                            then Move idGh L
                                                            else if cs == d 
                                                                then Move idGh R
                                                                else calcChasePlay mz pac gh
                     | otherwise = calcChasePlay mz pac gh 
        where
            idGh = getPlayerID gh
            cs = getPlayerCoords gh
            (e,d) = tunnelCoordsOdd mz
            (ec,dc,eb,db) = tunnelCoordsEven mz

{-| XV.) A função chaseModeAux recebe um estado e o id de um fantasma e irá aplicar a função tunnelPlay ao fantasma 
cujo id foi fornecido.

Funções auxiliares na construção da scatterMode:
* getPlayerID (função do módulo Types)
* takePacman (função do módulo Tarefa2)
* tunnelPlay (função do ponto XIV)
-}
chaseModeAux :: State -> Int -> Play 
chaseModeAux state id = tunnelPlay (maze state) pacman ghost
    where 
        ghost = getPlayer id (playersState state)
        pacman = takePacman (playersState state)

{-| XVI.) A função chaseMode recebe um estado e o id de um fantasma e decide consoante a posição do fantasma associado
ao id fornecido se irá utilizar a função insideHouse (caso este se encontre na casa dos fantasmas) ou a função chaseModeAux
(caso este se encontre fora da casa dos fantasmas).
Funções auxiliares na construção da chaseMode:
* insideHouse (função do ponto V)
* getPlayer (função do módulo Tarefa2)
* housePlay (função do módulo Tarefa2)
* chaseModeAux (função do ponto XV)
-}
chaseMode :: State -> Int -> Play 
chaseMode state id | insideHouse mz ghost = housePlay state ghost
                   | otherwise = chaseModeAux state id
    where
        mz = maze state
        ghost = getPlayer id (playersState state)

{-| XVII.) A calcScatterPlayAux recebe um labirinto e dois jogadores (o pacman e o fantasma, respetivamente). 
Conforme a posição relativa do pacman em relação ao fantasma, a função irá retornar a melhor jogada de fuga possível para o 
fantasma recebido.

Funções auxiliares na construção da calcScatterPlayAux:
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* takePiece (função do módulo Tarefa2)
* getRelativePacmanPosition (função do ponto VII)
 -}
calcScatterPlayAux :: Maze -> Player -> Player -> Play
calcScatterPlayAux m pacman ghost | pospac == IsLeft || pospac == IsLeftUp = if pieceD /= Wall then Move (getPlayerID ghost) D
                                                                              else 
                                                                                  if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                                  else if pieceU /= Wall then Move (getPlayerID ghost) U
                                                                                       else Move (getPlayerID ghost) L
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (getPlayerCoords pacman) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcScatterPlayAux m pacman ghost | pospac == IsLeftDown = if pieceU /= Wall then Move (getPlayerID ghost) U
                                                          else 
                                                              if pieceR /= Wall then Move (getPlayerID ghost) R
                                                              else if pieceL /= Wall then Move (getPlayerID ghost) L
                                                                   else Move (getPlayerID ghost) D
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (getPlayerCoords pacman);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcScatterPlayAux m pacman ghost | pospac == IsRight || pospac == IsRightUp = if pieceD /= Wall then Move (getPlayerID ghost) D
                                                                               else 
                                                                                   if pieceL /= Wall then Move (getPlayerID ghost) L
                                                                                   else if pieceU /= Wall then Move (getPlayerID ghost) U
                                                                                        else Move (getPlayerID ghost) R
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (getPlayerCoords pacman) ; pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcScatterPlayAux m pacman ghost | pospac == IsRightDown = if pieceU /= Wall then Move (getPlayerID ghost) U
                                                            else 
                                                                if pieceL /= Wall then Move (getPlayerID ghost) L
                                                                else if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                     else Move (getPlayerID ghost) D
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (getPlayerCoords pacman);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcScatterPlayAux m pacman ghost | pospac == IsUp = if pieceD /= Wall then Move (getPlayerID ghost) D
                                                     else 
                                                         if pieceL /= Wall then Move (getPlayerID ghost) L
                                                         else if pieceR /= Wall then Move (getPlayerID ghost) R
                                                              else Move (getPlayerID ghost) U
    where 
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (getPlayerCoords pacman);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcScatterPlayAux m pacman ghost | pospac == IsDown = if pieceU /= Wall then Move (getPlayerID ghost) U
                                                       else 
                                                           if pieceL /= Wall then Move (getPlayerID ghost) L
                                                           else if pieceR /= Wall then Move (getPlayerID ghost) R
                                                                else Move (getPlayerID ghost) D
    where
        (x,y) = getPlayerCoords ghost; pieceL = takePiece m (x,y-1); pieceR = takePiece m (x,y+1);
         pospac = getRelativePacmanPosition (x,y) (getPlayerCoords pacman);pieceU = takePiece m (x-1,y); pieceD = takePiece m (x+1,y)
calcScatterPlayAux _ _ ghost = Move (getPlayerID ghost) R

{-| XVIII.) A função calcScatterPlay recebe um labirinto e dois jogadores (o pacman e o fantasma, respetivamente) e tem o mesmo 
objetivo que a sua função auxiliar (calcScatterPlayAux - ponto XV). A única diferença desta função para a sua auxiliar 
é que este irá verificar a melhor jogada para quando o fantasma se encontra nalgum túnel to labirinto e caso isso não aconteça,
ela irá utilizar a função auxiliar referida anteriormente.

Funções auxiliares na construção da calcScatterPlay:
* even (pré-definida)
* length (pré-definida)
* calcScatterPlayAux (função do ponto XV)
* getPlayerID (função do módulo types)
* getPlayerCoords (função do módulo types)
* tunnelCoordsOdd (função do módulo Tarefa2)
* tunnelCoordsEven (função do módulo Tarefa2)
-}
calcScatterPlay :: Maze -> Player -> Player -> Play
calcScatterPlay mz pac gh = if even (length mz) 
                            then if cs == ec || cs == eb
                                 then Move idGh R
                                 else if cs == dc || cs == db
                                      then Move idGh L
                                      else calcScatterPlayAux mz pac gh
                            else if cs == e 
                                 then Move idGh R
                                 else if cs == d 
                                      then Move idGh L
                                      else calcScatterPlayAux mz pac gh
        where
            idGh = getPlayerID gh
            cs = getPlayerCoords gh
            (e,d) = tunnelCoordsOdd mz
            (ec,dc,eb,db) = tunnelCoordsEven mz

{-| XIX.) A função scatterModeAux recebe um estado e o id de um fantasma e irá utilizar a função calcScatterPlay 
(função do ponto XVI) para efetuar o cálculo da melhor jogada para o fantasma assciado ao id fornecido.

Funções auxiliares na construção da scatterModeAux:
* calcScatterPlay (função do ponto XVI)
* getPlayer (função do módulo Tarefa2)
* takePacman (função do módulo Tarefa2)
-}
scatterModeAux :: State -> Int -> Play -- ORLANDO
scatterModeAux state id = calcScatterPlay (maze state) pacman ghost
    where 
        ghost = getPlayer id (playersState state)
        pacman = takePacman (playersState state)

{-| XX.) A função scatterMode recebe um estado e o id de um fantasma e tem o mesmo objetivo que a sua função auxiliar 
(scatterModeAux - ponto XIX). A única diferença desta função para a sua auxiliar é que esta verifica se o fantasma se encontra
nas coordenadas associadas à casa dos fantasmas e, nesse caso, utiliza a função housePlay.

Funções auxiliares na construção da scatterMode:
* insideHouse (função do ponto V)
* getPlayer (função do módulo Tarefa2)
* housePlay (função do módulo Tarefa2)
-}
scatterMode :: State -> Int -> Play -- ORLANDO
scatterMode state id | insideHouse mz ghost = housePlay state ghost
                     | otherwise = scatterModeAux state id
    where
        mz = maze state
        ghost = getPlayer id (playersState state)

{-| XXI.) A função attributePlays recebe um estado, uma lista de jogadores (fantasmas) e conforme o estado de cada fantasma 
(Dead ou Alive) irá determinar para cada fantasma a melhor jogada possível aplicando-lhes ou a função chaseMode ou a função
scatterMode.

Funções auxiliares na construção da attributePlays:
* scatterMode (função do ponto XX)
* chaseMode (função do ponto XVI)
* getGhostMode (função do módulo Tarefa2)
-}
attributePlays :: State -> [Player] -> [Play]
attributePlays _ [] = []
attributePlays state (p:ps) | mode == Dead = scatterMode state (getPlayerID p) : attributePlays state ps
                            | otherwise    = chaseMode state (getPlayerID p) : attributePlays state ps
    where
        mode = getGhostMode p
{-| XXII.) A função ghostPlay recebe um estado e, através da utilização das funções anteriores, irá retornar as melhores jogadas 
para todos os fantasmas num determinado instante.

Funções auxiliares na construção da ghostPlay:
* attributePlays (função do ponto XXI)
* dropPacman (função do módulo Tarefa2)
-}
ghostPlay :: State -> [Play] 
ghostPlay state@(State mz players _) = attributePlays state ghosts
    where
         ghosts = dropPacman players