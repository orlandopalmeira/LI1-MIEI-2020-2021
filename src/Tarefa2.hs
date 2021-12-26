{-|
Module : Tarefa 2
Description : Tarefa 2 do Projeto da unidade curricular de /Laboratórios de informática I/
Copyright : Orlando José da Cunha Palmeira Palmeira <orlandopalmeira51@gmail.com>
            


== Relatório de execução da Tarefa 2
 
== Introdução
Nesta tarefa pretende-se alterar um estado do jogo através de uma jogada num jogador movendo-o ou alterando a sua orientação.

== Objetivos
O único objetivo que tivemos com esta tarefa foi de tentar aproximar o mais possível o nosso jogo ao original. Para esse efeito
desenvolvemos muitas funções, cada uma para executar cada verificação e ação após a jogada ser efetuada.
É de referir que esta tarefa foi a que mais problemas e obstáculos trouxe para o desenvolvimento do projeto uma vez que
é ela que controla as pontuações e todos os dados dos jogadores bem como determina o que deve ser feito conforme o modo dos jogadores.
A estratégia mais básica para resolver a maioria dos problemas foi, na maior parte da tarefa, repartir problemas grandes em problemas
mais pequenos que são facilmente resolvidos com funções auxiliares.
Outro problema que ocorreu nesta tarefa é que muitas das funções, em alguns casos, não podiam resultar nenhum resultado uma vez que
para esses casos a função não tinha que retornar nada. Para resolver esse problema optamos por fazer com que essas funções
também devolvessem flags numéricas que indicavam o tipo de resultado dessas funções. 

== Discussão e Conclusão
Após muitas correções desta tarefa, no final, conseguimos fazer que esta se tornasse totalmente funcional e com que o código
fosse bem sucedido a cumprir com tudo o que foi solicitado.
Também é de referir que esta tarefa foi atualizada inúmeras vezes mesmo após a sua entrega na 1ª fase do trabalho.
-}
module Tarefa2 where

import Types
    ( getPacmanMode,
      getPlayerCoords,
      getPlayerID,
      getPlayerOrientation,
      replaceElemInMaze,
      setPlayerCoords,
      Coords,
      FoodType(Big, Little),
      GhoState(GhoState),
      GhostMode(..),
      Maze,
      Mouth(Open, Closed),
      Orientation(..),
      PacMode(..),
      PacState(PacState),
      Piece(..),
      Play(..),
      Player(..),
      State(..), Corridor )
import FileUtils ( loadMaze )

state1 = loadMaze "../mazes/1.txt"
state2 = loadMaze "../mazes/2.txt"
state3 = loadMaze "../mazes/3.txt"
state4 = loadMaze "../mazes/4.txt"
state5 = loadMaze "../mazes/5.txt"
state6 = loadMaze "../mazes/6.txt"

{-| I.) A função checkIDPlayer tem dois inputs que são, respetivamente, um número inteiro e uma lista de players. O output será um booleano em que se houver um jogador com o id igual ao colocado na input devolve True, caso contrário devolve False.
	
Funções auxiliares utilizadas na construção da função checkIDPlayer:
* getPlayerID (função definida no módulo Types providenciada pelos docentes)	
-}
checkIDPlayer :: Int -> [Player] -> Bool
checkIDPlayer _ [] = False
checkIDPlayer id (h:t) | getPlayerID h == id = True
                       | otherwise = checkIDPlayer id t

{-| II.) A função getPlayer tem dois inputs que são, respetivamente, um nº inteiro e uma lista de players e o output será um jogador dessa lista cujo ID é igual ao nº dado no input.
	
Funções auxiliares utilizadas na construção da função getPlayer:
* getPlayerID (função definida no módulo Types providenciada pelos docentes)
-}
getPlayer :: Int -> [Player] -> Player
getPlayer _ [] = undefined 
getPlayer id (h:t) | getPlayerID h == id = h
                   | otherwise = getPlayer id t

{-| III.) A função takePiece tem dois inputs que são, respetivamente, um labirinto e coordenadas. O output será a peça do labirinto fornecido localizada nas coordenadas dadas no input
	
Funções auxiliares utilizadas na construção da função takePiece:
* (!!) (pré-definida)
-}
takePiece :: Maze -> Coords -> Piece
takePiece m (x,y) = let c = (!!) m x
                    in (!!) c y

{-| IV.) A função removePlayer tem dois inputs que são, respetivamente, um player e uma lista de players e retira o player dado no input da lista.
Funções auxiliares utilizadas na construção da função removePlayer:
* getPlayerID (função definida no módulo Types providenciada pelos docentes)
-}
removePlayer :: Player -> [Player] -> [Player]
removePlayer _ [] = []
removePlayer player (h:t) | getPlayerID h == getPlayerID player = t
                          | otherwise = h : removePlayer player t

{-| V.) A função changeGhostsStates tem um input que é uma lista de players, em particular, uma lista de fantasmas em que altera o modo de todos os fantasmas para Dead.
-}
changeGhostsStates :: [Player] -> [Player]
changeGhostsStates [] = []
changeGhostsStates ((Ghost (GhoState (id,c,v,o,p,l) _)):t) = Ghost (GhoState (id,c,0.5,o,p,l) Dead):changeGhostsStates t

{-| VI.) A função putGhostInHouse tem dois inputs que são, respetivamente, um labirinto e um jogador, em particular, um fantasma, em que coloca um determinado fantasma dentro da casa.
Funções auxiliares utilizadas na construção da função changeGhostsStates:
* even (pré-definida)
* length (pré-definida)
* head (pré-definida)
* div (pré-definida)
-}
putGhostInHouse :: Maze -> Player -> Player
putGhostInHouse maze (Ghost (GhoState (id,_,v,o,p,l) _ )) | even (length maze) = if even (length (head maze)) -- altura par
                                                                                    then Ghost (GhoState (id,(xap,ycp),v,o,p,l) Alive) -- comprimento par
                                                                                 else Ghost (GhoState (id,(xap,yci),v,o,p,l) Alive) -- comprimento impar
                                                          | otherwise = if even (length (head maze))  -- altura impar
                                                                           then Ghost (GhoState (id,(xai,ycp),v,o,p,l) Alive) -- comprimento par
                                                                        else Ghost (GhoState (id,(xai,yci),v,o,p,l) Alive) -- comprimento impar
        where 
            xap = div (length maze) 2 -1
            ycp = div (length (head maze)) 2 -1
            xai = div (length maze) 2
            yci = div (length (head maze)) 2 

{-| VII.) A função updateGhosts tem três inputs que são, respetivamente, um labirinto, um nº inteiro e uma lista de players, em particular, uma lista de fantasmas e, de acordo com a flag recebida (nº inteiro do input), esta função tem diferentes maneiras de atuar:
	
	1. Se a flag for igual a 100, a função não altera nada nos fantasmas.
	2. Se a flag for igual a 101, a função altera o modo de todos os fantasmas para Dead.
	3. Se a flag for igual ao ID de um fantasma, então é porque esse fantasma estava em modo Dead e foi comido pelo Pacman e é retornado à casa.
	
Funções auxiliares utilizadas na construção da função updateghosts:
* changeGhostsStates - função do ponto V
* putGhostsInHouse - função do ponto VI
-}
updateGhosts :: Maze -> Int -> [Player] -> [Player]
updateGhosts _ _ [] = []
updateGhosts maze flag players@ ((Ghost (GhoState (id,c,v,o,p,l) m )):t) | flag == 100 = players
                                                                         | flag == 101 = changeGhostsStates players
                                                                         | otherwise = if id == flag
                                                                                          then putGhostInHouse maze (Ghost (GhoState (id,c,v,o,p,l) m )):t
                                                                                        else Ghost (GhoState (id,c,v,o,p,l) m ) : updateGhosts maze flag t


{-| VIII.) A função updateStatePacMan tem três inputs que são, respetivamente, um par de coordenadas, um player, em particular, o pacman, e uma peça. O output será um tuplo cuja 1ª componente é o pacman com o seu estado atualizado e a 2ª componente é uma flag que servirá como auxiliar para informar outra função (updateGhosts - ponto VII) de como deve atuar.
-}
updateStatePacMan :: Coords -> Player -> Piece -> (Player, Int)
updateStatePacMan newCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) Empty | c == Closed = (Pacman (PacState (x,newCoords,z,t,h,l) q Open  d ), 100)
                                                                           | otherwise = (Pacman (PacState (x,newCoords,z,t,h,l) q Closed d ), 100)
updateStatePacMan newCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (Food Little) | c == Closed = (Pacman (PacState (x,newCoords,z,t,h+1,l) q Open d ), 100)
                                                                                   | otherwise = (Pacman (PacState (x,newCoords,z,t,h+1,l) q Closed d ), 100)
updateStatePacMan newCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (Food Big) | c == Closed = (Pacman (PacState (x,newCoords,2,t,h+5,l) 10 Open Mega ), 101)
                                                                                | otherwise = (Pacman (PacState (x,newCoords,2,t,h+5,l) 10 Closed Mega), 101)
updateStatePacMan newCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (PacPlayer (Ghost (GhoState (id,cf,_,_,_,_) m ))) = if m == Alive then (Pacman (PacState (x,y,z,t,h,l) q Closed Normal), 100)
                                                                                                                         else (Pacman (PacState (x,cf,z,t,h+10,l) q c d), id)

{-| IX.) A função updateMaze tem quatro inputs que são, respetivamente, as coordenadas de um jogador, as novas coordendas que o jogador irá tomar, um jogador e um labirinto. O output será um novo labirinto com o jogador nas suas novas coordenadas.
Esta função só se aplica ao PacMan visto que ela coloca peças Empty por todos os locais por onde este passou para simular o ato de comer as comidas.
	
Funções auxiliares utilizadas na construção da função updateMaze:
* replaceElemInMaze (função definida no módulo Types providenciadas pelos docentes)
-}
updateMaze :: Coords -> Coords -> Player -> Maze -> Maze -- só para o pacman
updateMaze oldCoords newCoords player = replaceElemInMaze oldCoords Empty

{-| X.) A função getGhostMode recebe um jogador, em particular, um fantasma, e devolve o seu modo (Dead ou Alive).-}
getGhostMode :: Player -> GhostMode 
getGhostMode (Ghost (GhoState _ q)) = q

{-| XI.) A função detectGhosts recebe uma jogada, um jogador (pacman), e uma lista jogadores (apenas de fantasmas). Esta função irá 
verificar se na lista com os fantasmas existe algum que esteja na posiçãop para a qual o pacman se pretende deslocar.
O resultado será um tuplo cuja 1ª componente é o fantasma detetado (caso exista, senão é indefenido) e uma flag (nº inteiro).
A flag será 0 se a função não detetar nenhum fantasma e será 1 caso algum fantasma seja detetado.

Funções auxiliares utilizadas na detectGhosts:
* getPlayerCoords (função do módulo types)
-}
detectGhosts :: Play -> Player -> [Player] -> (Player, Int)
detectGhosts _ _ [] = (undefined, 0)
detectGhosts pl@(Move _ L) pac (g:ps) | getPlayerCoords g == (x,y-1) = (g,1)
                                      | otherwise = detectGhosts pl pac ps
            where (x,y) = getPlayerCoords pac

detectGhosts pl@(Move _ R) pac (g:ps) | getPlayerCoords g == (x,y+1) = (g,1)
                                      | otherwise = detectGhosts pl pac ps
            where (x,y) = getPlayerCoords pac

detectGhosts pl@(Move _ U) pac (g:ps) | getPlayerCoords g == (x-1,y) = (g,1)
                                      | otherwise = detectGhosts pl pac ps
            where (x,y) = getPlayerCoords pac

detectGhosts pl@(Move _ D) pac (g:ps) | getPlayerCoords g == (x+1,y) = (g,1)
                                      | otherwise = detectGhosts pl pac ps
            where (x,y) = getPlayerCoords pac

{-| XII.) A função checkSurrounds tem três inputs que são, respetivamente, uma jogada, um labirinto e um jogador. O output será um tuplo cuja 1ª componente será a peça que está nas coordenadas para as quais o jogador se irá deslocar e a 2ª componente será as coordenadas dessa peca para onde o jogador vai.
Funções auxiliares utilizadas na construção da função checkSurrounds:
* takePiece - função definida no ponto III
* getPlayerCoords (função definida no módulo Types providenciadas pelos docentes)
-}
checkSurrounds :: Play -> Maze -> Player -> (Piece, Coords)
checkSurrounds (Move _ L) maze player = (takePiece maze (x, y-1), (x, y-1))
                                            where (x,y) = getPlayerCoords player
checkSurrounds (Move _ R) maze player = (takePiece maze (x, y+1), (x, y+1))
                                            where (x,y) = getPlayerCoords player
checkSurrounds (Move _ U) maze player = (takePiece maze (x-1, y), (x-1, y))
                                            where (x,y) = getPlayerCoords player            
checkSurrounds (Move _ D) maze player = (takePiece maze (x+1, y), (x+1, y))
                                            where (x,y) = getPlayerCoords player

{-| XIII.) A função checkGhostsAround recebe uma jogada, um labirinto, um jogador (pacman) e uma lista de jogadores. 
Esta função irá receber valores da função detectGhosts. Caso a flag dada pela detectghosts seja 0,  significa que não existe 
nenhum fantasma ao redor do pacman e, assim, a função que irá atuar é a checkSurrounds (função do ponto XII).
Caso contrário, é porque existe um fantasma na posição para a qual o pacman se pretende deslocar e, nesse caso, 
a função irá verificar se esse mesmo fantasma se encontra em modo Dead ou Alive. Neste caso, a função irá devolver 
o fantasma encontrado sob a forma de peça (Piece) e as novvas coordenadas do pacman.
-}
checkGhostsAround :: Play -> Maze -> Player -> [Player] -> (Piece, Coords)
checkGhostsAround pl@(Move _ L) maze pac pls | flag == 0 = checkSurrounds pl maze pac
                                             | otherwise = if getGhostMode pg == Alive then (PacPlayer pg, oldCoords)
                                                           else (PacPlayer pg, (x, y-1))
                            where (pg,flag) = detectGhosts pl pac pls
                                  oldCoords@(x,y) = getPlayerCoords pac

checkGhostsAround pl@(Move _ R) maze pac pls | flag == 0 = checkSurrounds pl maze pac
                                             | otherwise = if getGhostMode pg == Alive then (PacPlayer pg, oldCoords)
                                                           else (PacPlayer pg, (x, y+1))
                            where (pg,flag) = detectGhosts pl pac pls
                                  oldCoords@(x,y) = getPlayerCoords pac

checkGhostsAround pl@(Move _ U) maze pac pls | flag == 0 = checkSurrounds pl maze pac
                                             | otherwise = if getGhostMode pg == Alive then (PacPlayer pg, oldCoords)
                                                           else (PacPlayer pg, (x-1, y))
                            where (pg,flag) = detectGhosts pl pac pls
                                  oldCoords@(x,y) = getPlayerCoords pac

checkGhostsAround pl@(Move _ D) maze pac pls | flag == 0 = checkSurrounds pl maze pac
                                             | otherwise = if getGhostMode pg == Alive then (PacPlayer pg, oldCoords)
                                                           else (PacPlayer pg, (x+1, y))
                            where (pg,flag) = detectGhosts pl pac pls
                                  oldCoords@(x,y) = getPlayerCoords pac
 
{-| XIV.) A função setPlayerOrientations tem dois inputs que são, respetivamente,  uma orientação e um player. O output será um player com a nova orientação que lhe foi aplicada no input da função.
-}
setPlayerOrientation :: Orientation -> Player -> Player
setPlayerOrientation o (Pacman (PacState (x, y, z, _, h, l) q c d )) = Pacman (PacState (x, y, z, o, h, l) q c d )
setPlayerOrientation o (Ghost (GhoState (x, y, z, _, h, l) q )) = Ghost (GhoState (x, y, z, o, h, l) q )

{-| XV.) A função tunnelCoordsEven tem um input que é um labirinto e o output será um tuplo com quatro componentes que correspondem às quatro coordenadas do túnel do labirinto fornecido.
Funções auxiliares utilizadas na construção da função tunnelCoordsEven:
* div (pré-definida)
* length (pré-definida)
* head (pré-definida)
-}
tunnelCoordsEven :: Maze -> (Coords, Coords, Coords, Coords)
tunnelCoordsEven m  = ((div (length m) 2 -1,0), (div (length m) 2 -1,length (head m) -1), (div (length m) 2 ,0), (div (length m) 2,length (head m) -1))

{-| XVI.) A função tunnelCoordsOdd tem um input que é um labirinto e o output será um tuplo com duas componentes que correspondem às duas coordenadas do túnel do labirinto fornecido.
Funções auxiliares utilizadas na construção da função tunnelCoordsOdd:
* div (pré-definida)
* length (pré-definida)
* head (pré-definida)
-}
tunnelCoordsOdd :: Maze -> (Coords, Coords)
tunnelCoordsOdd m = ((div (length m) 2, 0), (div (length m) 2,length (head m) -1))


{-| XVII.) A função playPacManAuxAux tem quatro inputs que são, respetivamente, uma jogada, um labirinto, um jogador (pacman) e uma lista de jogadores (fantasmas) e o output será um tuplo cuja 1ª componente será o novo labirinto e a 2ª será a lista com os jogadores atualizados após a jogada fornecida no input.
Funções auxiliares utilizadas na construção da função playPacManAuxAux:
* updateMaze - função do ponto IX
* checkSurrounds - função do ponto X
* getPlayerCoords - (função definida no módulo Types providenciadas pelos docentes)
* updateStatePacMan - função do ponto VIII
* updateGhosts - função do ponto VII
-}
playPacManAuxAux :: Play -> Maze -> Player -> [Player] -> (Maze, [Player]) 
playPacManAuxAux move@(Move _ o) maze player players | piece == Empty = if fl == 1 
                                                                        then (updateMaze oldCoords ncoords newPac maze, newPac:newGhosts)
                                                                        else (updateMaze oldCoords newCoords newPlayer maze, newPlayer:players)
                                        where 
                                            gMode = getGhostMode pg
                                            (pg, fl) = detectGhosts move player players
                                            (pg',ncoords) = checkGhostsAround move maze player players
                                            newGhosts = updateGhosts maze flag players
                                            (newPac, fl') = updateStatePacMan ncoords player pg'
                                            (piece, newCoords) = checkSurrounds move maze player   
                                            oldCoords = getPlayerCoords player
                                            (newPlayer,flag) = updateStatePacMan newCoords player pg'                                                        
playPacManAuxAux move@(Move _ o) maze player players | piece == Food Little = if fl == 1
                                                                                then (updateMaze oldCoords ncoords newPac maze, newPac:newGhosts)
                                                                                else (updateMaze oldCoords newCoords newPlayer maze, newPlayer:players) --(updateMaze oldCoords newCoords newPlayer maze, [newPlayer] ++ players) -- bug igual ao empty pode ter o fantasme em cima da comida e o pacman nao o deteta, o raciocício é idêntico para quando a peça é empty. Tambem temos de usar a detectGhosts
                                                        where 
                                                            gMode = getGhostMode pg
                                                            (pg, fl) = detectGhosts move player players
                                                            (pg',ncoords) = checkGhostsAround move maze player players
                                                            newGhosts = updateGhosts maze flag players
                                                            (newPac, fl') = updateStatePacMan ncoords player pg'
                                                            (piece, newCoords) = checkSurrounds move maze player   
                                                            oldCoords = getPlayerCoords player
                                                            (newPlayer,flag) = updateStatePacMan newCoords player pg' 
playPacManAuxAux move@(Move _ o) maze player players | piece == Food Big = if fl == 1 
                                                                             then if gMode == Alive
                                                                                  then (updateMaze oldCoords ncoords newPac maze, newPac:players)
                                                                                  else (updateMaze oldCoords ncoords newPac maze, newPac:newGhosts)
                                                                             else (updateMaze oldCoords newCoords newPlayer maze, newPlayer:newPlayers) --(updateMaze oldCoords newCoords newPlayer maze, [newPlayer] ++ newPlayers) -- bug igual ao empty
                                                        where   --(updateMaze oldCoords ncoords newPac maze, [newPac] ++ newGhosts)
                                                            gMode = getGhostMode pg
                                                            (pg, fl) = detectGhosts move player players
                                                            (pg',ncoords) = checkGhostsAround move maze player players
                                                            newGhosts = updateGhosts maze (getPlayerID pg) players
                                                            (newPac, fl') = updateStatePacMan ncoords player pg'
                                                            (piece, newCoords) = checkSurrounds move maze player
                                                            oldCoords = getPlayerCoords player
                                                            (newPlayer, flag) = updateStatePacMan newCoords player piece
                                                            newPlayers = updateGhosts maze flag players
playPacManAuxAux move@(Move _ o) maze player players | piece == Wall = (maze, player:players) 
                                                        where (piece, newCoords) = checkSurrounds move maze player
playPacManAuxAux move@(Move _ o) maze player players = (updateMaze oldCoords newCoords newPlayer maze, newPlayer:players)
                    where
                        (newPlayer, flag) = updateStatePacMan newCoords player pg
                        oldCoords = getPlayerCoords player
                        (pg, newCoords) = checkGhostsAround move maze player players 

{-| XVIII.) A função playPacManAux faz exatamente o mesmo que a função playPacManAuxAux (função do ponto XV) visto que ambas têm os mesmos inputs e esta utiliza a função playPacManAuxAux como auxiliar. A única diferença desta para a anterioŕ é que a playPacManAux verifica se o modo do pacman está em Dying, quando tal não acontece, irá verificar se o referido jogador se encontra na zona do túnel para fazer a sua transição para o outro lado do labirinto.
	
Funções auxiliares utilizadas na construção da função playPacManAux: 
* getPacmanMode (função definida no módulo Types providenciada pelos docentes)
* replaceElemInMaze (função definida no módulo Types providenciada pelos docentes)
* setPlayerCoords (função definida no módulo Types providenciada pelos docentes)
* playPacManAuxAux - função do ponto XV
* getPlayerOrientation (função definida no módulo Types providenciada pelos docentes)
* length (pré-definida)
* getPlayerCoords (função definida no módulo Types providenciada pelos docentes)
* tunnelCoordsEven - função do ponto XII
* tunnelCoordsOdd - função do ponto XIII
-}
playPacManAux :: Play -> Maze -> Player -> [Player] -> (Maze, [Player]) 
playPacManAux move@(Move _ o) maze player players | getPacmanMode player /= Dying = if (oldCoords == ec && po == L) || (oldCoords == dc && po == R) || (oldCoords == eb && po == L) || (oldCoords == dc && po == R) || (oldCoords == e && po == L) || (oldCoords == d && po == R)
                                                                                        then
                                                                                            if even lm
                                                                                                then 
                                                                                                    if oldCoords == ec && o == L
                                                                                                        then (replaceElemInMaze dc (PacPlayer player) (replaceElemInMaze oldCoords Empty maze), setPlayerCoords player dc:players)
                                                                                                    else
                                                                                                        if oldCoords == eb && o == L
                                                                                                            then (replaceElemInMaze db (PacPlayer player) (replaceElemInMaze oldCoords Empty maze), setPlayerCoords player db:players)
                                                                                                        else
                                                                                                            if oldCoords == dc && o == R
                                                                                                                then (replaceElemInMaze ec (PacPlayer player) (replaceElemInMaze oldCoords Empty maze), setPlayerCoords player ec:players)
                                                                                                            else  (replaceElemInMaze eb (PacPlayer player) (replaceElemInMaze oldCoords Empty maze), setPlayerCoords player eb:players)
                                                                                            else 
                                                                                                if oldCoords == e && o == L
                                                                                                    then (replaceElemInMaze d (PacPlayer player) (replaceElemInMaze oldCoords Empty maze), setPlayerCoords player d:players)
                                                                                                else (replaceElemInMaze e (PacPlayer player) (replaceElemInMaze oldCoords Empty maze), setPlayerCoords player e:players)
                                                                                    else
                                                                                        playPacManAuxAux move maze player players
                                                  | otherwise = (maze, player:players) 
    where 
        po = getPlayerOrientation player
        lm = length maze
        oldCoords = getPlayerCoords player
        (ec,dc,eb,db) = tunnelCoordsEven maze
        (e,d) = tunnelCoordsOdd maze

{-| XIX.) A função playPacMan tem quatro inputs que são, respetivamente, uma jogada, um labirinto uma jogador (pacman) e uma lista de jogadores (fantasmas). O output será um tuplo cuja 1ª componente é o novo labirinto afetado pela jogada fornecida e 2ª componente é a lista de jogadores afetados pela jogada fornecida.

Funções auxiliares utilizadas na construção da função playPacMan:
* getPlayerOrientation (função definida no módulo Types providenciada pelos docentes)
* setPlayerOrientation - função do ponto XI
-}
playPacMan :: Play -> Maze -> Player -> [Player] -> (Maze, [Player]) 
playPacMan move@(Move _ o) maze pacman players | (getPlayerOrientation pacman == Null) || (getPlayerOrientation pacman /= o) = (maze, setPlayerOrientation o pacman:players)
                                               | otherwise = playPacManAux move maze pacman players

{-| XX.) A função updateStateGhost recebe um labirinto, as novas coordenadas do fantasma, um jogador (fantasma), a peça que 
se encontra na posição para a qual o fantasma se pretende deslocar e devolve o fantasma recebido devidamente atualizado.
Se a peça for uma comida ou vazia, o fantasma apenas adquire as novas coordenadas sem alterar nada no seu estado. Se a peça 
for uma parede, o fantasma não é alterado em nada e, se for um pacman em modo mega, o fantasma é "comido" pelo pacman e retorna
à casa. 

Funções auxiliares na construção da updateStateGhost:
* putGhostInHouse (função do ponto VI)
-}
updateStateGhost :: Maze -> Coords -> Player -> Piece -> Player
updateStateGhost _ newCoords (Ghost (GhoState (id,c,v,o,p,l) m)) Empty = Ghost (GhoState (id,newCoords,v,o,p,l) m)
updateStateGhost _ newCoords (Ghost (GhoState (id,c,v,o,p,l) m)) (Food Little) = Ghost (GhoState (id,newCoords,v,o,p,l) m)
updateStateGhost _ newCoords (Ghost (GhoState (id,c,v,o,p,l) m)) (Food Big) = Ghost (GhoState (id,newCoords,v,o,p,l) m)
updateStateGhost maze newCoords ghost@(Ghost (GhoState _ m)) (PacPlayer (Pacman (PacState _ _ _ d))) | d == Mega && m == Dead  = putGhostInHouse maze ghost
                                                                                                     | d == Mega && m == Alive = ghost
                                                                                                     | otherwise = ghost

{-| XXI.) A função checkPacman recebe um par de coordenadas e uma lista de jogadores e indica se existe um pacman nas coordenadas 
fornecidas e, nesse caso, devolve um tripo cujas componentes são o modo do pacman encontrado (Normal, Mega ou Dying), as 
coordenadas do pacman encontrado e uma flag que terá o valor 1 caso o pacman se encontre nas coordenadas dadas. Caso o pacman 
não se encontre nessas coordenadas, a função retornará o tripo cujas 1ª e 2ª componentes são indefinidas e a flag com valor 0.

Funções auxiliares na construção da função checkPacman:
* getPlayerCoords (função do módulo types)
-}
checkPacman :: Coords -> [Player] -> (PacMode, Coords ,Int)
checkPacman cs (pac@(Pacman (PacState _ _ _ d)):pls) | cs == c = (d, c, 1)
                                                     | otherwise = (undefined, undefined, 0)
                                    where c = getPlayerCoords pac
checkPacman cs ((Ghost _):pls) = checkPacman cs pls

{-| XXII.) A função respawnPacman recebe um labirinto e uma lista de jogadores. A função só atua quando o pacman é atacado por
algum fantasma e a mesma irá subtrair uma vida às que o pacman possui e colocá-lo-á numa posição do labirinto para recomeçar 
e repõe os fantasmas na casa.

Funções auxiliares na construção da função respawnPacman:
* map (pré-definida)
* length (pré-definida)
* head (pré-definida)
* div (pré-definida)
* takePacman (função do ponto XXIX)
* dropPacman (função do ponto XVIII)
-}
respawnPacman :: Maze -> [Player] -> [Player]
respawnPacman m [] = []
respawnPacman m pls = aux pacman:map (putGhostInHouse m) ghosts
    where
        pacman = takePacman pls
        ghosts = dropPacman pls
        newCoords = (length m -2, div (length (head m)) 2)
        aux :: Player -> Player 
        aux (Pacman (PacState (x,y,z,t,h,l) q c d)) = Pacman (PacState (x,newCoords,z,t,h,l-1) q c d)

{-| XXIII.) A função playGhostAuxAux recebe uma jogada, um labirinto, um jogador (fantasma) e uma lista de jogadores. 
Conforme a jogada recebida, a função irá verificar a peça na posição para a qual o fantasma se pretende deslocar e, de acordo 
com essa peça a função irá atualizar devidamente o fantasma. 

Funções utilizadas na construção da playGhostAuxAux:
* checkSurrounds (função do ponto XII)
* putGhostInHouse (função do ponto VI)
* getGhostMode (função do ponto X)
* checkPacman (função do ponto XXI)
* updateStateGhost (função do ponto XX)
-}
playGhostAuxAux :: Play -> Maze -> Player -> [Player] -> (Maze, [Player])
playGhostAuxAux move@(Move _ o) maze ghost players | piece == Wall = (maze, ghost:players)
                                        where (piece, _) = checkSurrounds move maze ghost 
playGhostAuxAux move@(Move _ o) maze ghost players | piece == Food Big || piece == Food Little || piece == Empty
                                                        = if flag == 0 then (maze, newGhost:players)
                                                          else if getGhostMode ghost == Dead
                                                               then (maze, putGhostInHouse maze ghost:players)
                                                               else (maze, putGhostInHouse maze ghost:respawnPacman maze players)
                                        where
                                            -- quando tem lá o pacman
                                            (_, coords, flag) = checkPacman newCoords players
                                            -- quando nao tem lá o pacman
                                            (piece, newCoords) = checkSurrounds move maze ghost
                                            newGhost = updateStateGhost maze newCoords ghost piece

{-| XXIV.) A função playGhostAux recebe uma jogada, um labirinto, um jogador (fantasma) e uma lista de jogadores.
Caso o fantasma se encontre no túnel e pretenda atravessá-lo, a função irá permitir que o fantasma o atravesse. Caso contrário, 
a função que se irá encarregar de atualizar o fantasma será função playGhostAuxAux (função do ponto XXIII) 

Funções utilizadas na construção da playGhostAux:
* setPlayerCoords (função do módulo types)
* playGhostAuxAux (função do ponto XXIII)
* getPlayerCoords (função do módulo types)
* getPlayerOrientation (função do módulo types)
* length (pré-definida)
* tunnelCoordsOdd (função do ponto XVI)
* tunnelCoordsEven (função do ponto XV)
-}
playGhostAux :: Play -> Maze -> Player -> [Player] -> (Maze, [Player])
playGhostAux move@(Move _ o) maze ghost players = if (oldCoords == ec && po == L) || (oldCoords == dc && po == R) || (oldCoords == eb && po == L) || (oldCoords == dc && po == R) || (oldCoords == e && po == L) || (oldCoords == d && po == R)
                                                  then if even lm
                                                       then if oldCoords == ec && o == L
                                                            then (maze, setPlayerCoords ghost dc:players)
                                                            else if oldCoords == eb && o == L
                                                                 then (maze, setPlayerCoords ghost db:players)
                                                                 else if oldCoords == dc && o == R
                                                                      then (maze, setPlayerCoords ghost ec:players)
                                                                      else (maze, setPlayerCoords ghost eb:players)
                                                       else if oldCoords == e && o == L
                                                            then (maze, setPlayerCoords ghost d:players)
                                                            else (maze, setPlayerCoords ghost e:players)
                                                  else playGhostAuxAux move maze ghost players
            where
                oldCoords = getPlayerCoords ghost
                po = getPlayerOrientation ghost
                lm = length maze
                (ec,dc,eb,db) = tunnelCoordsEven maze
                (e,d) = tunnelCoordsOdd maze


{-| XXV.) A função playGhost recebe uma jogada, um labirinto, um jogador (fantasma) e uma lista de jogadores. Esta função
irá verificar se a orientação do fantasma é nula ou diferente da orientação da jogada e, caso isso aconteça, a função irá apenas
mudar a orientação do fantasma para a da jogada fornecida. Caso contrário, a função encarregue de atualizar o fantasma, será a 
playGhostAux (ponto XXIV).

Funções utilizadas na construção da playGhost:
* getPlayerOrientation (função do módulo types)
* setPlayerOrientation (função do ponto XIV)
* playGhostAux (função do ponto XXIV)
-}
playGhost :: Play -> Maze -> Player -> [Player] -> (Maze, [Player]) 
playGhost move@(Move _ o) maze ghost players | (getPlayerOrientation ghost == Null) || (getPlayerOrientation ghost /= o) = (maze, setPlayerOrientation o ghost:players)
                                             | otherwise = playGhostAux move maze ghost players


{-| XXVI.) A função playAux tem três inputs que são, respetivamente, uma jogada, um labirinto, uma lista de players e um nº inteiro (nível do jogo). O output será um estado atualizado qued foi afetado pela jogada fornecida no input,
Funções auxiliares utilizadas na construção da função playAux:
* getPlayerID (função definida no módulo Types providenciada pelos docentes)
* playPacMan - função do ponto XVII
* playGhost - função do ponto XVIII
* removePlayer - função do ponto IV
-}
playAux :: Play -> Maze -> [Player] -> Int -> State
playAux move@(Move id _) maze players@(pac@(Pacman _):t) level | getPlayerID pac == id = State maze' (reduceTimeMega players') level
                                                               | otherwise = playAux move maze (t ++ [pac]) level
                                                                    where
                                                                        (maze', players') = playPacMan move maze pac (removePlayer pac players)                                                            
playAux move@(Move id _) maze players@(ghost@(Ghost _):t) level | getPlayerID ghost == id = State maze' players' level
                                                                | otherwise = playAux move maze (t ++ [ghost]) level
                                                                    where
                                                                        (maze', players') = playGhost move maze ghost (removePlayer ghost players) 

{-| XXVII.) A função reduceTimeMega recebe uma lista de jogadores e, dentro dessa lista, retira 0.25 unidades de tempo 
mega ao pacman sempre que é executada. É de referir que a unidade de tempo considerada é o segundo. -}
reduceTimeMega :: [Player] -> [Player]
reduceTimeMega [] = []
reduceTimeMega (g@(Ghost _):t) = g:reduceTimeMega t
reduceTimeMega ((Pacman (PacState (x,y,z,t,h,l) q c d)):t') = if q > 0 then Pacman (PacState (x,y,z,t,h,l) (q-0.25) c d):t'
                                                              else Pacman (PacState (x,y,z,t,h,l) q c Normal):t'
{-| XXVIII.) A função dropPacman recebe uma lista de jogadores e retira o pacman dessa lista.-}
dropPacman :: [Player] -> [Player]
dropPacman [] = []
dropPacman (p@(Pacman _):t') = t'
dropPacman (g@(Ghost _):t) = g:dropPacman t

{-| XXIX.) A função takePacman recebe uma lista de jogadores e dessa lista apenas devolve o pacman.-}
takePacman :: [Player] -> Player
takePacman (p@(Pacman _):t') = p
takePacman ((Ghost _):t) = takePacman t

{-| XXX.) A função deadToAlive recebe uma lista de jogadores e, nessa lista, coloca o modo de todos os fantasmas em Alive.
Será utilizada para quando o tempo mega do pacman se esgotar.

Funções utilizadas na construção da deadToAlive:
* getPacmanMode (função do modulo types)
* takePacman (função do ponto XXIX)
* dropPacman (função do ponto XXVIII)
-}
deadToAlive :: [Player] -> [Player] 
deadToAlive [] = []
deadToAlive pls = if pacmode == Mega then pls else pacman:map deadToAlive' ghosts
        where
            pacmode = getPacmanMode pacman
            pacman = takePacman pls
            ghosts = dropPacman pls
            deadToAlive' :: Player -> Player
            deadToAlive' (Ghost (GhoState (id,c,v,o,p,l) m)) = Ghost (GhoState (id,c,v,o,p,l) Alive)

{-| XXXI.) A função livesPacman recebe um jogador (pacman) e retorna as vidas restantes desse jogador.
-}
livesPacman :: Player -> Int 
livesPacman (Pacman (PacState (_,_,_,_,_,l) _ _ _)) = l

{-| XXXII.) A função pacmanIsDead recebe uma lista de jogares e, nessa lista, verifica se o pacman tem o número de vidas
restantes menor do que 0 e, nesse caso, retorna True. Caso contrário, retorna False.

Funções utilizadas na construção da pacmanIsDead:
* livespacman (função do ponto XXXI)
* takePacman (função do ponto XXIX)
-}
pacmanIsDead :: [Player] -> Bool
pacmanIsDead pls = livesPacman (takePacman pls) < 0

{-| XXXIII.) A função numberFoods recebe um labirinto e devolve o número de comidas (grandes e pequenas) restantes no labirinto.

Funções utilizadas na construção da numberFoods:
* sum (pré-definida)
* map (pré-definida)
-}
numberFoods :: Maze -> Int
numberFoods [] = 0
numberFoods maze = sum $ map numberFoodsCorridor maze
    where
        numberFoodsCorridor :: Corridor -> Int
        numberFoodsCorridor [] = 0
        numberFoodsCorridor ((Food Little):t) = 1+ numberFoodsCorridor t
        numberFoodsCorridor ((Food Big ):t) = 1+ numberFoodsCorridor t
        numberFoodsCorridor (_:t) = numberFoodsCorridor t

{-| XXXIV.) A função noFoods recebe um labirinto e verifica se este ainda tem, ou não, comidas que o pacman não comeu. 

Funções utilizadas na construção da numberFoods:
* numberFoods (função do ponto XXXIII)
-}
noFoods :: Maze -> Bool 
noFoods maze = numberFoods maze <= 0

{-| XXXV.) A função play tem dois inputs que são, respetivamente, uma jogada e um estado e o output será um o estado dado no input afetado pela jogada também dada no input.
Funções auxiliares utilizadas na construção da função play:
* checkIDPlayer - função do ponto I
* playAux - função do ponto XIX
* noFoods - função do ponto XXXIV
* pacManIsDead - função do ponto XXXII
-}
play :: Play -> State -> State 
play move@(Move id _) state | noFoods (maze state) = error "You won!!"
                            | pacmanIsDead (playersState state) = error "Game Over"
                            | checkIDPlayer id (playersState state) = playAux move (maze state) (deadToAlive(playersState state)) (level state)
                            | otherwise = state