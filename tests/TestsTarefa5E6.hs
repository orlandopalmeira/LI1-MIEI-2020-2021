module TestsTarefa5e6 where

import Tarefa1
import Tarefa2
import Tarefa5
import Tarefa6
import Types

m1 = generateMaze 30 22 2352

j1 = (Pacman (PacState (1,(8,8),1,R,0,1) 0 Open Normal))
j2 = (Ghost (GhoState (2,(9,5),1,L,0,1) Alive)) 
j3 = (Ghost (GhoState (3,(3,12),1,U,0,1) Alive)) 
j4 = (Ghost (GhoState (4,(9,27),1,D,0,1) Alive))
l1 = [j1,j2,j3,j4]
s1 = State m1 l1 1

gp1 = ghostPlay s1
b1 = bot 1 s1


j5 = (Pacman (PacState (5,(8,8),1,R,0,1) 0 Open Mega))
j6 = (Ghost (GhoState (6,(9,5),1,L,0,1) Dead)) 
j7 = (Ghost (GhoState (7,(1,12),1,U,0,1) Dead))
j8 = (Ghost (GhoState (8,(9,27),1,D,0,1) Dead))
l2 = [j5,j6,j7,j8]
s2 = State m1 l2 1

gp2 = ghostPlay s2
b2 = bot 5 s2

m2 = generateMaze 20 15 8787

j9 = (Pacman (PacState (9,(2,11),1,R,0,1) 0 Open Normal))
j10 = (Ghost (GhoState (10,(5,9),1,L,0,1) Alive)) 
j11 = (Ghost (GhoState (11,(8,14),1,U,0,1) Alive))
l3 = [j9,j10,j11]
s3 = State m2 l3 1

gp3 = ghostPlay s3
b3 = bot 9 s3 

j12 = (Pacman (PacState (12,(2,11),1,R,0,1) 0 Open Mega))
j13 = (Ghost (GhoState (13,(1,4),1,L,0,1) Dead)) 
j14 = (Ghost (GhoState (14,(8,14),1,U,0,1) Dead))
l4 = [j12,j13,j14]
s4 = State m2 l4 1

gp4 = ghostPlay s4
b4 = bot 12 s4 

results = printResults [s1,s2,s3,s4] [gp1,gp2,gp3,gp4] [b1,b2,b3,b4]

printResults :: [State] -> [[Play]] -> [Maybe Play] -> IO ()
printResults [] [] [] = putStrLn ""
printResults a b c = putStrLn (printResults' a b c)
    where
        printResults' :: [State] -> [[Play]] -> [Maybe Play] -> String
        printResults' [] [] [ ]= "\n"
        printResults' (s:ss) (gp:gps) (b:bs) = ("test >>> \n" ++ (show s) ++ "GhostPlay: " ++ (show gp) ++ "\n" ++ "Bot: " ++ (show b) ++ "\n" ) 
                                               ++ printResults' ss gps bs