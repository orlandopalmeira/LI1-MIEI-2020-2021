module TestsTarefa1 where
import Types
import Tarefa1
--ghci TestsTarefa1.hs -i../src
maze1 = generateMaze 30 15 193274
maze2 = generateMaze 30 16 1903854
maze3 = generateMaze 31 15 93452345
maze4 = generateMaze 31 16 23945
maze5 = generateMaze 20 10 83254
maze6 = generateMaze 20 11 23495
maze7 = generateMaze 21 10 2384975
maze8 = generateMaze 21 11 8975432
mazes = [maze1,maze2,maze3,maze4,maze5,maze6,maze7,maze8]
results = printResults mazes

printMaze :: Maze -> String
printMaze [] = "\n"
printMaze (c:cs) = printCorridor c ++ printMaze cs
    where
        printCorridor :: Corridor -> String
        printCorridor [] = "\n"
        printCorridor (p:ps) = show p ++ printCorridor ps

printResults :: [Maze] -> IO ()
printResults [] = putStrLn ""
printResults l = putStrLn (printMazes l)
    where
        printMazes :: [Maze] -> String
        printMazes [] = ""
        printMazes (m:ms) = ("test >>> \n" ++ printMaze m) ++ printMazes ms