module TestsTarefa3 where
import Tarefa3
import Types 

maze1 = [[Wall, Wall, Wall, Wall],
         [Wall, Food Big, Food Little, Wall],
         [Wall, Food Little, Food Big, Wall],
         [Wall, Wall, Wall, Wall]]
maze2 = [[Wall, Wall, Wall, Wall],
         [Wall, Food Big, Food Little, Wall],
         [Wall, Food Big, Food Little, Wall],
         [Wall, Wall, Wall, Wall]]
maze3 = [[Wall, Wall, Wall, Wall],
         [Wall, Food Big, Food Little, Wall],
         [Wall, Empty, Empty, Wall],
         [Wall, Wall, Wall, Wall]]
maze4 = [[Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall],
         [Wall, Wall, Wall, Wall]]
maze5 = [[Food Big, Food Big, Food Big, Food Big],
         [Food Big, Food Big, Food Big, Food Big],
         [Food Big, Food Big, Food Big, Food Big],
         [Food Big, Food Big, Food Big, Food Big]]
maze6 = [[Wall, Empty, Empty, Wall],
         [Food Little, Food Little, Food Little, Food Little],
         [Wall, Empty, Empty, Wall],
         [Wall, Empty, Empty, Wall]]

mazes = [maze1,maze2,maze3,maze4,maze5,maze6]

results = printResults mazes

printResults :: [Maze] -> IO ()
printResults [] = putStrLn ""
printResults l  = putStrLn (printResults' l)
    where
        printResults' :: [Maze] -> String
        printResults' [] = "\n"
        printResults' (m:ms) = "test >>> " ++ show (compactMaze m) ++ "\n" ++ printResults' ms