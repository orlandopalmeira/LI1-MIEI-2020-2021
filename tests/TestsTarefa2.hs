module TestsTarefa2 where
import Types 
import Tarefa2
import FileUtils

mudancaOrientacaoUP = play (Move 0 U) state1
mudancaOrientacaoDown = play (Move 0 D) state1
mudancaOrientacaoRight = play (Move 0 R) state1
mudancaOrientacaoLeft = play (Move 2 L) state2
colisaoCparede = play (Move 0 L) state1
comeComidaPequena = play (Move 2 R) state2
comeComidaGrande = play (Move 0 R) state6
entraNoTunel1 = play (Move 1 L) state4
entraNoTunel2 = play (Move 2 R) state5
comeFantasmaVivo = play (Move 3 L) (play (Move 3 L) state3)
comeFantasmaMorto = play (Move 0 R) comeComidaGrande

tests = [mudancaOrientacaoUP,mudancaOrientacaoDown,mudancaOrientacaoRight,mudancaOrientacaoLeft,colisaoCparede,comeComidaPequena,comeComidaGrande,entraNoTunel1,entraNoTunel2,comeFantasmaVivo,comeFantasmaMorto]

results = printResults tests

printResults :: [State] -> IO ()
printResults [] = putStrLn ""
printResults l = putStrLn (printResults' l)
    where
        printResults' :: [State] -> String
        printResults' [] = "\n"
        printResults' (s:ss) = ("test >>> \n" ++show s) ++ printResults' ss