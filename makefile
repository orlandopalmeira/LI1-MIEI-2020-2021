all: main docS
main: Tarefa1 Tarefa2 Tarefa3 Tarefa4 Tarefa5 Tarefa6
Tarefa1:
	ghc src/Tarefa1.hs -i./src -i./libs
Tarefa2:
	ghc src/Tarefa2.hs -i./src -i./libs
Tarefa3:
	ghc src/Tarefa3.hs -i./src -i./libs
Tarefa4:
	ghc src/Tarefa4.hs -i./src -i./libs
Tarefa5:
	ghc src/Tarefa5.hs -i./src -i./libs
Tarefa6:
	ghc src/Tarefa6.hs -i./src -i./libs
docS: 
	haddock -h -o ./docs ./src/**.hs ./libs/**.hs
clean:
	rm -rf ./src/**.hi
	rm -rf ./src/**.o
	rm -rf ./libs/**.hi
	rm -rf ./libs/**.o
