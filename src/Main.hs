module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils 
import Tarefa2 
import Tarefa4 
import Tarefa6


data Manager = Manager 
    {   
        state  :: State
    ,   pid    :: Int
    ,   step   :: Int
    ,   before :: Integer
    ,   delta  :: Integer
    ,   delay  :: Integer
    }


loadManager :: Manager
loadManager = Manager (loadMaze "../mazes/4.txt") 4 0 0 0 defaultDelayTime

dropPlayer :: Int -> [Player] -> [Player]
dropPlayer _ [] = []
dropPlayer id (p:t) = if getPlayerID p == id then t else p:dropPlayer id t

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyLeftArrow (Manager state@(State m pls l) pid step before delta delay) = Manager (State m ([newplayer] ++ newplayers) l) pid step before delta delay
    where
      newplayer = setPlayerOrientation L chplayer -- player com nova orientação
      newplayers = dropPlayer pid (playersState state) -- lista com os players inalterados
      chplayer = getPlayer pid (playersState state) -- player cuja orientação é alterada      

updateControlledPlayer KeyRightArrow (Manager state@(State m pls l) pid step before delta delay) = Manager (State m ([newplayer] ++ newplayers) l) pid step before delta delay
    where
      newplayer = setPlayerOrientation R chplayer -- player com nova orientação
      newplayers = dropPlayer pid (playersState state) -- lista com os players inalterados
      chplayer = getPlayer pid (playersState state) -- player cuja orientação é alterada 
updateControlledPlayer KeyUpArrow (Manager state@(State m pls l) pid step before delta delay) = Manager (State m ([newplayer] ++ newplayers) l) pid step before delta delay
    where
      newplayer = setPlayerOrientation U chplayer -- player com nova orientação
      newplayers = dropPlayer pid (playersState state) -- lista com os players inalterados
      chplayer = getPlayer pid (playersState state) -- player cuja orientação é alterada 
updateControlledPlayer KeyDownArrow (Manager state@(State m pls l) pid step before delta delay) = Manager (State m ([newplayer] ++ newplayers) l) pid step before delta delay
    where
      newplayer = setPlayerOrientation D chplayer -- player com nova orientação
      newplayers = dropPlayer pid (playersState state) -- lista com os players inalterados
      chplayer = getPlayer pid (playersState state) -- player cuja orientação é alterada 
updateControlledPlayer _ m = m

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager s pid st bf dlt del) = Manager s pid st now (dlt + (now - bf)) del -- TODO

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager s pid st bf dlt del) = Manager s pid st now 0 del  -- TODO

nextFrame :: Integer -> Manager -> Manager
nextFrame now man@(Manager state pid step before delta delay) = resetTimer now $ Manager (passTime step state) pid (step + 1) before delta delay
--nextFrame now man = man -- TODO old version 


loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if delt > del 
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if ev' == EventCharacter 'q'
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager