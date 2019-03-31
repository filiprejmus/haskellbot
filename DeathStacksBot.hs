--- module
module DeathStacksBot where
--- imports
import Data.Char
import Util

--- external signatures
getMove :: String -> String
listMoves :: String -> String


prelistMoves string = if length(botStonesabove4 string) > 0 then init(simStones (botStonesabove4 string)) else init(simStones (botStones string))
getMove string = head (splitOn "," (prelistMoves string))
worklist :: String -> [String]
worklist string = splitOn "," (removeSlash string)

botCol string = last string
removeSlash [] = []
removeSlash ('/':xs) = [','] ++ removeSlash xs
removeSlash (x:xs) = [x] ++ removeSlash xs

pureList string = init(worklist(string)) ++ [init(init(last(worklist(string))))]

controlledByBot _ "" = False
controlledByBot string stone = head stone == botCol string

tupleList string = zip [(x,y) | x <- [6,5..1], y <- [1..6]] (pureList string)

botStonesEmpty string = filter((>0).length.snd)(tupleList string)
botStones string = filter ((==)(botCol string).head.snd)(botStonesEmpty string)
botStonesabove4 string = filter((>4).length.snd)(botStones string)

simStones []  = []
simStones (x:xs) = fieldconv (moveStoneallDirnoRepList (length (snd x)) (fst x)) ++ simStones (xs) 
---simulates every possible Movement for a stone with parametres stacklength, position on board and direction
moveStoneSim 0 pos dir = pos
moveStoneSim len pos dir
  | dir == (-1,-1) = if (fst pos == 1 && snd pos == 1) then moveStoneSim len pos (1,1) else if (fst pos == 1) then moveStoneSim len pos (1, -1) else if (snd pos == 1) then moveStoneSim len pos (-1, 1) else moveStoneSim (len - 1) (fst pos -1, snd pos -1) dir          
  | dir == (-1, 1) = if (fst pos == 1 && snd pos == 6) then moveStoneSim len pos (1,-1) else if (fst pos == 1) then moveStoneSim len pos (1, 1) else if (snd pos == 6) then moveStoneSim len pos (-1, -1) else moveStoneSim (len - 1) (fst pos -1, snd pos +1) dir 
  | dir == (0 ,-1) = if (snd pos == 1) then moveStoneSim len pos (0, 1) else moveStoneSim (len -1) (fst pos, snd pos -1) dir
  | dir == (0 , 1) = if (snd pos == 6) then moveStoneSim len pos (0, -1) else moveStoneSim (len -1) (fst pos, snd pos +1) dir            
  | dir == (1 ,-1) = if (fst pos == 6 && snd pos == 1) then moveStoneSim len pos (-1,1) else if (fst pos == 6) then moveStoneSim len pos (-1, -1) else if (snd pos == 1) then moveStoneSim len pos (1, 1) else moveStoneSim (len - 1) (fst pos +1, snd pos -1) dir
  | dir == (1 , 0) = if (fst pos == 6) then moveStoneSim len pos (-1, 0) else moveStoneSim (len -1) (fst pos + 1, snd pos) dir
  | dir == (1 , 1) = if (fst pos == 6 && snd pos == 6) then moveStoneSim len pos (-1,-1) else if (fst pos == 6) then moveStoneSim len pos (-1, 1) else if (snd pos == 6) then moveStoneSim len pos (1, -1) else moveStoneSim (len - 1) (fst pos +1, snd pos +1) dir
  | dir == (-1 , 0) = if (fst pos == 1) then moveStoneSim len pos (1, 0) else moveStoneSim (len -1) (fst pos -1, snd pos) dir
--- simulates for all directions
moveStoneallDir 0 _ = []
moveStoneallDir len pos = let direct = (-1,-1) : (1,1) : [(x,y) | x <- [-1..1], y <- [-1..1], x/=y ] in
                         zip (zip (map (moveStoneSim len pos) direct) (cycle [len])) (cycle [pos]) ++ moveStoneallDir (len - 1) pos

moveStoneallDirnoRep [] = []
moveStoneallDirnoRep (x:xs) = if (fst (fst x) /= snd x) then x : moveStoneallDirnoRep xs else moveStoneallDirnoRep xs 

moveStoneallDirnoRepList len pos = moveStoneallDirnoRep (moveStoneallDir len pos)

fieldconv [] = []
fieldconv (x:xs) = posToLit (snd x) ++ "-" ++ show (snd(fst x)) ++"-"++ posToLit (fst(fst x)) ++ "," ++ fieldconv xs

--converts positions to the right format
posToLit (x,pos)
 | pos == 1 = 'a' : show x 
 | pos == 2 = 'b' : show x 
 | pos == 3 = 'c': show x 
 | pos == 4 = 'd': show x
 | pos == 5 = 'e': show x 
 | pos == 6 = 'f' : show x 
 

deduplicatelistMoves string = nub2 (splitOn "," (prelistMoves string))
listMoves string = "[" ++ init (uniteList (deduplicatelistMoves string)) ++ "]"

uniteList [] = []
uniteList (x:xs) = x ++ "," ++ uniteList xs

nub2                     :: (Eq a) => [a] -> [a]
nub2 l                   = nub2' l []
  where
    nub2' [] _           = []
    nub2' (x:xs) ls
        | x `elem` ls   = nub2' xs ls
        | otherwise     = x : nub2' xs (x:ls)