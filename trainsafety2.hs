import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map
import Data.List (nub, nubBy)

makeSafe :: Layout -> MessageType -> String -> ([TrackInstruction], Layout)
makeSafe t Speed m = (checkSpeed t m, t)
makeSafe t Direction m = undefined
makeSafe t Sensor m = checkSensor t m


checkSensor :: Layout -> String -> ([TrackInstruction], Layout)
checkSensor t s = checkWaitingLocos t

checkMerging :: Layout -> Section -> ([TrackInstruction],Layout)
checkMerging t s@(Section { loco=(Locomotive { direction=d }) }) | sec == s = ([],t)
																 | otherwise = pauseLoco t (slower s sec)
	where sec = findMerging t s d

findMerging :: Layout -> Section -> Direction -> Section
findMerging t s d | onMerge t s d = checkDirection s (findParallel t s d)
				  | otherwise = s

onMerge :: Layout -> Section -> Direction -> Bool
onMerge t s FWD = length (prev (findNextSection t s FWD)) > 1
onMerge t s BKW = length (next (findNextSection t s BKW)) > 1

findParallel :: Layout -> Section -> Direction -> Section
findParallel t s FWD = (t Map.! (head (filter ((sid s) /=) (prev n))))
	where n = findNextSection t s FWD
findParallel t s BKW = (t Map.! (head (filter ((sid s) /=) (next p))))
	where p = findNextSection t s BKW

checkDirection :: Section -> Section -> Section
checkDirection s s2 | direction l == direction l2 = s2
					| otherwise = s
	where
		l = loco s
		l2 = loco s2

slower :: Section -> Section -> Section
slower a b | speed (loco a) > speed (loco b) = b
		   | otherwise = a


checkWaitingLocos :: Layout -> ([TrackInstruction], Layout)
checkWaitingLocos t = examinePaused t (listWaitingLocos t)

examinePaused :: Layout -> [Section] -> ([TrackInstruction], Layout)
examinePaused t [] = ([],t)
examinePaused t (s:ss) | not (containsLoco (findNextSection t s (direction l))) = (a ++ x, y)
					   | otherwise = examinePaused t ss
	where
		l = loco s
		(x,y) = examinePaused b ss
		(a,b) = unpauseLoco t s

checkAdjacent :: Layout -> Section -> [TrackInstruction]
checkAdjacent t s = checkFollowing t s

-------------------------------------------------
---
---      Speed Checks
---
-------------------------------------------------

checkSpeed :: Layout -> String -> [TrackInstruction]
checkSpeed t s = (checkSpeedLimit sec) ++ (checkFollowing t sec)
	where
		sec = findLoco t (read s)

checkSpeedLimit :: Section -> [TrackInstruction]
checkSpeedLimit s | speed l > speedlim s = [setLocoSpeed l (speedlim s)]
				  | otherwise = []
	where l = loco s

checkFollowing :: Layout -> Section -> [TrackInstruction]
checkFollowing t s = speedCheckNextSection t s (nextNextSection t s (direction (loco s)))

speedCheckNextSection :: Layout -> Section -> Section -> [TrackInstruction]
speedCheckNextSection t s1 s2 | not (containsLoco s2) = []
						 -- | direction (loco s1) /= direction (loco s2) = (stopLoco (loco s1)) : checkSpeed t (show (slot (loco s2)))
							  | otherwise = checkFollowingSpeeds (loco s1) (loco s2)

checkFollowingSpeeds :: Locomotive -> Locomotive -> [TrackInstruction]
checkFollowingSpeeds a b | speed a > speed b = [setLocoSpeed a (speed b)]
						 | otherwise = []

-------------------------------------------------
---
---      Util Functions
---
-------------------------------------------------

pauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
pauseLoco t s = ([stopLoco l], Map.insert (sid s) (upd s) t)
	where 
		upd s = s { loco=(l { waiting=True, prevspeed=speed l})}
		l = loco s

unpauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
unpauseLoco t s = ([setLocoSpeed l (prevspeed l)], Map.insert (sid s) (upd s) t)
	where
		upd s = s { loco=(l { waiting=False })}
		l = loco s

stopLoco :: Locomotive -> TrackInstruction
stopLoco l = "0 " ++ show (slot l) ++ " 0"

setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i

findNextSection :: Layout -> Section -> Direction -> Section
findNextSection t s@(Section { nextturn=Noturn }) FWD = t Map.! (head (next s))
findNextSection t s@(Section { prevturn=Noturn }) BKW = t Map.! (head (prev s))
findNextSection t s@(Section { nextturn=Unset }) FWD = t Map.! (head (next s))
findNextSection t s@(Section { prevturn=Unset }) BKW = t Map.! (head (next s))
findNextSection t s@(Section { nextturn=Set }) FWD = t Map.! (head (tail (next s)))
findNextSection t s@(Section { prevturn=Set }) BKW = t Map.! (head (tail (next s)))

nextNextSection :: Layout -> Section -> Direction -> Section
nextNextSection t s d = findNextSection t (findNextSection t s d) d

containsLoco :: Section -> Bool
containsLoco s = (loco s) /= Noloco

-- Track -> All sections containing a loco
listLocos :: Layout -> [Section]
listLocos t = Map.elems (Map.filter (containsLoco) t)

listWaitingLocos :: Layout -> [Section]
listWaitingLocos t = filter (\x -> waiting (loco x)) (listLocos t)

findLoco :: Layout -> Int -> Section
findLoco t s = head (Map.elems (Map.filter (\x -> (slot (loco x)) == s) notempty))
	where
		notempty = Map.filter (containsLoco) t