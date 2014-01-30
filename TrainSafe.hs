module TrainSafe where

import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map
import Data.List (nub, nubBy)

process :: Layout -> String -> ([TrackInstruction],Layout)
process t s = makeSafe (updateLayout t msg) msg
	where msg = checkMessage (words s)

updateLayout :: Layout -> (MessageType, Message) -> Layout
updateLayout t (Speed,m) = speedChange t m
updateLayout t (Direction,m) = changeDirection t m
updateLayout t (Sensor,m) = sectionSensorTrigger t m

makeSafe :: Layout -> (MessageType, Message) -> ([TrackInstruction], Layout)
makeSafe t (Speed, m) = (checkSpeed t m, t)
makeSafe t (Direction, m) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(sid (findLoco t (dirslot m)))}))
makeSafe t (Sensor, m) = checkSensor t m

checkMessage :: [String] -> (MessageType, Message)
checkMessage a@("speed":_) = (Speed, parseSpeed a)
checkMessage a@("dir":_) = (Direction, parseDirection a)
checkMessage a@("sensor":_) = (Sensor, parseSensor a)



checkSensor :: Layout -> Message -> ([TrackInstruction], Layout)
checkSensor t s = (g++a++c++e, f)
	where
		(a,b) = checkWaitingLocos t
		(c,d) = checkMerging b (sec b)
		(e,f) = locoCheckNextSection d (sec d)
		g = sensorSpeedCheck t (sec t)
		sec m = locoFromSensorMessage m s

sensorSpeedCheck :: Layout -> Section -> [TrackInstruction]
sensorSpeedCheck t s | l == Noloco = []
				     | otherwise = checkSpeed t (SpeedMessage { fromslot=(slot l), newspeed=(speed l)})
	where l = loco s

locoFromSensorMessage :: Layout -> Message -> Section
locoFromSensorMessage t m = t Map.! (updid m)
--locoFromSensorMessage t (SensorMessage {upd=Hi,updid=sd}) | state (t Map.! sd) == Occupied = (t Map.! sd)
--														  | otherwise = findAdjacentLoco t (t Map.! sd) Hi
--locoFromSensorMessage t (SensorMessage {upd=Low,updid=sd}) | state (t Map.! sd) == Justleft = (t Map.! sd)
--														  | otherwise = findAdjacentLoco t (t Map.! sd) Low

findAdjacentLoco :: Layout -> Section -> SensorUpdate -> Section
findAdjacentLoco t s Hi | containsLoco nexts && direction (loco nexts) == BKW = nexts
						| containsLoco prevs && direction (loco prevs) == FWD = prevs
	where
		nexts = findNextSection t s FWD
		prevs = findNextSection t s BKW
findAdjacentLoco t s Low | containsLoco nexts && direction (loco nexts) == FWD = nexts
						 | containsLoco prevs && direction (loco prevs) == BKW = prevs
	where
		nexts = findNextSection t s FWD
		prevs = findNextSection t s BKW


locoCheckNextSection :: Layout -> Section -> ([TrackInstruction], Layout)
locoCheckNextSection t s | not (containsLoco s) = ([],t)
						 | containsLoco (findNextSection t s (direction (loco s))) = pauseLoco t s
						 | otherwise = ([],t)

checkMerging :: Layout -> Section -> ([TrackInstruction],Layout)
checkMerging t s@(Section { loco=(Locomotive { direction=d }) }) | sec == s = ([],t)
																 | otherwise = pauseLoco t (slower s sec)
	where sec = findMerging t s d
checkMerging t s@(Section { loco=Noloco }) = ([], t)

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
checkDirection s s2 | not (containsLoco s2) = s
					| direction l == direction l2 = s2
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
examinePaused t (s:ss) | onMerge t s (direction (loco s)) = (u ++ n, m) --checkUnpauseMerge t s
					   | not (containsLoco (findNextSection t s (direction l))) = (a ++ x, y)
					   | otherwise = examinePaused t ss
	where
		l = loco s
		(x,y) = examinePaused b ss
		(a,b) = unpauseLoco t s
		(n,m) = examinePaused i ss
		(u,i) = checkUnpauseMerge t s

checkUnpauseMerge :: Layout -> Section -> ([TrackInstruction], Layout)
checkUnpauseMerge t s | containsLoco (findParallel t s (direction (loco s))) = ([],t)
					  | otherwise = unpauseLoco t s

checkAdjacent :: Layout -> Section -> [TrackInstruction]
checkAdjacent t s = checkFollowing t s

-------------------------------------------------
---
---      Speed Checks
---
-------------------------------------------------

checkSpeed :: Layout -> Message -> [TrackInstruction]
checkSpeed t s = (checkSpeedLimit sec) ++ (checkFollowing t sec)
	where
		sec = findLoco t (fromslot s)

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
findNextSection t s@(Section { prevturn=Unset }) BKW = t Map.! (head (prev s))
findNextSection t s@(Section { nextturn=Set }) FWD = t Map.! (head (tail (next s)))
findNextSection t s@(Section { prevturn=Set }) BKW = t Map.! (head (tail (prev s)))

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




-------------------------------------------------
--
--       Incoming Speed Message
--
-------------------------------------------------

parseSpeed :: [String] -> Message
parseSpeed (_:a:b:_) = SpeedMessage { fromslot=(read a), newspeed=(read b) }

speedChange :: Layout -> Message -> Layout
speedChange t m = Map.insert (sid sec) (sec { loco=((loco sec) { speed=(newspeed m) }) }) t
	where sec = findLoco t (fromslot m)


-------------------------------------------------
--
--       Incoming Direction Change Message
--
-------------------------------------------------

parseDirection :: [String] -> Message
parseDirection (_:a:b:_) = DirectionMessage { dirslot=(read a), newdir=(parsedir b) }
	where parsedir x = if x=="fwd" then FWD else BKW

changeDirection :: Layout -> Message -> Layout
changeDirection t m = Map.insert (sid sec) (sec { loco=((loco sec) { direction=(newdir m) })}) t
	where sec = findLoco t (dirslot m)


-------------------------------------------------
--
--       Incoming Section Boundary Message
--
-------------------------------------------------
-- On sensor trigger:
-- If sensor goes high, check prev/next sections for Justleft
-- If justleft, take its locomotive, set this to occupied, neighbour to empty
-- If none left, set this to justentered
-- Opposite for sensor goes low

parseSensor :: [String] -> Message
parseSensor inp = SensorMessage { upd=up a, updid=b }
	where
		(_:a:b:_) = inp
		up x = if x == "Hi" then Hi else Low

sectionSensorTrigger :: Layout -> Message -> Layout
sectionSensorTrigger track msg = checkNeighbours track (track Map.! (updid msg)) (upd msg)

--sectionSensorTrigger :: Layout -> SensorUpdate -> SensorID -> Layout
--sectionSensorTrigger track change sensor = checkNeighbours track (track Map.! sensor) change
--respondToSensor track change sensor = Map.insert sensor (checkNeighbours (track Map.! sensor) change) track

checkNeighbours :: Layout -> Section -> SensorUpdate -> Layout
checkNeighbours t s u | u == Hi = checkNextEntered t s
					  | u == Low = checkNextExited t s

checkNextEntered :: Layout -> Section -> Layout
checkNextEntered t s@(Section { next=[] })  = checkPrevEntered t (t Map.! (sid s))
checkNextEntered t s@(Section { next=(n:ns) }) | state (t Map.! n) == Justleft = moveLoco t (t Map.! n) (t Map.! (sid s))
											   | otherwise = checkNextEntered t (s { next=ns })

checkPrevEntered :: Layout -> Section -> Layout
checkPrevEntered t s@(Section { prev=[] }) = Map.insert (sid s) ((t Map.! (sid s)) { state=Justentered }) t
checkPrevEntered t s@(Section { prev=(n:ns) }) | state (t Map.! n) == Justleft = moveLoco t (t Map.! n) (t Map.! (sid s))
											   | otherwise = checkPrevEntered t (s { prev=ns })


checkNextExited :: Layout -> Section -> Layout
checkNextExited t s@(Section { next=[] })  = checkPrevExited t s
checkNextExited t s@(Section { next=(n:ns) }) | state (t Map.! n) == Justentered = moveLoco t (t Map.! (sid s)) (t Map.! n)
											  | otherwise = checkNextExited t (s { next=ns })

checkPrevExited :: Layout -> Section -> Layout
checkPrevExited t s@(Section { prev=[] }) = Map.insert (sid s) ((t Map.! (sid s)) { state=Justleft }) t
checkPrevExited t s@(Section { prev=(n:ns) }) | state (t Map.! n) == Justentered = moveLoco t (t Map.! (sid s)) (t Map.! n)
											  | otherwise = checkPrevExited t (s { prev=ns })

moveLoco :: Layout -> Section -> Section -> Layout
moveLoco t from to = clearSection (Map.insert (sid to) (to {state=Occupied,loco=(loco from)}) t) from

clearSection :: Layout -> Section -> Layout
clearSection t s = Map.insert (sid s) (s {state=Empty, loco=Noloco}) t
