import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map
import Data.List (nub, nubBy)
--import Debug.Trace

-- Things to detect:
-- Two locos heading straight for each other (noting turnout positions) (TODO: turnouts)
-- One loco catching another  (done)
-- Two locos merging on a turnout  (currently both stop)
-- Investigate emitting instructions as they are found

process :: [String] -> Layout -> [TrackInstruction]
process i t = makeSafe (handleInput i t)

handleInput :: [String] -> Layout -> Layout
handleInput inp@("A0":as) t = speedChange t (parseSpeed inp)
handleInput inp@("sensor":as) t = sectionSensorTrigger t (parseSensor inp)

makeSafe :: Layout -> [TrackInstruction]
makeSafe t = undefined


-------------------------------------------------
---
---      Detect/Handle Parallel Locos
---
-------------------------------------------------

checkParallel :: Layout -> [(SensorID,SensorID)] -> [TrackInstruction]
checkParallel _ [] = []
checkParallel t (s:ss) | areSectionsParallel t s = handleParallel t s ++ checkParallel t ss
					   | otherwise = checkParallel t ss

areSectionsParallel :: Layout -> (SensorID,SensorID) -> Bool
areSectionsParallel t (a,b) = any (True == ) (map (\x -> checkNext t s2 (t Map.! x)) (prev s1)) || any (True == ) (map (\x -> checkPrev t s2 (t Map.! x)) (next s1))
	where
		s1 = t Map.! a
		s2 = t Map.! b

checkNext :: Layout -> Section -> Section -> Bool
checkNext t a b = any (True ==) $ map (\x -> a == (t Map.! x)) (next b)

checkPrev :: Layout -> Section -> Section -> Bool
checkPrev t a b = any (True ==) $ map (\x -> a == (t Map.! x)) (prev b)

handleParallel :: Layout -> (SensorID, SensorID) -> [TrackInstruction]
handleParallel t (a,b) | oppositeParallelDirection s1 s2 = []
					   | otherwise = sameParallelDirection s1 s2
	where
		s1 = t Map.! a
		s2 = t Map.! b

oppositeParallelDirection :: Section -> Section -> Bool
oppositeParallelDirection a b = direction (loco a) /= direction (loco b)

sameParallelDirection :: Section -> Section -> [TrackInstruction]
sameParallelDirection a@(Section {loco=(Locomotive{direction=FWD})}) b | strait a && strait b = []
																	   | otherwise = stopLoco (loco a) : [stopLoco (loco b)]
	where
		strait x = (nextturn x) == Noturn
sameParallelDirection a@(Section {loco=(Locomotive{direction=BKW})}) b | strait a && strait b = []
																	   | otherwise = stopLoco (loco a) : [stopLoco (loco b)]
	where
		strait x = (prevturn x) == Noturn


-------------------------------------------------
--
--       Detect/Handle Adjacent Locos
--
-------------------------------------------------

checkAdjacent :: Layout -> [(SensorID,SensorID)] -> [TrackInstruction]
checkAdjacent _ [] = []
checkAdjacent t (s:ss) | areSectionsAdjacent t s = handleAdjacent t s ++ checkAdjacent t ss
					   | otherwise = checkAdjacent t ss

areSectionsAdjacent :: Layout -> (SensorID,SensorID) -> Bool
areSectionsAdjacent t (a,b) | a `elem` (next s ++ prev s) = True
					  		| otherwise = False
	where s = (t Map.! b)

handleAdjacent :: Layout -> (SensorID,SensorID) -> [TrackInstruction]
handleAdjacent t (a,b) | (direction (loco s1)) == (direction (loco s2)) = following s1 s2
					   | otherwise = headOn s1 s2
	where
		s1 = (t Map.! a)
		s2 = (t Map.! b)

following :: Section -> Section -> [TrackInstruction]
following a@(Section { loco=(Locomotive { direction=FWD }) }) b | (sid b) `elem` (next a) = matchSpeed a b --a is following b
																  | otherwise = matchSpeed b a
following a@(Section { loco=(Locomotive { direction=BKW }) }) b | (sid b) `elem` (prev a) = matchSpeed a b --a is following b
																  | otherwise = matchSpeed b a

matchSpeed :: Section -> Section -> [TrackInstruction]
matchSpeed a b | (speed la) > (speed lb) = [setLocoSpeed la (speed lb)]
			   | otherwise = []
	where
		la = loco a
		lb = loco b

headOn :: Section -> Section -> [TrackInstruction]
headOn a b = stopLoco (loco a) : [stopLoco (loco b)]


-------------------------------------------------
--
--       Detect Proximity
--
-------------------------------------------------

-- Track -> All sections containing a loco
listLocos :: Layout -> Layout
listLocos t = Map.filter (\x -> (loco x) /= Noloco) t

-- Track -> List of sections within 2 track breaks of each other both containing a locomotive
findProximity :: Layout -> [(SensorID, SensorID)]
findProximity t = nubBy (\(x,y) (z,q) -> (x==z && y==q) || (x==q && y==z)) $ checkLocos t (Map.elems (listLocos t))

checkLocos :: Layout -> [Section] -> [(SensorID, SensorID)]
checkLocos _ [] = []
checkLocos t (s:ss) = map (\x -> (sid s,sid x)) (filter (\y -> (loco y) /= Noloco) (nearbySections t s 2)) ++ checkLocos t ss

nearbySections :: Layout -> Section -> Int -> [Section]
nearbySections t s i = nub $ filter (\x -> x /= s) (nearbySections' t s i)

-- Find sections within Int transitions
nearbySections' :: Layout -> Section -> Int -> [Section]
nearbySections' _ s 0 = [s]
nearbySections' _ (Section {prev=[], next=[]}) _ = []
nearbySections' t s@(Section {prev=(p:ps), next=[]}) i = (nearbySections' t (t Map.! p) (i-1)) ++ (nearbySections' t (s {prev=ps}) i)
nearbySections' t s@(Section {next=(n:ns)}) i = [s] ++ (nearbySections' t (t Map.! n) (i-1)) ++ (nearbySections' t (s {next=ns}) i)


-------------------------------------------------
--
--       Slow Down Speeding Trains
--
-------------------------------------------------

checkSpeeds :: Layout -> [TrackInstruction]
checkSpeeds t = slowSpeeds (Map.elems speeders)
	where	speeders = Map.filter speeding t
		speeding (Section { speedlim=sl, loco=l }) = if (speed l) > sl then True else False

slowSpeeds :: [Section] -> [TrackInstruction]
slowSpeeds [] = []
slowSpeeds (t:ts) = setLocoSpeed (loco t) (speedlim t) : (slowSpeeds ts)


-------------------------------------------------
--
--       Incoming Speed Message
--
-------------------------------------------------

parseSpeed :: [String] -> SpeedMessage
parseSpeed inp = SpeedMessage { fromslot=x, newspeed=y }
	where (_:x:y:_) = readinp (makeHex inp)

readinp :: [String] -> [Int]
readinp r = map read r

makeHex :: [String] -> [String]
makeHex st = map (\x -> "0x" ++ x) st

speedChange :: Layout -> SpeedMessage -> Layout
speedChange t m = Map.insert (sid sec) (sec { loco=((loco sec) { speed=(newspeed m) }) }) t
	where sec = findLoco t (fromslot m)

findLoco :: Layout -> Int -> Section
findLoco t s = head (Map.elems (Map.filter (\x -> (slot (loco x)) == s) t))


-------------------------------------------------
--
--       Incoming Direction Change Message
--
-------------------------------------------------

changeDirection :: Layout -> DirectionMessage -> Layout
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

parseSensor :: [String] -> SensorMessage
parseSensor inp = SensorMessage { upd=up a, updid=b }
	where
		(_:a:b:_) = inp
		up x = if x == "hi" then Hi else Low

sectionSensorTrigger :: Layout -> SensorMessage -> Layout
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


-------------------------------------------------
---
---      Loco Control Message Generators
---
-------------------------------------------------

stopLoco :: Locomotive -> TrackInstruction
stopLoco l = "0 " ++ show (slot l) ++ " 0"

setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i