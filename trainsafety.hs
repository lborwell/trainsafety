import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map
import Data.List (nub, nubBy)
--import Debug.Trace

-- Things to detect:
-- Two locos heading straight for each other (noting turnout positions)
-- One loco catching another
-- Two locos merging on a turnout
-- Investigate emitting instructions as they are found

process :: [String] -> Layout -> [String]
process i t = makeSafe (handleInput i t)

handleInput :: [String] -> Layout -> Layout
handleInput inp@("A0":as) t = speedChange t (parseSpeed inp)

makeSafe :: Layout -> [String]
makeSafe t = checkSpeeds t


-------------------------------------------------
--
--       Detect Proximity
--
-------------------------------------------------

listLocos :: Layout -> Layout
listLocos t = Map.filter (\x -> (loco x) /= Noloco) t

findProximity :: Layout -> [(String, String)]
findProximity t = nubBy (\(x,y) (z,q) -> if (x==z && y==q) || (x==q && y==z) then True else False) $ checkLocos t (Map.elems (listLocos t))

checkLocos :: Layout -> [Section] -> [(String, String)]
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

checkSpeeds :: Layout -> [String]
checkSpeeds t = slowSpeeds (Map.elems speeders)
	where	speeders = Map.filter speeding t
		speeding (Section { speedlim=sl, loco=l }) = if (speed l) > sl then True else False

slowSpeeds :: [Section] -> [String]
slowSpeeds [] = []
slowSpeeds (t:ts) = ("0 " ++ show (slot (loco t)) ++ " " ++ show (speedlim t)) : (slowSpeeds ts)


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

sectionSensorTrigger :: Layout -> SensorUpdate -> SensorID -> Layout
sectionSensorTrigger track change sensor = checkNeighbours track (track Map.! sensor) change
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