import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map
--import Debug.Trace

process :: [String] -> Layout -> [String]
process i t = makeSafe (parseInput i t)

parseInput :: [String] -> Layout -> Layout
parseInput inp@("A0":as) t = speedChange t (parseSpeed inp)

makeSafe :: Layout -> [String]
makeSafe t = checkSpeeds t


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
clearSection t s = Map.insert (sid s) (s {state=Empty, loco=noloco}) t

