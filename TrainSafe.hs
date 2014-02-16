module TrainSafe where

import Testtracks

import TrainControl

import TrainSafetyTypes

-- | Given layout and message from LocoNet, return updated
-- layout and instructions to make layout safe
process :: Layout -> String -> ([TrackInstruction],Layout)
process t s = makeSafe (updateLayout t msg) msg
	where msg = checkMessage (words s)

-- | Given layout and message, update layout to state of new message
updateLayout :: Layout -> (MessageType, Message) -> Layout
updateLayout t (Speed,m) = speedChange t m
updateLayout t (Direction,m) = changeDirection t m
updateLayout t (Sensor,m) = sectionSensorTrigger t m
updateLayout t (Turnout,m) = setTurnout t m

-- | Examine layout and current LocoNet message to maintain safety
makeSafe :: Layout -> (MessageType, Message) -> ([TrackInstruction], Layout)
--makeSafe t (Speed, m) = (checkSpeed t m, t)
makeSafe t (Speed, m) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(sid (findLoco t (fromslot m)))}))
makeSafe t (Direction, m) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(sid (findLoco t (dirslot m)))}))
makeSafe t (Sensor, m) = checkSensor t m
makeSafe t (Turnout, m) | containsLoco (getSection t (turnid m)) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(turnid m) }))
						| otherwise = ([],t)

-- | Parse input message, return in more usable format
checkMessage :: [String] -> (MessageType, Message)
checkMessage a@("speed":_) = (Speed, parseSpeed a)
checkMessage a@("dir":_) = (Direction, parseDirection a)
checkMessage a@("sensor":_) = (Sensor, parseSensor a)
checkMessage a@("turn":_) = (Turnout, parseTurn a)

-- | Given a sensor input message, ensure safety of layout
checkSensor :: Layout -> Message -> ([TrackInstruction], Layout)
checkSensor t s = (g++a++c++e, f)
	where
		(a,b) = checkWaitingLocos t
		(c,d) = checkMerging b (sec b)
		(e,f) = locoCheckNextSection d (sec d)
		g = sensorSpeedCheck t (sec t)
		sec m = locoFromSensorMessage m s

-- | Given sensor message, ensure speed of responsible loco is safe
sensorSpeedCheck :: Layout -> Section -> [TrackInstruction]
sensorSpeedCheck t s | l == Noloco = []
				     | otherwise = checkSpeed t (SpeedMessage { fromslot=(slot l), newspeed=(speed l)})
	where l = loco s

-- | Given a sensor message, find the location of the responsible loco
locoFromSensorMessage :: Layout -> Message -> Section
locoFromSensorMessage t (SensorMessage {upd=Hi,updid=sd}) | state sec == Occupied = sec
														  | otherwise = findAdjacentLoco t sec Hi
	where sec = getSection t sd
locoFromSensorMessage t (SensorMessage {upd=Low,updid=sd}) | state sec == Justleft = sec
														  | otherwise = findAdjacentLoco t sec Low
	where sec = getSection t sd


-- | Called by locoFromSensorMessage, finds loco when sensor was triggered in adjacent
-- section
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

-- | Check next section for a loco. Pause if it exists.
locoCheckNextSection :: Layout -> Section -> ([TrackInstruction], Layout)
locoCheckNextSection t s | not (containsLoco s) = ([],t)
						 | containsLoco (findNextSection t s (direction (loco s))) = pauseLoco t s
						 | otherwise = ([],t)

-- | Check to see if we are merging into the path of another loco
checkMerging :: Layout -> Section -> ([TrackInstruction],Layout)
checkMerging t s@(Section { loco=(Locomotive { direction=d }) }) | sec == s = ([],t)
																 | otherwise = handleMerging t s sec
	where sec = findMerging t s d
checkMerging t s@(Section { loco=Noloco }) = ([], t)

-- | Find locomotive we are merging with. If no such loco exists, return
-- input section. 
findMerging :: Layout -> Section -> Direction -> Section
findMerging t s d | onMerge t s d = checkLocoDirection t s (findParallel t s d)
				  | otherwise = s



-- | Find parallel section we are merging "in front" of.
findParallel :: Layout -> Section -> Direction -> Section
findParallel t s FWD = (getSection t (head (filter ((sid s) /=) (prev n))))
	where n = findNextSection t s FWD
findParallel t s BKW = (getSection t (head (filter ((sid s) /=) (next p))))
	where p = findNextSection t s BKW

-- | Check if we may hit loco in "merging" position
checkLocoDirection :: Layout -> Section -> Section -> Section
checkLocoDirection t s s2 | not (containsLoco s2) = s
						  | waiting l2 = s
						  | findNextSection t s (direction l) == findNextSection t s2 (direction l2) = s2
						  | otherwise = s
	where
		l = loco s
		l2 = loco s2

-- | Given two locos, return slower one
slower :: Section -> Section -> Section
slower a b | speed (loco a) > speed (loco b) = b
		   | otherwise = a

-- | When merging locos are found, pause the slower loco and ensure turnout is pointing
-- at the one that will continue to move
handleMerging :: Layout -> Section -> Section -> ([TrackInstruction],Layout)
handleMerging t s s2 | slower s s2 == s = (a ++ e,f)
					 | otherwise = (c ++ g,h)
	where
		(a,b) = pauseLoco t s
		(c,d) = pauseLoco t s2
		(e,f) = pointSwitchAt b (findNextSection b s (direction (loco s))) s2
		(g,h) = pointSwitchAt d (findNextSection d s2 (direction (loco s2))) s

-- | Set switch in from to point to to
pointSwitchAt :: Layout -> Section -> Section -> ([TrackInstruction],Layout)
pointSwitchAt t from to = setSwitchToMerge t from (sid to) (direction (loco to))

-- | Check waiting locos to see if it is safe to resume movement
checkWaitingLocos :: Layout -> ([TrackInstruction], Layout)
checkWaitingLocos t = examinePaused t (listWaitingLocos t)

-- | see checkWaitingLocos
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

-- | Is it safe to unpause loco from merge?
checkUnpauseMerge :: Layout -> Section -> ([TrackInstruction], Layout)
checkUnpauseMerge t s | containsLoco (findParallel t s (direction l)) = ([],t)
					  | containsLoco (findNextSection t s (direction l)) = ([],t)
					  | otherwise = unpauseMergingLoco t s
	where
		l = loco s

unpauseMergingLoco :: Layout -> Section -> ([TrackInstruction],Layout)
unpauseMergingLoco t s = (c ++ a,d)
	where
		(a,b) = unpauseLoco t s
		(c,d) = pointSwitchAt b (findNextSection b s (direction (loco s))) s

-------------------------------------------------
---
---      Speed Checks
---
-------------------------------------------------

-- | Check if locomotive is violating speed constraints
checkSpeed :: Layout -> Message -> [TrackInstruction]
checkSpeed t s = (checkSpeedLimit sec) ++ (checkFollowing t sec)
	where
		sec = findLoco t (fromslot s)

-- | If the locomotive is exceeding speed limit, slow it to the limit
checkSpeedLimit :: Section -> [TrackInstruction]
checkSpeedLimit s | speed l > speedlim s = [setLocoSpeed l (speedlim s)]
				  | otherwise = []
	where l = loco s

-- | If the locomotive is following another, ensure it is going slower than the leading train
checkFollowing :: Layout -> Section -> [TrackInstruction]
checkFollowing t s = speedCheckNextSection t s (nextNextSection t s (direction (loco s)))

speedCheckNextSection :: Layout -> Section -> Section -> [TrackInstruction]
speedCheckNextSection t s1 s2 | not (containsLoco s2) = []
							  | direction l1 /= direction l2 = (stopLoco l1) : [stopLoco l2]
							  | otherwise = checkFollowingSpeeds l1 l2
	where
		l1 = loco s1
		l2 = loco s2

checkFollowingSpeeds :: Locomotive -> Locomotive -> [TrackInstruction]
--checkFollowingSpeeds a b | speed a > speed b = [setLocoSpeed a (speed b)]
--						 | otherwise = []
checkFollowingSpeeds _ _ = []








test :: ([TrackInstruction],Layout) -> [String] -> ([TrackInstruction],Layout)
test a b = foldl (combne) a b

speedReset = ["speed 8 0","speed 9 0"]
doubleTest = ["speed 9 113","speed 8 113","sensor Hi B2","sensor Low A2","speed 8 113","sensor Hi D2","sensor Low C2"]
switchFlipTest = ["speed 9 90","speed 8 113","turn B2 fwd set","speed 9 0","turn C1 bkw set","sensor Hi C1","sensor Low B2","sensor Hi D1","sensor Low C1","speed 9 90"]
parallelWalk = ["sensor Hi B1","sensor Low A1","sensor Hi B2","sensor Low A2","sensor Hi C1","sensor Low B1","sensor Hi C2","sensor Low B2"]
following = ["sensor Hi B1","sensor Low A1","speed 8 113","sensor Hi D1","sensor Low C1"]
singleMerge = ["speed 9 113","sensor Hi C1","sensor Low B2"]

combne :: ([TrackInstruction],Layout) -> String -> ([TrackInstruction],Layout)
combne a b = ((fst a) ++ [b ++ ":"] ++ (c) ++ [""], d)
	where (c,d) = process (snd a) b