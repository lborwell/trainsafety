module TrainSafe where

import Testtracks()

import TrainControl

import TrainSafetyTypes

process :: Layout -> Message -> ([TrackInstruction],Layout)
process t m@(SpeedMessage {}) = makeSafe t (Speed, m)
process t m@(DirectionMessage {}) = makeSafe t (Direction, m)
process t m@(SensorMessage {}) = makeSafe t (Sensor, m)
process t m@(TurnoutMessage {}) = makeSafe t (Turnout, m)

-- | Examine layout and current LocoNet message to maintain safety
makeSafe :: Layout -> (MessageType, Message) -> ([TrackInstruction], Layout)
--makeSafe t (Speed, m) = (checkSpeed t m, t)
makeSafe t (Speed, m) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(sid (getSectionBySlot t (fromslot m)))}))
makeSafe t (Direction, m) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(sid (getSectionBySlot t (dirslot m)))}))
makeSafe t (Sensor, m) = checkSensor t m
makeSafe t (Turnout, m) | containsLoco (getSection t (turnid m)) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(turnid m) }))
						| otherwise = ([],t)

-- | Given a sensor input message, ensure safety of layout
checkSensor :: Layout -> Message -> ([TrackInstruction], Layout)
checkSensor t s = (i++g++a++c++e, h)
	where
		(a,b) = checkWaitingLocos t
		(c,d) = checkMerging b (sec b)
		(e,f) = locoCheckNextSection d (sec d)
		(g,h) = sensorSpeedCheck f (sec f)
		i = checkAgainstSwitch t (sec t)
		sec m = locoFromSensorMessage m s

-- | Given sensor message, ensure speed of responsible loco is safe
sensorSpeedCheck :: Layout -> Section -> ([TrackInstruction], Layout)
sensorSpeedCheck t s | l == Noloco = ([],t)
				     | otherwise = checkSpeed t (SpeedMessage { fromslot=(slot l), newspeed=(speed l)})
	where l = loco s

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
checkMerging t (Section { loco=Noloco }) = ([], t)

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
handleMerging t s s2 | slower s s2 == s = (a ++ e,b)
					 | otherwise = (c ++ f,d)
	where
		(a,b) = pauseLoco t s
		(c,d) = pauseLoco t s2
		e = pointSwitchAt (findNextSection b s (direction (loco s))) s2
		f = pointSwitchAt (findNextSection d s2 (direction (loco s2))) s

-- | Set switch in from to point to to
pointSwitchAt :: Section -> Section -> [TrackInstruction]
pointSwitchAt from to = setSwitchToMerge from (sid to) (direction (loco to))

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
unpauseMergingLoco t s = (c ++ a,b)
	where
		(a,b) = unpauseLoco t s
		c = pointSwitchAt (findNextSection b s (direction (loco s))) s

-------------------------------------------------
---
---      Speed Checks
---
-------------------------------------------------

-- | Check if locomotive is violating speed constraints
checkSpeed :: Layout -> Message -> ([TrackInstruction], Layout)
checkSpeed t s = ((fst sl) ++ (checkFollowing t sec), snd sl)
	where
		sl = checkSpeedLimit t sec
		sec = getSectionBySlot t (fromslot s)

-- | If the locomotive is exceeding speed limit, slow it to the limit
checkSpeedLimit :: Layout -> Section -> ([TrackInstruction], Layout)
checkSpeedLimit t s | speed l < speedlim s = ([], updateLoco t (l { fastspeed=(speed l) }))
					| speed l > speedlim s = ([setLocoSpeed l (speedlim s)], updateLoco t (l { fastspeed=(speed l) }))
					| otherwise = ([],t)
	where l = loco s

-- | If the locomotive is following another, ensure it is going slower than the leading train
checkFollowing :: Layout -> Section -> [TrackInstruction]
checkFollowing t s = speedCheckNextSection s (nextNextSection t s (direction (loco s)))

speedCheckNextSection :: Section -> Section -> [TrackInstruction]
speedCheckNextSection s1 s2 | not (containsLoco s2) = []
							  | direction l1 /= direction l2 = (stopLoco l1) : [stopLoco l2]
							  | otherwise = checkFollowingSpeeds l1 l2
	where
		l1 = loco s1
		l2 = loco s2

checkFollowingSpeeds :: Locomotive -> Locomotive -> [TrackInstruction]
--checkFollowingSpeeds a b | speed a > speed b = [setLocoSpeed a (speed b)]
--						 | otherwise = []
checkFollowingSpeeds _ _ = []




checkAgainstSwitch :: Layout -> Section -> [TrackInstruction]
checkAgainstSwitch t s | intoSwitch t s d = setSwitchToMerge (findNextSection t s d) (sid s) d
					   | otherwise = []
	where
		d = direction (loco s)

intoSwitch :: Layout -> Section -> Direction -> Bool
intoSwitch t s FWD = length (prev (findNextSection t s FWD)) > 1
intoSwitch t s BKW = length (next (findNextSection t s BKW)) > 1 



{-test :: ([TrackInstruction],Layout) -> [String] -> ([TrackInstruction],Layout)
test a b = foldl (combne) a b

speedReset = ["speed 8 0","speed 9 0"]
doubleTest = ["speed 9 113","speed 8 113","sensor Hi B2","sensor Low A2","speed 8 113","sensor Hi D2","sensor Low C2"]
switchFlipTest = ["speed 9 90","speed 8 113","turn B2 fwd set","speed 9 0","turn C1 bkw set","sensor Hi C1","sensor Low B2","sensor Hi D1","sensor Low C1","speed 9 90"]
parallelWalk = ["sensor Hi B1","sensor Low A1","sensor Hi B2","sensor Low A2","sensor Hi C1","sensor Low B1","sensor Hi C2","sensor Low B2"]
following = ["sensor Hi B1","sensor Low A1","speed 8 113","sensor Hi D1","sensor Low C1"]
singleMerge = ["speed 9 113","sensor Hi C1","sensor Low B2"]

combne :: ([TrackInstruction],Layout) -> String -> ([TrackInstruction],Layout)
combne a b = ((fst a) ++ [b ++ ":"] ++ (c) ++ [""], d)
	where (c,d) = process (snd a) b-}