module TrainSafe where

import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map

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
makeSafe t (Turnout, m) | containsLoco (t Map.! (turnid m)) = makeSafe t (Sensor, (SensorMessage { upd=Hi, updid=(turnid m) }))
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
locoFromSensorMessage t (SensorMessage {upd=Hi,updid=sd}) | state (t Map.! sd) == Occupied = (t Map.! sd)
														  | otherwise = findAdjacentLoco t (t Map.! sd) Hi
locoFromSensorMessage t (SensorMessage {upd=Low,updid=sd}) | state (t Map.! sd) == Justleft = (t Map.! sd)
														  | otherwise = findAdjacentLoco t (t Map.! sd) Low


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

-- | Are we heading towards a turnout?
onMerge :: Layout -> Section -> Direction -> Bool
onMerge t s FWD = length (prev (findNextSection t s FWD)) > 1
onMerge t s BKW = length (next (findNextSection t s BKW)) > 1

-- | Find parallel section we are merging "in front" of.
findParallel :: Layout -> Section -> Direction -> Section
findParallel t s FWD = (t Map.! (head (filter ((sid s) /=) (prev n))))
	where n = findNextSection t s FWD
findParallel t s BKW = (t Map.! (head (filter ((sid s) /=) (next p))))
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
handleMerging t s s2 | slower s s2 == s = (a ++ [pointSwitchAt (findNextSection t s (direction (loco s))) s2],b)
					 | otherwise = (c ++ [pointSwitchAt (findNextSection t s2 (direction (loco s2))) s],d)
	where
		(a,b) = pauseLoco t s
		(c,d) = pauseLoco t s2

-- | Set switch in from to point to to
pointSwitchAt :: Section -> Section -> TrackInstruction
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
unpauseMergingLoco t s = (pointSwitchAt (findNextSection t s (direction (loco s))) s : a,b)
	where (a,b) = unpauseLoco t s

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


-------------------------------------------------
---
---      Util Functions
---
-------------------------------------------------

-- | Point switch in section switch to section with ID dest
-- Direction FWD == change PREV switch, direction BKW == change NEXT switch
-- (direction represents direction of loco moving towards switch)
setSwitchToMerge :: Section -> SensorID -> Direction -> TrackInstruction
setSwitchToMerge switch dest FWD | dest == head (prev switch) = "2 " ++ (sid switch) ++ " bkw unset"
							| otherwise = "2 " ++ (sid switch) ++ " bkw set"
setSwitchToMerge switch dest BKW | dest == head (next switch) = "2 " ++ (sid switch) ++ " fwd unset"
							| otherwise = "2 " ++ (sid switch) ++ " fwd set"

-- | Set loco speed to 0, loco state to waiting
pauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
pauseLoco t s = ([stopLoco l], Map.insert (sid s) (upd s) t)
	where 
		upd s = s { loco=(l { waiting=True, prevspeed=speed l})}
		l = loco s

-- | Set loco speed to speed before it was paused, waiting to false
unpauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
unpauseLoco t s = ([setLocoSpeed l (prevspeed l)], Map.insert (sid s) (upd s) t)
	where
		upd s = s { loco=(l { waiting=False })}
		l = loco s

-- | Set locomotive speed to zero
stopLoco :: Locomotive -> TrackInstruction
stopLoco l = setLocoSpeed l 0

-- | Set locomotive speed to given value
setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i

-- | Find the next section in a given direction, noting turnout positions
findNextSection :: Layout -> Section -> Direction -> Section
findNextSection t s@(Section { nextturn=Noturn }) FWD = t Map.! (head (next s))
findNextSection t s@(Section { prevturn=Noturn }) BKW = t Map.! (head (prev s))
findNextSection t s@(Section { nextturn=Unset }) FWD = t Map.! (head (next s))
findNextSection t s@(Section { prevturn=Unset }) BKW = t Map.! (head (prev s))
findNextSection t s@(Section { nextturn=Set }) FWD = t Map.! (head (tail (next s)))
findNextSection t s@(Section { prevturn=Set }) BKW = t Map.! (head (tail (prev s)))

-- | Find the section two steps ahead
nextNextSection :: Layout -> Section -> Direction -> Section
nextNextSection t s d = findNextSection t (findNextSection t s d) d

-- | Does the section contain a loco
containsLoco :: Section -> Bool
containsLoco s = (loco s) /= Noloco

-- | Track -> All sections containing a loco
listLocos :: Layout -> [Section]
listLocos t = Map.elems (Map.filter (containsLoco) t)

-- | Track -> All sections containing a waiting loco
listWaitingLocos :: Layout -> [Section]
listWaitingLocos t = filter (\x -> waiting (loco x)) (listLocos t)

-- | Return section containing loco with given slot number
findLoco :: Layout -> Int -> Section
findLoco t s = head (Map.elems (Map.filter (\x -> (slot (loco x)) == s) notempty))
	where
		notempty = Map.filter (containsLoco) t




-------------------------------------------------
--
--       Incoming Speed Message
--
-------------------------------------------------

-- | Parse incoming speed message into SpeedMessage
parseSpeed :: [String] -> Message
parseSpeed (_:a:b:_) = SpeedMessage { fromslot=(read a), newspeed=(read b) }

-- | Update layout to represent new loco speed
speedChange :: Layout -> Message -> Layout
speedChange t m = Map.insert (sid sec) (sec { loco=((loco sec) { speed=(newspeed m) }) }) t
	where sec = findLoco t (fromslot m)


-------------------------------------------------
--
--       Incoming Direction Change Message
--
-------------------------------------------------

-- | Parse incoming direction message into DirectionMessage
parseDirection :: [String] -> Message
parseDirection (_:a:b:_) = DirectionMessage { dirslot=(read a), newdir=(parsedir b) }
	where parsedir x = if x=="fwd" then FWD else BKW

-- | Update layout to represent new loco direction
changeDirection :: Layout -> Message -> Layout
changeDirection t m = Map.insert (sid sec) (sec { loco=((loco sec) { direction=(newdir m) })}) t
	where sec = findLoco t (dirslot m)


-------------------------------------------------
--
--       Incoming Turnout Change Message
--
-------------------------------------------------

-- | Parse incoming turnout message into TurnoutMessage
parseTurn :: [String] -> Message
parseTurn (_:a:b:c:_) = TurnoutMessage { turnid=a, side=(end b), newstate=(set c)  }
	where
		end x = if x=="fwd" then FWD else BKW
		set x = if x=="set" then Set else Unset

-- | Update layout to represent new turnout setting
setTurnout :: Layout -> Message -> Layout
setTurnout t m | side m == FWD = Map.insert (sid sec) (sec { nextturn=(newstate m) }) t
			   | side m == BKW = Map.insert (sid sec) (sec { prevturn=(newstate m) }) t
	where sec = t Map.! (turnid m)

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

checkNeighbours :: Layout -> Section -> SensorUpdate -> Layout
checkNeighbours t s Hi | state nxt == Justleft && correctDirection s Hi NXT = moveLoco t nxt s
					   | state prv == Justleft && correctDirection s Hi PRV = moveLoco t prv s
					   | otherwise = Map.insert (sid s) ((t Map.! (sid s)) { state=Justentered }) t
	where
		nxt = findNextSection t s FWD
		prv = findNextSection t s BKW
checkNeighbours t s Low | state nxt == Justentered && correctDirection s Low NXT = moveLoco t s nxt
						| state prv == Justentered && correctDirection s Low PRV = moveLoco t s prv
						| otherwise = Map.insert (sid s) ((t Map.! (sid s)) { state=Justleft }) t
	where
		nxt = findNextSection t s FWD
		prv = findNextSection t s BKW

data D = NXT | PRV

correctDirection :: Section -> SensorUpdate -> D -> Bool
correctDirection ns _ _ | loco ns == Noloco = True
correctDirection ns Hi NXT | direction (loco ns) == BKW = True
						   | otherwise = False
correctDirection ns Hi PRV | direction (loco ns) == FWD = True
						   | otherwise = False
correctDirection ns Low NXT | direction (loco ns) == FWD = True
							| otherwise = False
correctDirection ns Low PRV | direction (loco ns) == BKW = True
							| otherwise = False


moveLoco :: Layout -> Section -> Section -> Layout
moveLoco t from to = clearSection (Map.insert (sid to) (to {state=Occupied,loco=(loco from)}) t) from

clearSection :: Layout -> Section -> Layout
clearSection t s = Map.insert (sid s) (s {state=Empty, loco=Noloco}) t






test :: ([TrackInstruction],Layout) -> [String] -> ([TrackInstruction],Layout)
test a b = foldl (combne) a b

speedReset = ["speed 8 0","speed 9 0"]
doubleTest = ["speed 9 113","speed 8 113","sensor Hi B2","sensor Low A2","speed 8 113","sensor Hi D2","sensor Low C2"]
switchFlipTest = ["speed 9 90","speed 8 113","turn B2 fwd set","speed 9 0","turn C1 bkw set","sensor Hi C1","sensor Low B2","sensor Hi D1","sensor Low C1","speed 9 90"]
parallelWalk = ["sensor Hi B1","sensor Low A1","sensor Hi B2","sensor Low A2","sensor Hi C1","sensor Low B1","sensor Hi C2","sensor Low B2"]

combne :: ([TrackInstruction],Layout) -> String -> ([TrackInstruction],Layout)
combne a b = ((fst a) ++ (c) ++ [""], d)
	where (c,d) = process (snd a) b