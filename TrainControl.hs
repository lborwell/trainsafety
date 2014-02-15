module TrainControl where

import TrainSafetyTypes
import qualified Data.Map as Map

-------------------------------------------------
---
---      Util Functions
---
-------------------------------------------------

-- | Point switch in section switch to section with ID dest
-- Direction FWD == change PREV switch, direction BKW == change NEXT switch
-- (direction represents direction of loco moving towards switch)
-- No LocoNet message occurs, so switching must update layout on the assumption
-- that it has worked.
setSwitchToMerge :: Layout -> Section -> SensorID -> Direction -> ([TrackInstruction],Layout)
setSwitchToMerge t switch dest FWD | dest == head (prev switch) = (["2 " ++ (sid switch) ++ " bkw unset"],setSwitchToMergeLayout t switch BKW Unset)
								   | otherwise = (["2 " ++ (sid switch) ++ " bkw set"],setSwitchToMergeLayout t switch BKW Set)
setSwitchToMerge t switch dest BKW | dest == head (next switch) = (["2 " ++ (sid switch) ++ " fwd unset"],setSwitchToMergeLayout t switch FWD Unset)
								   | otherwise = (["2 " ++ (sid switch) ++ " fwd set"],setSwitchToMergeLayout t switch FWD Set)

setSwitchToMergeLayout :: Layout -> Section -> Direction -> TurnoutState -> Layout
setSwitchToMergeLayout t switch d s = setTurnout t (TurnoutMessage{turnid=(sid switch), side=d, newstate=s })

-- | Set loco speed to 0, loco state to waiting
pauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
pauseLoco t s | waiting l = ([],t)
			  | otherwise = ([stopLoco l], Map.insert (sid s) (upd s) t)
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
