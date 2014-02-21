module TrainControl where

import TrainSafetyTypes

-------------------------------------------------
---
---      Util Functions
---
-------------------------------------------------

-- | Point switch in section switch to section with ID dest
-- Direction FWD == change PREV switch, direction BKW == change NEXT switch
-- (direction represents direction of loco moving towards switch)

setSwitchToMerge :: Layout -> Section -> SensorID -> Direction -> [TrackInstruction]
setSwitchToMerge t switch dest FWD | dest == head (prev switch) = ["2 " ++ (sid switch) ++ " bkw unset"]
								   | otherwise = ["2 " ++ (sid switch) ++ " bkw set"]
setSwitchToMerge t switch dest BKW | dest == head (next switch) = ["2 " ++ (sid switch) ++ " fwd unset"]
								   | otherwise = ["2 " ++ (sid switch) ++ " fwd set"]

setSwitchToDiverge :: Layout -> Section -> SensorID -> Direction -> [TrackInstruction]
setSwitchToDiverge t switch dest FWD | dest == head (next switch) = ["2 " ++ (sid switch) ++ " fwd unset"]
									 | otherwise = ["2 " ++ (sid switch) ++ " fwd set"]
setSwitchToDiverge t switch dest BKW | dest == head (prev switch) = ["2 " ++ (sid switch) ++ " bkw unset"]
									 | otherwise = ["2 " ++ (sid switch) ++ " bkw set"]

setDivergingSwitch :: Layout -> Section -> SensorID -> Direction -> [TrackInstruction]
setDivergingSwitch t s d d' | d'==FWD && length (prev s') > 1 = setSwitchToDiverge t s d d' ++ setSwitchToMerge t s' (sid s) d'
							| d'==BKW && length (next s') > 1 = setSwitchToDiverge t s d d' ++ setSwitchToMerge t s' (sid s) d'
							| otherwise = setSwitchToDiverge t s d d'
	where
		s' = getSection t d

-- | Set loco speed to 0, loco state to waiting
pauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
pauseLoco t s | waiting l = ([],t)
			  | otherwise = ([stopLoco l], setSection t (upd s))
	where 
		upd s = s { loco=(l { waiting=True, prevspeed=speed l})}
		l = loco s

-- | Set loco speed to speed before it was paused, waiting to false
unpauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
unpauseLoco t s = ([setLocoSpeed l (prevspeed l)], setSection t (upd s))
	where
		upd s = s { loco=(l { waiting=False })}
		l = loco s

-- | Set locomotive speed to zero
stopLoco :: Locomotive -> TrackInstruction
stopLoco l = setLocoSpeed l 0

-- | Set locomotive speed to given value
setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i

setLocoDirection :: Locomotive -> Direction -> TrackInstruction
setLocoDirection l FWD = "1 " ++ show (slot l) ++ " fwd"
setLocoDirection l BKW = "1 " ++ show (slot l) ++ " bkw"

reverseLoco :: Locomotive -> [TrackInstruction]
reverseLoco l = stopLoco l : [setLocoDirection l (rev (direction l))] ++ [setLocoSpeed l (speed l)]



-- | Find the section two steps ahead
nextNextSection :: Layout -> Section -> Direction -> Section
nextNextSection t s d = findNextSection t (findNextSection t s d) d

-- | Does the section contain a loco
containsLoco :: Section -> Bool
containsLoco s = (loco s) /= Noloco

-- | Track -> All sections containing a loco
listLocos :: Layout -> [Section]
listLocos t = searchLayout t containsLoco

-- | Track -> All sections containing a waiting loco
listWaitingLocos :: Layout -> [Section]
listWaitingLocos t = filter (\x -> waiting (loco x)) (listLocos t)



-- | Are we heading towards a turnout?
onMerge :: Layout -> Section -> Direction -> Bool
onMerge t s FWD = length (prev (findNextSection t s FWD)) > 1
onMerge t s BKW = length (next (findNextSection t s BKW)) > 1

