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

setSwitchToMerge :: Section -> SensorID -> Direction -> [TrackInstruction]
setSwitchToMerge switch dest FWD | dest == head (prev switch) = ["2 " ++ (sid switch) ++ " bkw unset"]
							     | otherwise = ["2 " ++ (sid switch) ++ " bkw set"]
setSwitchToMerge switch dest BKW | dest == head (next switch) = ["2 " ++ (sid switch) ++ " fwd unset"]
								 | otherwise = ["2 " ++ (sid switch) ++ " fwd set"]

setSwitchToDiverge :: Section -> SensorID -> Direction -> [TrackInstruction]
setSwitchToDiverge switch dest FWD | dest == head (next switch) = ["2 " ++ (sid switch) ++ " fwd unset"]
								   | otherwise = ["2 " ++ (sid switch) ++ " fwd set"]
setSwitchToDiverge switch dest BKW | dest == head (prev switch) = ["2 " ++ (sid switch) ++ " bkw unset"]
								   | otherwise = ["2 " ++ (sid switch) ++ " bkw set"]

setDivergingSwitch :: Layout -> Section -> SensorID -> Direction -> [TrackInstruction]
setDivergingSwitch t s d d' | d'==FWD && length (prev s') > 1 = setSwitchToDiverge s d d' ++ setSwitchToMerge s' (sid s) d'
							| d'==BKW && length (next s') > 1 = setSwitchToDiverge s d d' ++ setSwitchToMerge s' (sid s) d'
							| otherwise = setSwitchToDiverge s d d'
	where
		s' = getSection t d

-- | Set loco speed to 0, loco state to waiting
pauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
pauseLoco t s | waiting l = ([],t)
			  | otherwise = ([stopLoco l], setSection t u)
	where 
		u = s { loco=(l { waiting=True, prevspeed=speed l})}
		l = loco s

-- | Set loco speed to speed before it was paused, waiting to false
unpauseLoco :: Layout -> Section -> ([TrackInstruction], Layout)
unpauseLoco t s = ([setLocoSpeed l (prevspeed l)], setSection t u)
	where
		u = s { loco=(l { waiting=False })}
		l = loco s

-- | Set locomotive speed to zero
stopLoco :: Locomotive -> TrackInstruction
stopLoco l = setLocoSpeed l 0

-- | Set locomotive speed to given value
setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i

setLocoDirection :: Locomotive -> Direction -> [TrackInstruction]
setLocoDirection l d | direction l == d = []
					 | otherwise = reverseLoco l
--setLocoDirection l FWD = "1 " ++ show (slot l) ++ " fwd"
--setLocoDirection l BKW = "1 " ++ show (slot l) ++ " bkw"

reverseLoco :: Locomotive -> [TrackInstruction]
--reverseLoco l = stopLoco l : [setLocoDirection l (rev (direction l))] ++ [setLocoSpeed l (speed l)]
reverseLoco l = stopLoco l : ["1 " ++ show (slot l) ++ " " ++ d] ++ [setLocoSpeed l (speed l)]
	where
		d = if (direction l) == FWD then "bkw" else "fwd"



-- | Find the section two steps ahead
nextNextSection :: Layout -> Section -> Direction -> Section
nextNextSection t s d = findNextSection t (findNextSection t s d) d



-- | Track -> All sections containing a waiting loco
listWaitingLocos :: Layout -> [Section]
listWaitingLocos t = filter (\x -> waiting (loco x)) (listLocos t)



-- | Are we heading towards a turnout?
onMerge :: Layout -> Section -> Direction -> Bool
onMerge t s FWD = length (prev (findNextSection t s FWD)) > 1
onMerge t s BKW = length (next (findNextSection t s BKW)) > 1

