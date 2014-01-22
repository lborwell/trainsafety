import Testtracks

import TrainSafetyTypes
import qualified Data.Map as Map
import Data.List (nub, nubBy)

findLoco :: Layout -> Int -> Section
findLoco t s = head (Map.elems (Map.filter (\x -> (slot (loco x)) == s) notempty))
	where
		notempty = Map.filter (containsLoco) t

makeSafe :: Layout -> MessageType -> String -> [TrackInstruction]
makeSafe t Speed m = checkSpeed t m



-------------------------------------------------
---
---      Speed Checks
---
-------------------------------------------------

checkSpeed :: Layout -> String -> [TrackInstruction]
checkSpeed t s = (checkSpeedLimit sec) ++ (checkFollowing t sec)
	where
		sec = findLoco t (read s)

checkSpeedLimit :: Section -> [TrackInstruction]
checkSpeedLimit s | speed l > speedlim s = [setLocoSpeed l (speedlim s)]
				  | otherwise = []
	where l = loco s

checkFollowing :: Layout -> Section -> [TrackInstruction] 
checkFollowing t s = findFollowing (findAdjacentLocos t s) s

findFollowing :: [(Direction,[Section])] -> Section -> [TrackInstruction]
findFollowing [] _ = []
findFollowing (x:xs) s = checkDirection x s ++ findFollowing xs s

checkDirection :: (Direction,[Section]) -> Section -> [TrackInstruction]
checkDirection (_,[]) _ = []
checkDirection (FWD,(x:xs)) s | isFollowing s x && direction (loco s) == FWD = (checkFollowingSpeeds (loco s) (loco x)) ++ (checkDirection (FWD,xs) s)
							  | isFollowing s x && direction (loco s) == BKW = (checkFollowingSpeeds (loco x) (loco s)) ++ (checkDirection (FWD,xs) s)
							  | otherwise = checkDirection (FWD,xs) s
checkDirection (BKW,(x:xs)) s | isFollowing s x && direction (loco s) == BKW = (checkFollowingSpeeds (loco s) (loco x)) ++ (checkDirection (FWD,xs) s)
							  | isFollowing s x && direction (loco s) == FWD = (checkFollowingSpeeds (loco x) (loco s)) ++ (checkDirection (FWD,xs) s)
							  | otherwise = checkDirection (BKW,xs) s

checkFollowingSpeeds :: Locomotive -> Locomotive -> [TrackInstruction]
checkFollowingSpeeds a b | speed a > speed b = [setLocoSpeed a (speed b)]
						 | otherwise = []

isFollowing :: Section -> Section -> Bool
isFollowing a b = direction (loco a) == direction (loco b)

findAdjacentLocos :: Layout -> Section -> [(Direction,[Section])]
findAdjacentLocos t s = [(FWD, locosIn (secs (next s))), (BKW, locosIn (secs (prev s)))]
	where
		locosIn = filter (containsLoco)
		secs l = getSections t l

getSections :: Layout -> [SensorID] -> [Section]
getSections t s = map (t Map.!) s

containsLoco :: Section -> Bool
containsLoco s = (loco s) /= Noloco

-------------------------------------------------
---
---      Loco Control
---
-------------------------------------------------

stopLoco :: Locomotive -> TrackInstruction
stopLoco l = "0 " ++ show (slot l) ++ " 0"

setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i