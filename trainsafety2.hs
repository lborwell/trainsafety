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
makeSafe t Direction m = undefined
makeSafe t Sensor m = undefined


checkAdjacent :: Layout -> Section -> [TrackInstruction]
checkAdjacent t s = undefined

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

checkFollowingSpeeds :: Locomotive -> Locomotive -> [TrackInstruction]
checkFollowingSpeeds a b | speed a > speed b = [setLocoSpeed a (speed b)]
						 | otherwise = []

containsLoco :: Section -> Bool
containsLoco s = (loco s) /= Noloco

checkFollowing :: Layout -> Section -> [TrackInstruction]
checkFollowing t s = checkNextSection s (findNextSection t s (direction (loco s)))

checkNextSection :: Section -> Section -> [TrackInstruction]
checkNextSection s1 s2 | not (containsLoco s2) = []
					   | direction (loco s1) /= direction (loco s2) = []
					   | otherwise = checkFollowingSpeeds (loco s1) (loco s2)

findNextSection :: Layout -> Section -> Direction -> Section
findNextSection t s@(Section { nextturn=Noturn }) FWD = t Map.! (head (next s))
findNextSection t s@(Section { prevturn=Noturn }) BKW = t Map.! (head (prev s))
findNextSection t s@(Section { nextturn=Unset }) FWD = t Map.! (head (next s))
findNextSection t s@(Section { prevturn=Unset }) BKW = t Map.! (head (next s))
findNextSection t s@(Section { nextturn=Set }) FWD = t Map.! (head (tail (next s)))
findNextSection t s@(Section { prevturn=Set }) BKW = t Map.! (head (tail (next s)))

-------------------------------------------------
---
---      Loco Control
---
-------------------------------------------------

stopLoco :: Locomotive -> TrackInstruction
stopLoco l = "0 " ++ show (slot l) ++ " 0"

setLocoSpeed :: Locomotive -> Int -> TrackInstruction
setLocoSpeed l i = "0 " ++ show (slot l) ++ " " ++ show i