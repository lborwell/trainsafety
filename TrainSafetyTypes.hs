module TrainSafetyTypes where

import qualified Data.Map as Map

type Layout = Map.Map String Section
type TrackInstruction = String
type SensorID = String
data Direction = FWD | BKW deriving (Eq)
instance Show Direction where
	show d = if d==FWD then "Forward" else "Backward"

data TurnoutState = Set | Unset | Noturn deriving (Show, Eq)
data State = Justleft | Justentered | Empty | Occupied deriving (Show, Eq)
data SensorUpdate = Hi | Low deriving (Show, Eq)
data MessageType = Speed | Direction | Sensor | Turnout deriving (Show, Eq)

data Message = SpeedMessage { fromslot :: Int
							, newspeed :: Int
							} |
			   DirectionMessage { dirslot :: Int
								, newdir :: Direction 
								} |
			   SensorMessage { upd :: SensorUpdate
							 , updid :: SensorID
							 } deriving (Show, Eq)

data Section = Section { state :: State
					   , prev :: [SensorID]
					   , next :: [SensorID]
					   , speedlim :: Int
					   , loco :: Locomotive
					   , sid :: SensorID
					   , prevturn :: TurnoutState
					   , nextturn :: TurnoutState
					   }

instance Eq Section where
	Section { sid=x } == Section { sid=y } = x == y

instance Show Section where
	show s@(Section{loco=l}) | l /= Noloco = sid s ++ ": " ++ show (state s) ++ " " ++ show (slot l)
							 | otherwise = sid s ++ ": " ++ show (state s)

--instance Show Section where
--	show s@(Section{prev=[], next=[n]}) = stateshow s ++ "Next zone is " ++ show n ++ "."
--	show s@(Section{prev=[p], next=[]}) = stateshow s ++ "Next zone is " ++ show p ++ "."
--	show s@(Section{prev=[p], next=[n]}) = stateshow s ++ "Previous zone is " ++ show p ++ ". Next zone is " ++ show n ++ "."
--	show s@(Section{prev=[p,p2], next=[n]}) = stateshow s ++ "Previous zones are " ++ p ++ ", " ++ p2 ++ 
--		". Turnout is " ++ show (prevturn s) ++ ". Next zone is " ++ n ++ "."
--	show s@(Section{prev=[p], next=[n,n2]}) = stateshow s ++ "Previous zone is " ++ p ++ 
--		". Next zones are " ++ n ++ ", " ++ n2 ++ ". Turnout is " ++ show (nextturn s)

stateshow :: Section -> String
stateshow s = "Section " ++ show (sid s) ++ " is " ++ show (state s) ++ ", containing " ++ show (loco s) ++ ". Speedlimit " ++ show (speedlim s) ++ ". " 

data Locomotive = Locomotive { slot :: Int
							 , speed :: Int
							 , ide :: Int
							 , direction :: Direction
							 , waiting :: Bool
							 , prevspeed :: Int
							 } | Noloco

instance Eq Locomotive where
	Noloco == Noloco = True
	Locomotive { slot=x } == Locomotive { slot = y } = x == y
	_ == _ = False

instance Show Locomotive where
	show Noloco = "No locomotive"
	show l@(Locomotive { waiting=True, prevspeed=x }) = (locoshow l) ++ ". This loco is waiting to go " ++ show x ++ "."
	show l = locoshow l

locoshow :: Locomotive -> String
locoshow l = "Loco " ++ show (slot l) ++ " going " ++ show (direction l) ++ " at " ++ show (speed l) ++ " (id " ++ show (ide l) ++ ")"