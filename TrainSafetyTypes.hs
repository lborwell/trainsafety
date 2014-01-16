module TrainSafetyTypes where

import qualified Data.Map as Map

type Layout = Map.Map String Section
type SensorID = String
data Direction = FWD | BKW deriving (Show, Eq)
data TurnoutState = Set | Unset | Noturn deriving (Show, Eq)
data State = Justleft | Justentered | Empty | Occupied deriving (Show, Eq)
data SensorUpdate = Hi | Low deriving (Show, Eq)

data SpeedMessage = SpeedMessage { fromslot :: Int
								 , newspeed :: Int
								 } deriving (Show)

data DirectionMessage = DirectionMessage { dirslot :: Int
										 , newdir :: Direction 
										 } deriving (Show, Eq)

data Section = Section { state :: State
					   , prev :: [SensorID]
					   , next :: [SensorID]
					   , speedlim :: Int
					   , loco :: Locomotive
					   , sid :: SensorID
					   , prevturn :: TurnoutState
					   , nextturn :: TurnoutState
					   } deriving (Show)

instance Eq Section where
	Section { sid=x } == Section { sid=y } = if x == y then True else False

data Locomotive = Locomotive { slot :: Int
							 , speed :: Int
							 , ide :: Int
							 , direction :: Direction
							 } | Noloco deriving (Show, Eq)