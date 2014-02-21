module TrainSafetyTypes where

import qualified Data.Map as Map

--type Layout = Map.Map String Section
type TrackInstruction = String
type SensorID = String
data Direction = FWD | BKW deriving (Eq)
instance Show Direction where
	show d = if d==FWD then "Forward" else "Backward"

rev :: Direction -> Direction
rev FWD = BKW
rev _ = FWD

data TurnoutState = Set | Unset | Noturn deriving (Show, Eq)
data State = Justleft | Justentered | Empty | Occupied deriving (Show, Eq)
data SensorUpdate = Hi | Low deriving (Show, Eq)
data MessageType = Speed | Direction | Sensor | Turnout deriving (Show, Eq)

data Layout = Layout { track :: Map.Map String Section
					 , locos :: Map.Map Int String
					 } deriving (Show)

buildLayout :: [(String,Section)] -> Layout
buildLayout a = Layout { track=(Map.fromList a), locos=(Map.fromList (buildLocos a)) }

buildLocos :: [(String,Section)] -> [(Int,String)]
buildLocos [] = []
buildLocos ((a,b):xs) | loco b /= Noloco = (slot (loco b),a) : buildLocos xs
					  | otherwise = buildLocos xs

data Message = SpeedMessage { fromslot :: Int
							, newspeed :: Int
							} |
			   DirectionMessage { dirslot :: Int
								, newdir :: Direction 
								} |
			   SensorMessage { upd :: SensorUpdate
							 , updid :: SensorID
							 } |
			   TurnoutMessage { turnid :: SensorID
			   				  , side :: Direction
			   				  , newstate :: TurnoutState
							  } deriving (Show,Eq)

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



getSection :: Layout -> SensorID -> Section
getSection t s = (track t) Map.! s

setSection :: Layout -> Section -> Layout
setSection t s = t { track=(Map.insert (sid s) s (track t)) }

getLocoBySlot :: Layout -> Int -> Locomotive
getLocoBySlot t i = loco (getSectionBySlot t i)

getLocoBySensorID :: Layout -> SensorID -> Locomotive
getLocoBySensorID t s = loco ((track t) Map.! s)

getLocoBySection :: Section -> Locomotive
getLocoBySection s = loco s

getSectionBySlot :: Layout -> Int -> Section
getSectionBySlot t i = (track t) Map.! ((locos t) Map.! i)

setSectionLoco :: Section -> Locomotive -> Section
setSectionLoco s l = s { loco=l }

updateLoco :: Layout -> Locomotive -> Layout
updateLoco t l = t { track=Map.insert (sid sec) (setSectionLoco sec l) (track t) }
	where
		sec = getSectionBySlot t i
		i = slot l

moveLoco :: Layout -> Section -> Section -> Layout
moveLoco t from to = clearSection (Layout { track=Map.insert (sid to) (setSectionLoco (to {state=Occupied}) l) (track t), locos=Map.insert (slot l) (sid to) (locos t)}) from
	where
		l = getLocoBySection from

clearSection :: Layout -> Section -> Layout
clearSection t s = setSection t (s {state=Empty, loco=Noloco})

searchLayout :: Layout -> (Section -> Bool) -> [Section]
searchLayout t f = Map.elems (Map.filter f (track t))

layoutSids :: Layout -> [SensorID]
layoutSids t = Map.keys (track t)

trackLength :: Layout -> Int
trackLength t = Map.size (track t)

-- | Find the next section in a given direction, noting turnout positions
findNextSection :: Layout -> Section -> Direction -> Section
findNextSection t s@(Section { nextturn=Noturn }) FWD = getSection t (head (next s))
findNextSection t s@(Section { prevturn=Noturn }) BKW = getSection t (head (prev s))
findNextSection t s@(Section { nextturn=Unset }) FWD = getSection t (head (next s))
findNextSection t s@(Section { prevturn=Unset }) BKW = getSection t (head (prev s))
findNextSection t s@(Section { nextturn=Set }) FWD = getSection t (head (tail (next s)))
findNextSection t s@(Section { prevturn=Set }) BKW = getSection t (head (tail (prev s)))