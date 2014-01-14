import qualified Data.Map as Map
--import Debug.Trace

type Layout = Map.Map String Section
type SensorID = String
data Direction = FWD | BKW deriving (Show)
data State = Justleft | Justentered | Empty | Occupied deriving (Show, Eq)
data SensorUpdate = Hi | Low deriving (Show, Eq)

data Section = Section { state :: State
					   , prev :: [SensorID]
					   , next :: [SensorID]
					   , speedlim :: Int
					   , loco :: Locomotive
					   , sid :: SensorID
					   } deriving (Show)

data Locomotive = Locomotive { slot :: Int
							 , speed :: Int
							 , ide :: Int
							 , direction :: Direction
							 } deriving (Show)

noloco :: Locomotive
noloco = Locomotive { slot=0, speed=0, ide=0, direction=FWD }

testLoco :: Locomotive
testLoco = Locomotive { slot=999, speed=113, ide=2, direction=FWD }

a1 :: Section
a1 = Section { state=Occupied, prev=["D1"], next=["B1"], speedlim=113, loco=testLoco, sid="A1" }

a2 :: Section
a2 = Section { state=Empty, prev=["D1","D2"], next=["B2"], speedlim=113, loco=noloco, sid="A2" }

b1 :: Section
b1 = Section { state=Empty, prev=["A1"], next=["C1"], speedlim=113, loco=noloco, sid="B1" }

b2 :: Section
b2 = Section { state=Empty, prev=["A2"], next=["B2"], speedlim=113, loco=noloco, sid="B2" }

c1 :: Section
c1 = Section { state=Empty, prev=["B1","B2"], next=["D1"], speedlim=113, loco=noloco, sid="C1" }

c2 :: Section
c2 = Section { state=Empty, prev=["B2"], next=["D2"], speedlim=113, loco=noloco, sid="C2" }

d1 :: Section
d1 = Section { state=Empty, prev=["C1"], next=["A1","A2"], speedlim=113, loco=noloco, sid="D1" }

d2 :: Section
d2 = Section { state=Empty, prev=["C2"], next=["A2"], speedlim=113, loco=noloco, sid="D2" }

track :: [(String, Section)]
track = [("A1",a1),("A2",a2),("B1",b1),("B2",b2),("C1",c1),("C2",c2),("D1",d1),("D2",d2)]

trackDict :: Layout 
trackDict = Map.fromList track


-- On sensor trigger:
-- If sensor goes high, check prev/next sections for Justleft
-- If justleft, take its locomotive, set this to occupied, neighbour to empty
-- If none left, set this to justentered
-- Opposite for sensor goes low

respondToSensor :: Layout -> SensorUpdate -> SensorID -> Layout
respondToSensor track change sensor = checkNeighbours track (track Map.! sensor) change
--respondToSensor track change sensor = Map.insert sensor (checkNeighbours (track Map.! sensor) change) track

checkNeighbours :: Layout -> Section -> SensorUpdate -> Layout
checkNeighbours t s u | u == Hi = checkNextEntered t s
					  | u == Low = checkNextExited t s

checkNextEntered :: Layout -> Section -> Layout
checkNextEntered t s@(Section { next=[] })  = checkPrevEntered t (t Map.! (sid s))
checkNextEntered t s@(Section { next=(n:ns) }) | state (t Map.! n) == Justleft = moveLoco t (t Map.! n) (t Map.! (sid s))
											   | otherwise = checkNextEntered t (s { next=ns })
																			

checkPrevEntered :: Layout -> Section -> Layout
checkPrevEntered t s@(Section { prev=[] }) = Map.insert (sid s) ((t Map.! (sid s)) { state=Justentered }) t
checkPrevEntered t s@(Section { prev=(n:ns) }) | state (t Map.! n) == Justleft = moveLoco t (t Map.! n) (t Map.! (sid s))
											   | otherwise = checkPrevEntered t (s { prev=ns })

checkNextExited :: Layout -> Section -> Layout
checkNextExited t s@(Section { next=[] })  = checkPrevExited t s
checkNextExited t s@(Section { next=(n:ns) }) | state (t Map.! n) == Justentered = moveLoco t (t Map.! (sid s)) (t Map.! n)
											  | otherwise = checkNextExited t (s { next=ns })
																			

checkPrevExited :: Layout -> Section -> Layout
checkPrevExited t s@(Section { prev=[] }) = Map.insert (sid s) ((t Map.! (sid s)) { state=Justleft }) t
checkPrevExited t s@(Section { prev=(n:ns) }) | state (t Map.! n) == Justentered = moveLoco t (t Map.! (sid s)) (t Map.! n)
											  | otherwise = checkPrevExited t (s { prev=ns })

moveLoco :: Layout -> Section -> Section -> Layout
moveLoco t from to = clearSection (Map.insert (sid to) (to {state=Occupied,loco=(loco from)}) t) from

clearSection :: Layout -> Section -> Layout
clearSection t s = Map.insert (sid s) (s {state=Empty, loco=noloco}) t