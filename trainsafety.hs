import qualified Data.Map as Map

type SensorID = String
data Direction = FWD | BKW deriving (Show)
data State = Justleft | Justentered | Empty | Occupied deriving (Show)
data SensorUpdate = Hi | Low deriving (Show)

data Section = Section { state :: State
					   , prev :: [SensorID]
					   , next :: [SensorID]
					   , speedlim :: Int
					   , loco :: Locomotive
					   } deriving (Show)

data Locomotive = Locomotive { slot :: Int
							 , speed :: Int
							 , ide :: Int
							 , direction :: Direction
							 } deriving (Show)

noloco :: Locomotive
noloco = Locomotive { slot=0, speed=0, ide=0, direction=FWD }

a1 :: Section
a1 = Section { state=Empty, prev=["D1"], next=["B1"], speedlim=113, loco=noloco }

a2 :: Section
a2 = Section { state=Empty, prev=["D1","D2"], next=["B2"], speedlim=113, loco=noloco }

b1 :: Section
b1 = Section { state=Empty, prev=["A1"], next=["C1"], speedlim=113, loco=noloco }

b2 :: Section
b2 = Section { state=Empty, prev=["A2"], next=["B2"], speedlim=113, loco=noloco }

c1 :: Section
c1 = Section { state=Empty, prev=["B1","B2"], next=["D1"], speedlim=113, loco=noloco }

c2 :: Section
c2 = Section { state=Empty, prev=["B2"], next=["D2"], speedlim=113, loco=noloco }

d1 :: Section
d1 = Section { state=Empty, prev=["C1"], next=["A1","A2"], speedlim=113, loco=noloco }

d2 :: Section
d2 = Section { state=Empty, prev=["C2"], next=["A2"], speedlim=113, loco=noloco }

track :: [(String, Section)]
track = [("A1",a1),("A2",a2),("B1",b1),("B2",b2),("C1",c1),("C2",c2),("D1",d1),("D2",d2)]

trackDict :: Map.Map String Section 
trackDict = Map.fromList track


-- On sensor trigger:
-- If sensor goes high, check prev/next sections for Justleft
-- If justleft, take its locomotive, set this to occupied, neighbour to empty
-- Opposite for sensor goes low

respondToSensor :: Map.Map String Section -> SensorUpdate -> SensorID -> Map.Map String Section
respondToSensor track change sensor = Map.insert sensor (checkNeighbours (track Map.! sensor) change) track

checkNeighbours :: Map.Map String Section -> Section -> SensorUpdate -> Section
checkNeighbours t s u = checkNext t s u

checkNext :: Map.Map String Section -> Section -> SensorUpdate -> Section
checkNext t s@(Section { state=st, prev=p, next=[], speedlim=sl, loco=l }) upd = checkPrev s upd
checkNext t s@(Section { state=st, prev=p, next=(n:ns), speedlim=sl, loco=l }) upd | state (t Map.! n) == Justleft = 
																			

checkNext :: Map.Map String Section -> Section -> SensorUpdate -> Section
checkPrev s@(Section { state=st, prev=p, next=n, speedlim=sl, loco=l }) upd | length p == 0 = s