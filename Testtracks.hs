module Testtracks where

import TrainSafetyTypes
import qualified Data.Map as Map

test1 :: Section
test1 = Section { state=Occupied, prev=["T2"], next=[], speedlim=113, loco=testLoco, sid="T1" }

test2 :: Section
test2 = Section { state=Empty, prev=[], next=["T1"], speedlim=113, loco=noloco, sid="T2" }

test3 :: Section
test3 = Section { state=Occupied, prev=["T4"], next=[], speedlim=113, loco=testLoco, sid="T3" }

test4 :: Section
test4 = Section { state=Empty, prev=[], next=["T3"], speedlim=113, loco=noloco, sid="T4" }

testTrack :: [(String,Section)]
testTrack = [("T1",test1),("T2",test2),("T3",test3),("T4",test4)]

testDict :: Layout
testDict = Map.fromList testTrack


noloco :: Locomotive
noloco = Locomotive { slot=0, speed=0, ide=0, direction=FWD }

testLoco :: Locomotive
testLoco = Locomotive { slot=8, speed=115, ide=2, direction=FWD }

testLoco2 :: Locomotive
testLoco2 = Locomotive { slot=9, speed=115, ide=2, direction=FWD }

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