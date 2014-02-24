module Testtracks where

import TrainSafetyTypes

testLoco :: Locomotive
testLoco = Locomotive { slot=9, speed=0, ide=2, direction=FWD, waiting=False, prevspeed=0, path=[("C1",["fuck yes","you prick"])] }

testLoco2 :: Locomotive
testLoco2 = Locomotive { slot=8, speed=0, ide=2, direction=FWD, waiting=False, prevspeed=0, path=[] }

testLoco3 :: Locomotive
testLoco3 = Locomotive { slot=10, speed=114, ide=2, direction=FWD, waiting=False, prevspeed=0, path=[] }

a1 :: Section
a1 = Section { state=Empty, prev=["D1"], next=["B1"], speedlim=113, loco=Noloco, sid="A1", prevturn=Noturn, nextturn=Noturn }

a2 :: Section
a2 = Section { state=Empty, prev=["D2","D1"], next=["B2"], speedlim=113, loco=Noloco, sid="A2", prevturn=Unset, nextturn=Noturn }

b1 :: Section
b1 = Section { state=Occupied, prev=["A1"], next=["C1"], speedlim=113, loco=testLoco, sid="B1", prevturn=Noturn, nextturn=Noturn }

b2 :: Section
b2 = Section { state=Occupied, prev=["A2"], next=["C2","C1"], speedlim=113, loco=testLoco2, sid="B2", prevturn=Noturn, nextturn=Unset }

c1 :: Section
c1 = Section { state=Empty, prev=["B1","B2"], next=["D1"], speedlim=113, loco=Noloco, sid="C1", prevturn=Unset, nextturn=Noturn }

c2 :: Section
c2 = Section { state=Empty, prev=["B2"], next=["D2"], speedlim=113, loco=Noloco, sid="C2", prevturn=Noturn, nextturn=Noturn }

d1 :: Section
d1 = Section { state=Empty, prev=["C1"], next=["A1","A2"], speedlim=113, loco=Noloco, sid="D1", prevturn=Noturn, nextturn=Unset }

d2 :: Section
d2 = Section { state=Empty, prev=["C2"], next=["A2"], speedlim=113, loco=Noloco, sid="D2", prevturn=Noturn, nextturn=Noturn }

actualtrack :: [(String, Section)]
actualtrack = [("A1",a1),("A2",a2),("B1",b1),("B2",b2),("C1",c1),("C2",c2),("D1",d1),("D2",d2)]

trackDict :: Layout 
trackDict = buildLayout actualtrack