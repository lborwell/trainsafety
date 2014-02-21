module TrainStateUpdate where

import TrainSafetyTypes

process :: Layout -> String -> (Message,Layout)
process t s = (msg, l)
	where
		msg = parseInput (words s)
		l = updateLayout t msg

parseInput :: [String] -> Message
parseInput a@("speed":_) = parseSpeed a
parseInput a@("dir":_) = parseDirection a
parseInput a@("sensor":_) = parseSensor a
parseInput a@("turn":_) = parseTurn a

updateLayout :: Layout -> Message -> Layout
updateLayout t m@(SpeedMessage {}) = speedChange t m
updateLayout t m@(DirectionMessage {}) = changeDirection t m
updateLayout t m@(SensorMessage {}) = sectionSensorTrigger t m
updateLayout t m@(TurnoutMessage {}) = setTurnout t m

-------------------------------------------------
--
--       Incoming Speed Message
--
-------------------------------------------------

-- | Parse incoming speed message into SpeedMessage
parseSpeed :: [String] -> Message
parseSpeed (_:a:b:_) = SpeedMessage { fromslot=(read a), newspeed=(read b) }

-- | Update layout to represent new loco speed
speedChange :: Layout -> Message -> Layout
speedChange t m = setSection t (sec { loco=((loco sec) { speed=(newspeed m) }) })
	where sec = getSectionBySlot t (fromslot m)


-------------------------------------------------
--
--       Incoming Direction Change Message
--
-------------------------------------------------

-- | Parse incoming direction message into DirectionMessage
parseDirection :: [String] -> Message
parseDirection (_:a:b:_) = DirectionMessage { dirslot=(read a), newdir=(parsedir b) }
	where parsedir x = if x=="fwd" then FWD else BKW

-- | Update layout to represent new loco direction
changeDirection :: Layout -> Message -> Layout
changeDirection t m = setSection t (sec { loco=((loco sec) { direction=(newdir m) })})
	where sec = getSectionBySlot t (dirslot m)


-------------------------------------------------
--
--       Incoming Turnout Change Message
--
-------------------------------------------------

-- | Parse incoming turnout message into TurnoutMessage
parseTurn :: [String] -> Message
parseTurn (_:a:b:c:_) = TurnoutMessage { turnid=a, side=(end b), newstate=(set c)  }
	where
		end x = if x=="fwd" then FWD else BKW
		set x = if x=="set" then Set else Unset

-- | Update layout to represent new turnout setting
setTurnout :: Layout -> Message -> Layout
setTurnout t m | side m == FWD = setSection t (sec { nextturn=(newstate m) })
			   | side m == BKW = setSection t (sec { prevturn=(newstate m) })
	where sec = getSection t (turnid m)

-------------------------------------------------
--
--       Incoming Section Boundary Message
--
-------------------------------------------------
-- On sensor trigger:
-- If sensor goes high, check prev/next sections for Justleft
-- If justleft, take its locomotive, set this to occupied, neighbour to empty
-- If none left, set this to justentered
-- Opposite for sensor goes low

parseSensor :: [String] -> Message
parseSensor inp = SensorMessage { upd=up a, updid=b }
	where
		(_:a:b:_) = inp
		up x = if x == "Hi" then Hi else Low

sectionSensorTrigger :: Layout -> Message -> Layout
sectionSensorTrigger track msg = checkNeighbours track (getSection track (updid msg)) (upd msg)

checkNeighbours :: Layout -> Section -> SensorUpdate -> Layout
checkNeighbours t s Hi | state nxt == Justleft && correctDirection s Hi NXT = moveLoco t nxt s
					   | state prv == Justleft && correctDirection s Hi PRV = moveLoco t prv s
					   | otherwise = setSection t ((getSection t (sid s)) { state=Justentered })
	where
		nxt = findNextSection t s FWD
		prv = findNextSection t s BKW
checkNeighbours t s Low | state nxt == Justentered && correctDirection s Low NXT = moveLoco t s nxt
						| state prv == Justentered && correctDirection s Low PRV = moveLoco t s prv
						| otherwise = setSection t ((getSection t (sid s)) { state=Justleft })
	where
		nxt = findNextSection t s FWD
		prv = findNextSection t s BKW

data D = NXT | PRV

correctDirection :: Section -> SensorUpdate -> D -> Bool
correctDirection ns _ _ | loco ns == Noloco = True
correctDirection ns Hi NXT | direction (loco ns) == BKW = True
						   | otherwise = False
correctDirection ns Hi PRV | direction (loco ns) == FWD = True
						   | otherwise = False
correctDirection ns Low NXT | direction (loco ns) == FWD = True
							| otherwise = False
correctDirection ns Low PRV | direction (loco ns) == BKW = True
							| otherwise = False