module TrainRoute where

import TrainSafetyTypes
import TrainControl
import Testtracks

import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import Data.Set as S
import qualified Data.Map as Map

pathBetween :: Layout -> DijkStruct -> SensorID -> SensorID -> [(SensorID,[TrackInstruction])]
pathBetween t d from to = pathToInstrs t (doAPath d from to) (getLocoBySensorID t from)



 --http://rosettacode.org/wiki/Dijkstra%27s_algorithm#Haskell
dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v,w)] -> (Array v w, Array v v)
dijkstra src invalid_index adj_list = runST $ do
  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  previous <- newSTArray b invalid_index
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance v
                  if dist_thru_u >= old_dist then
                    return vertex_queue
                  else do
                    let vertex_queue' = S.delete (old_dist, v) vertex_queue
                    writeArray min_distance v dist_thru_u
                    writeArray previous v u
                    return $ S.insert (dist_thru_u, v) vertex_queue'
            in
            foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  m <- freeze min_distance
  p <- freeze previous
  return (m, p)
  where b = bounds adj_list
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray
 
shortest_path_to :: (Ix v) => v -> v -> Array v v -> [v]
shortest_path_to target invalid_index previous =
  aux target [] where
    aux vertex acc | vertex == invalid_index = acc
                   | otherwise = aux (previous ! vertex) (vertex : acc)
 
adj_list :: Array Char [(Char, Int)]
adj_list = listArray ('a', 'f') [ [('b',7), ('c',9), ('f',14)],
                                  [('a',7), ('c',10), ('d',15)],
                                  [('a',9), ('b',10), ('d',11), ('f',2)],
                                  [('b',15), ('c',11), ('e',6)],
                                  [('d',6), ('f',9)],
                                  [('a',14), ('c',2), ('e',9)] ]

layoutToArrayMaps :: Layout -> (Map.Map Int String, Map.Map String Int)
layoutToArrayMaps t = (Map.fromList (layoutNumSids t), Map.fromList (layoutSidNums t))

layoutNumSids :: Layout -> [(Int, String)]
layoutNumSids t = zip [0..n] sids
	where
		sids = layoutSids t
		n = trackLength t

layoutSidNums :: Layout -> [(String,Int)]
layoutSidNums t = Prelude.map (\(a,b) -> (b,a)) (layoutNumSids t)

layoutSidNumsMap :: Layout -> Map.Map String Int
layoutSidNumsMap t = Map.fromList (layoutSidNums t)

layoutNumSidsMap :: Layout -> Map.Map Int String
layoutNumSidsMap t = Map.fromList (layoutNumSids t)

layoutAdjList :: Layout -> Array Int [(Int, Int)]
layoutAdjList t = listArray (0, (trackLength t)-1) (buildArr (layoutNumSids t))
	where
		buildArr [] = []
		buildArr ((_,b):xs) = [[(x,1) | x <- Prelude.map ((layoutSidNumsMap t) Map.!) ((next (sec b)) ++ (prev (sec b))) ]] ++ buildArr xs
		sec y = getSection t y

numsToSids :: Map.Map Int String -> [Int] -> [String]
numsToSids m = Prelude.map (m Map.!)

data DijkStruct = DijkStruct { numToSid :: Map.Map Int String
							 , sidToNum :: Map.Map String Int
							 , adjList :: Array Int [(Int,Int)] }
data RouteInstrs = RouteInstrs { locom :: Locomotive
							   , instrs :: [(SensorID,[TrackInstruction])]}

makeStruct :: Layout -> DijkStruct
makeStruct t = DijkStruct { numToSid=(layoutNumSidsMap t), sidToNum=(layoutSidNumsMap t), adjList=(layoutAdjList t)}

doAPath :: DijkStruct -> SensorID -> SensorID -> [SensorID]
doAPath d from to = Prelude.map ((numToSid d) Map.!) route
	where
		getNum a = (sidToNum d) Map.! a
		route = shortest_path_to (getNum to) (-1) (snd (dijkstra (getNum from) (-1) (adjList d)))

-- | Check first join
pathToInstrs :: Layout -> [SensorID] -> Locomotive -> [(SensorID,[TrackInstruction])]
pathToInstrs _ [] _ = []
pathToInstrs _ (a:[]) l = [(a,[stopLoco l])]
pathToInstrs t x@(a:b:_) l = (a, processJoinSwitch t (seca,secb) d ++ setLocoDirection l d) : pathToInstrs' t x l
	where
		seca = getSection t a
		secb = getSection t b
		d = directionBetween seca secb

-- | Check the rest
pathToInstrs' :: Layout -> [SensorID] -> Locomotive -> [(SensorID,[TrackInstruction])]
pathToInstrs' _ [] _ = []
pathToInstrs' _ (x:[]) l = [(x,[stopLoco l])]
pathToInstrs' _ (_:b:[]) l = [(b,[stopLoco l])]
pathToInstrs' t (a:b:xs) l = (b, processJoin t (a,b,(head xs)) l) : pathToInstrs' t (b:xs) l

processJoin :: Layout -> (SensorID, SensorID, SensorID) -> Locomotive -> [TrackInstruction]
processJoin t (from, curr, to) l = processJoinSwitch t (b,c) (directionBetween b c) ++ processJoinDirection (a,b,c) l
	where
		a = getSection t from
		b = getSection t curr
		c = getSection t to

processJoinDirection :: (Section,Section,Section) -> Locomotive -> [TrackInstruction]
processJoinDirection (a,b,c) l | d /= d' = setLocoDirection l d'
								 | otherwise = []
	where
		d = directionBetween a b
		d' = directionBetween b c

processJoinSwitch :: Layout -> (Section,Section) -> Direction -> [TrackInstruction]
processJoinSwitch t (a,b) FWD | nextturn a == Noturn = []
                              | otherwise = setDivergingSwitch t a (sid b) FWD
processJoinSwitch t (a,b) BKW | prevturn a == Noturn = []
							  | otherwise = setDivergingSwitch t a (sid b) BKW

directionBetween :: Section -> Section -> Direction
directionBetween from to | (sid to) `elem` (next from) = FWD
						 | otherwise = BKW



process :: Layout -> Message -> ([TrackInstruction],Layout)
process t m@(SensorMessage {}) = checkLoco t (locoFromSensorMessage t m)
process t m@(DirectionMessage {}) = ([],t)
process t m@(SpeedMessage {}) = ([],t)
process t m@(TurnoutMessage {}) = ([],t)

checkLoco :: Layout -> Section -> ([TrackInstruction],Layout)
checkLoco t s | p == [] = ([],t)
			  | sid s == fst (head p) = (snd (head p), setSection t (setSectionLoco s (l { path=tail p })))
			  | otherwise = ([],t)
	where 
		l = loco s
		p = path l


main :: IO ()
main = do
    let t = trackDict
    let st = makeStruct t
    runrun st t

    --let (min_distance, previous) = dijkstra 'a' ' ' adj_list
    --putStrLn $ "Distance from a to e: " ++ show (min_distance ! 'e')
    --let path = shortest_path_to 'e' ' ' previous
    --putStrLn $ "Path: " ++ show path

runrun :: DijkStruct -> Layout -> IO ()
runrun s t = do
    putStrLn "Enter path (loco id from to):"
    inp <- getLine
    let (from:to:_) = words inp
    putStrLn (show (pathBetween t s from to))
    --putStrLn (show (pathToInstrs t (doAPath s from to) (getLocoBySlot t (read slot))))
    runrun s t