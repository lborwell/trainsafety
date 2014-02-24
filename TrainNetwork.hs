import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import System.IO
import TrainSafetyTypes
import Testtracks
import qualified TrainSafe as TS
import qualified TrainStateUpdate as TSUpd
import qualified TrainRoute as TR


main :: IO ()
main = do
  send <- opensock "localhost" "55555" WriteMode
  rec <- opensock "localhost" "55555" ReadMode
  putStrLn "runnin"
  doit trackDict rec send

doit :: Layout -> Handle -> Handle -> IO ()
doit t rec send = do
  msg <- hGetLine rec
  putStrLn $ "Inc: " ++ msg
  let (mess,track) = process t msg
  sendmessages mess send
  putStrLn $ (show track) ++ "\n"
  doit track rec send

process :: Layout -> String -> ([TrackInstruction], Layout)
process t s = (routeout++safeout, safetrack)
  where
    (m,newtrack) = TSUpd.process t s
    (routeout,routetrack) = TR.process newtrack m
    (safeout,safetrack) = TS.process routetrack m



sendmessages :: [String] -> Handle -> IO ()
sendmessages m h = do
  mapM_ (sendmsg h) m

sendmsg :: Handle -> String -> IO ()
sendmsg h s = do
  hPutStrLn h s
  hFlush h

opensock :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> IOMode
        -> IO Handle      -- ^ Handle to use for logging
opensock hostname port mode =
    withSocketsDo $ do  -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Mark the socket for keep-alive handling since it may be idle
       -- for long periods of time
       setSocketOption sock KeepAlive 1

       -- Connect to server
       connect sock (addrAddress serveraddr)

       -- Make a Handle out of it for convenience
       h <- socketToHandle sock mode

       -- We're going to set buffering to BlockBuffering and then
       -- explicitly call hFlush after each message, below, so that
       -- messages get logged immediately
       hSetBuffering h LineBuffering
       
       -- Save off the socket, program name, and server address in a handle
       return h

