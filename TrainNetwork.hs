import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes
import System.IO
import TrainSafetyTypes
import Testtracks
import qualified TrainSafe as TS

data SyslogHandle = 
    SyslogHandle {slHandle :: Handle,
                  slProgram :: String}

main :: IO ()
main = do
  rec <- opensock "localhost" "55555"
  send <- opensock "localhost" "55556"
  putStrLn "runnin"
  doit trackDict rec send

doit :: Layout -> Handle -> Handle -> IO ()
doit t rec send = do
  msg <- hGetLine rec
  let (out,newtrack) = TS.process t msg
  sendmessages out send
  putStrLn "dummy"

sendmessages :: [String] -> Handle -> IO ()
sendmessages m h = do
  mapM_ (hPutStrLn h) m

opensock :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> IO Handle      -- ^ Handle to use for logging
opensock hostname port =
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
       h <- socketToHandle sock WriteMode

       -- We're going to set buffering to BlockBuffering and then
       -- explicitly call hFlush after each message, below, so that
       -- messages get logged immediately
       hSetBuffering h (BlockBuffering Nothing)
       
       -- Save off the socket, program name, and server address in a handle
       return h

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
    do hPutStrLn (slHandle syslogh) sendmsg
       -- Make sure that we send data immediately
       hFlush (slHandle syslogh)
    where code = makeCode fac pri
          sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++
                    ": " ++ msg

closelog :: SyslogHandle -> IO ()
closelog syslogh = hClose (slHandle syslogh)

{- | Convert a facility and a priority into a syslog code -}
makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = codeOfFac fac
        pricode = fromEnum pri 
        in
          (faccode `shiftL` 3) .|. pricode