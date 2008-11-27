module Net where

import Network
import IO

server :: Int -> (String -> String) -> IO()
server n f =  withSocketsDo $ 
    do s <- listenOn (PortNumber n')
       runServer s f
 where n' = fromInteger $ toInteger n

runServer :: Socket -> (String -> String) -> IO ()
runServer s f = do 
  (h,_,_) <- accept s
  hSetBuffering h LineBuffering
  hGetLine h >>= \str -> hPutStrLn h (f str)
  runServer s f

