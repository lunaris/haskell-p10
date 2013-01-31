module HP10 where

import Control.Monad
import Data.Functor
import Data.Time
import Data.Time.Clock.POSIX
import Network
import System.Environment
import System.IO

main :: IO ()
main
  = withSocketsDo $ do
      now <- round <$> getPOSIXTime
      h <- connectTo "localhost" (PortNumber 4400)
      [pass, host] <- getArgs

      hSetBuffering h NoBuffering
      hPutStrLn h $
        "PASS :" ++ pass

      hPutStrLn h $
        "SERVER " ++ host ++ " 1 " ++ show now ++ " " ++
          show now ++ " J10 AZ]]] :Local services"

      forever $ do
        l <- hGetLine h
        putStrLn l

      hClose h
