import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.IMAP

import Data.Traversable
import Data.Foldable
import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception

import System.IO

main :: IO ()
main = do
   let server = "imap.exascale-computing.eu"
   putStrLn ("Connecting to " ++ server)
   conn <- connectIMAPSSL server
   putStr "Username: "
   hFlush stdout
   username <- getLine
   putStr "Password: "
   hFlush stdout
   password <- withEcho False getLine
   putStrLn ""
   login conn username password
   ls <- list conn
   traverse_ putStrLn (fmap snd ls)
   
   select conn "INBOX"

   uids <- search conn [ALLs]
   
   hdr <- fetch conn (head uids)
   putStrLn (show hdr)

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
