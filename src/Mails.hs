import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types

import Data.Traversable
import Data.Foldable
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception

import System.IO

import Text.ParserCombinators.Parsec.Rfc2822 as Mail
import Text.ParserCombinators.Parsec

import qualified Data.ByteString.Char8 as BS


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

   traverse_ (showHeader conn) uids
   
   logout conn

showHeader :: IMAPConnection -> UID -> IO ()
showHeader conn uid = do
   content <- fetch conn uid

   let 
      (Right (Message fields body)) = parse Mail.message "" (BS.unpack content)
      subject = head . catMaybes $ fmap extractSubject fields
      extractSubject (Subject x) = Just x
      extractSubject _ = Nothing

   putStrLn ("Subject: "++ subject)
   --putStrLn ("Body: "++ body)


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
