module Melvin.Options (
  Options(..),
  options
) where

import Data.Ix
import Data.Word
import Network
import Options.Applicative
import Prelude

data Options = Options
    { optionPort       :: PortID
    , optionMaxClients :: Maybe Integer
    }

opts :: Parser Options
opts = Options
    <$> nullOption
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "Port to listen on"
       <> value (PortNumber 6667)
       <> reader portNumReader )
    <*> optional
        (option
            ( long "max-clients"
           <> short 'm'
           <> metavar "COUNT"
           <> help "Maximum number of clients to handle (default: unlimited)"
           <> reader maxClientsReader ))

maxClientsReader :: String -> Either ParseError Integer
maxClientsReader s =
    case reads s of
        [] -> fail $ s ++ " isn't a number"
        ((c,_):_) -> if c < 1
                         then fail "Max clients can't be less than 1."
                         else Right c

portNumReader :: String -> Either ParseError PortID
portNumReader s =
    case reads s of
        [] -> fail "Parsing port number failed"
        ((p,_):_) -> if inRange bounds p
                         then Right $ PortNumber (fromIntegral p)
                         else fail "Port number invalid."
    where bounds :: (Integer, Integer)
          bounds = (fromIntegral (minBound :: Word16), fromIntegral (maxBound :: Word16))

options :: IO Options
options = execParser $ info (helper <*> opts)
    (fullDesc <> header "melvin -- a dAmn <-> IRC server")
