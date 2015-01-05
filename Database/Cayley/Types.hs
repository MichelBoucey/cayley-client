{-# LANGUAGE OverloadedStrings #-}

module Database.Cayley.Types where

import qualified Data.Aeson as A
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Network.HTTP.Client (Manager)

data CayleyConfig = CayleyConfig
    { serverPort :: Int    -- ^ Default to 64210
    , serverName :: String -- ^ Default to "localhost"
    , apiVersion :: String -- ^ Default to "1"
    , queryLang  :: String -- ^ Default to "gremlin"
    } deriving (Eq,Show)

-- | CayleyConfig { serverPort = 64210 , serverName = "localhost" , apiVersion = "1" , queryLang  = "gremlin" }
defaultCayleyConfig = CayleyConfig
    { serverPort = 64210
    , serverName = "localhost"
    , apiVersion = "1"
    , queryLang  = "gremlin"
    }

data CayleyConnection = CayleyConnection (CayleyConfig,Manager)
     
data Quad = Quad
    { subject   :: T.Text       -- ^ Subject node
    , predicate :: T.Text       -- ^ Predicate node
    , object    :: T.Text       -- ^ Object node
    , label     :: Maybe T.Text -- ^ Label node
    } deriving (Eq,Show)

instance A.ToJSON Quad where
    toJSON (Quad subject predicate object label) =
        A.object [ "subject"   A..= subject
                 , "predicate" A..= predicate
                 , "object"    A..= object
                 , "label"     A..= label
                 ]

instance A.FromJSON Quad where
    parseJSON (A.Object v) = Quad <$>
                             v A..: "subject" <*>
                             v A..: "predicate" <*>
                             v A..: "object" <*>
                             v A..: "label"
    parseJSON _            = mzero
