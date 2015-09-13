{-# LANGUAGE OverloadedStrings #-}

module Database.Cayley.Types where

import qualified Data.Aeson as A
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Network.HTTP.Client (Manager)

data APIVersion = V1

instance Show APIVersion where
    show V1 = "1"

data QueryLang = Gremlin | MQL

instance Show QueryLang where
    show Gremlin = "gremlin"
    show MQL     = "mql"

data CayleyConfig = CayleyConfig
    { serverPort :: Int
    , serverName :: String
    , apiVersion :: APIVersion
    , queryLang  :: QueryLang
    } deriving (Show)

-- | CayleyConfig { serverPort = 64210 , serverName = "localhost" , apiVersion = V1 , queryLang  = Gremlin }
defaultCayleyConfig = CayleyConfig
    { serverPort = 64210
    , serverName = "localhost"
    , apiVersion = V1
    , queryLang  = Gremlin
    }

data CayleyConnection = CayleyConnection (CayleyConfig,Manager)
     
data Quad = Quad
    { subject   :: T.Text       -- ^ Subject node
    , predicate :: T.Text       -- ^ Predicate node
    , object    :: T.Text       -- ^ Object node
    , label     :: Maybe T.Text -- ^ Label node
    }

instance Show Quad where
    show (Quad s p o (Just l)) = T.unpack s
                                 ++ " -- "
                                 ++ T.unpack p
                                 ++ " -> "
                                 ++ T.unpack o
                                 ++ " ("
                                 ++ T.unpack l
                                 ++ ")"
    show (Quad s p o Nothing)  = T.unpack s
                                 ++ " -- "
                                 ++ T.unpack p
                                 ++ " -> "
                                 ++ T.unpack o

-- | Two quads are equals when subject, predicate, object /and/ label are equals.
instance Eq Quad where
    Quad s p o l == Quad s' p' o' l' = s == s' && p == p' && o == o' && l == l'

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
