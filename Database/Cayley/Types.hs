{-# LANGUAGE OverloadedStrings #-}

module Database.Cayley.Types where

import           Control.Monad
import qualified Data.Aeson          as A
import qualified Data.Text           as T
import           Network.HTTP.Client (Manager)

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
defaultCayleyConfig :: CayleyConfig
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
    toJSON (Quad s p o l) =
        A.object [ "subject"   A..= s
                 , "predicate" A..= p
                 , "object"    A..= o
                 , "label"     A..= l
                 ]

instance A.FromJSON Quad where
    parseJSON (A.Object v) = Quad <$>
                             v A..: "subject" <*>
                             v A..: "predicate" <*>
                             v A..: "object" <*>
                             v A..: "label"
    parseJSON _            = mzero

data Node = Node
    { id           :: Integer
    , tags         :: [Tag] -- ^ list of tags from the query
    , values       :: [Value] -- ^ Known values from the query
    , is_link_node :: Bool     -- ^ Does the node represent the link or the node (the oval shapes)
    , is_fixed     :: Bool     -- ^ Is the node a fixed starting point of the query
    } deriving Show

instance A.FromJSON Node where
    parseJSON (A.Object v) = Node <$>
                           v A..: "id"<*>
                           v A..: "tags" <*>
                           v A..: "values" <*>
                           v A..: "is_link_node" <*>
                           v A..: "is_fixed"
    parseJSON _            = mzero

data Link = Link
    { source    :: Integer -- ^ Node ID
    , target    :: Integer -- ^ Node ID
    , link_node :: Integer -- ^ Node ID
    } deriving Show

instance A.FromJSON Link where
    parseJSON (A.Object v) = Link <$>
                           v A..: "source" <*>
                           v A..: "target" <*>
                           v A..: "link_node"
    parseJSON _            = mzero

type Query = T.Text

type Subject = T.Text

type Predicate = T.Text

type Object = T.Text

type Label = T.Text

type Tag = T.Text

type Value = T.Text

