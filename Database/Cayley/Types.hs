{-# LANGUAGE DeriveGeneric #-}

module Database.Cayley.Types where

import           Control.Monad       (mzero)
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as AT
import           Data.Binary
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (Manager)

data APIVersion = V1

instance Show APIVersion where
    show V1 = "1"

data QueryLang = Gremlin | MQL

instance Show QueryLang where
  show Gremlin = "gremlin"
  show MQL     = "mql"

data CayleyConfig = CayleyConfig
  { serverPort :: !Int
  , serverName :: !String
  , apiVersion :: !APIVersion
  , queryLang  :: !QueryLang
  } deriving (Show)

-- | CayleyConfig { serverPort = 64210 , serverName = "localhost" , apiVersion = V1 , queryLang  = Gremlin }
defaultCayleyConfig :: CayleyConfig
defaultCayleyConfig = CayleyConfig
  { serverPort = 64210
  , serverName = "localhost"
  , apiVersion = V1
  , queryLang  = Gremlin
  }

data CayleyConnection = CayleyConnection
  { cayleyConfig :: !CayleyConfig
  , manager      :: !Manager
  }

data Quad = Quad
  { subject   :: !T.Text         -- ^ Subject node
  , predicate :: !T.Text         -- ^ Predicate node
  , object    :: !T.Text         -- ^ Object node
  , label     :: !(Maybe T.Text) -- ^ Label node
  } deriving (Generic)

instance Binary Quad

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
  parseJSON (A.Object v) =
    Quad <$>
    v A..: "subject" <*>
    v A..: "predicate" <*>
    v A..: "object" <*>
    v A..:? "label"
  parseJSON _            = mzero

data Shape = Shape
    { nodes :: ![Node]
    , links :: ![Link]
    } deriving (Eq, Show)

instance A.FromJSON Shape where
  parseJSON (A.Object v) = do
    vnds <- v A..: "nodes"
    nds  <- mapM parseNode $ V.toList vnds
    vlks <- v A..: "links"
    lks  <- mapM parseLink $ V.toList vlks
    return Shape { nodes = nds, links = lks }
  parseJSON _            = mzero

parseNode :: A.Value -> AT.Parser Node
parseNode (A.Object v) = Node <$>
                       v A..: "id"<*>
                       v A..:? "tags" <*>
                       v A..:? "values" <*>
                       v A..: "is_link_node" <*>
                       v A..: "is_fixed"
parseNode _            = fail "Node expected"

parseLink :: AT.Value -> AT.Parser Link
parseLink (A.Object v) = Link <$>
                       v A..: "source" <*>
                       v A..: "target" <*>
                       v A..: "link_node"
parseLink _            = fail "Link expected"

data Node = Node
  { id           :: !Integer
  , tags         :: !(Maybe [Tag])   -- ^ list of tags from the query
  , values       :: !(Maybe [Value]) -- ^ Known values from the query
  , isLinkNode   :: !Bool            -- ^ Does the node represent the link or the node (the oval shapes)
  , isFixed      :: !Bool            -- ^ Is the node a fixed starting point of the query
  } deriving (Eq, Show)

data Link = Link
  { source    :: !Integer -- ^ Node ID
  , target    :: !Integer -- ^ Node ID
  , linkNode  :: !Integer -- ^ Node ID
  } deriving (Eq, Show)

type Query = T.Text

type Subject = T.Text

type Predicate = T.Text

type Object = T.Text

type Label = T.Text

type Tag = T.Text

type Value = T.Text

