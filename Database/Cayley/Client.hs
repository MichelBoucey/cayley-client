{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Cayley.Client (

      Quad (..)

    -- * Connection
    , defaultCayleyConfig
    , connectCayley

    -- * Operations
    , query
    , Shape
    , queryShape
    , write
    , writeQuad
    , writeQuads
    , writeNQuadFile
    , delete
    , deleteQuad
    , deleteQuads

    -- * Utils
    , createQuad
    , isValid
    , results

    ) where

import           Control.Applicative                   ((<|>))
import           Control.Lens.Fold                     ((^?))
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson                            as A
import qualified Data.Aeson.Lens                       as L
import qualified Data.Attoparsec.Text                  as APT
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData

import           Database.Cayley.Client.Internal
import           Database.Cayley.Types

-- | Get a connection to Cayley with the given configuration.
--
-- >λ> conn <- connectCayley defaultCayleyConfig
--
connectCayley :: CayleyConfig -> IO CayleyConnection
connectCayley c =
  newManager defaultManagerSettings >>= \m -> return $ CayleyConnection (c,m)

-- | Perform a query, in Gremlin graph query language per default (or in MQL).
--
-- >λ> query conn "graph.Vertex('Humphrey Bogart').In('name').All()"
-- >Right (Array (fromList [Object (fromList [("id",String "/en/humphrey_bogart")])]))
--
query :: CayleyConnection
      -> Query
      -> IO (Either String A.Value)
query c q =
  runReaderT (doQuery (getManager c) (encodeUtf8 q)) (getConfig c)
  where
    doQuery m _q = do
        CayleyConfig{..} <- ask
        r <- apiRequest
               m (urlBase serverName apiVersion
                 ++ "/query/" ++ show queryLang)
               serverPort (RequestBodyBS _q)
        return $ case r of
          Just a  ->
            case a ^? L.key "result" of
              Just v  -> Right v
              Nothing ->
                case a ^? L.key "error" . L._String of
                  Just e  -> fail (show e)
                  Nothing -> fail "No JSON response from Cayley server"
          Nothing -> fail "Can't get any response from Cayley server"

-- | Return the description of the given executed query.
queryShape :: CayleyConnection
           -> Query
           -> IO (A.Result Shape)
queryShape c q =
  runReaderT (doShape (getManager c) (encodeUtf8 q)) (getConfig c)
  where
    doShape m _q = do
      CayleyConfig{..} <- ask
      r <- apiRequest
             m (urlBase serverName apiVersion ++ "/shape/" ++ show queryLang)
             serverPort (RequestBodyBS _q)
      return $
        case r of
          Just o  -> A.fromJSON o
          Nothing -> A.Error "API request error"

-- | Write a 'Quad' with the given subject, predicate, object and optional
-- label. Throw result or extract amount of query 'results'
-- from it.
--
-- >λ> writeQuad conn "Humphrey" "loves" "Lauren" (Just "In love")
-- >Just (Object (fromList [("result",String "Successfully wrote 1 quads.")]))
--
writeQuad :: CayleyConnection
          -> Subject
          -> Predicate
          -> Object
          -> Maybe Label
          -> IO (Maybe A.Value)
writeQuad c s p o l =
  writeQuads c [Quad { subject = s, predicate = p, object = o, label = l }]

-- | Write the given 'Quad'.
write :: CayleyConnection
      -> Quad
      -> IO (Maybe A.Value)
write c q = writeQuads c [q]

-- | Delete the 'Quad' defined by the given subject, predicate, object
-- and optional label.
deleteQuad :: CayleyConnection
           -> Subject
           -> Predicate
           -> Object
           -> Maybe Label
           -> IO (Maybe A.Value)
deleteQuad c s p o l =
  deleteQuads c [Quad { subject = s, predicate = p, object = o, label = l }]

-- | Delete the given 'Quad'.
delete :: CayleyConnection -> Quad -> IO (Maybe A.Value)
delete c q = deleteQuads c [q]

-- | Write the given list of 'Quad'(s).
writeQuads :: CayleyConnection
           -> [Quad]
           -> IO (Maybe A.Value)
writeQuads c qs =
  runReaderT (_write (getManager c) qs) (getConfig c)
  where
    _write m _qs = do
      CayleyConfig{..} <- ask
      apiRequest
        m (urlBase serverName apiVersion ++ "/write")
        serverPort (toRequestBody _qs)

-- | Delete the given list of 'Quad'(s).
deleteQuads :: CayleyConnection
            -> [Quad]
            -> IO (Maybe A.Value)
deleteQuads c qs =
  runReaderT (_delete (getManager c) qs) (getConfig c)
  where
    _delete m _qs = do
      CayleyConfig{..} <- ask
      apiRequest
        m (urlBase serverName apiVersion ++ "/delete")
        serverPort
        (toRequestBody _qs)

-- | Write a N-Quad file.
--
-- >λ> writeNQuadFile conn "testdata.nq"
-- >Just (Object (fromList [("result",String "Successfully wrote 11 quads.")]))
--
writeNQuadFile :: (MonadThrow m, MonadIO m)
               => CayleyConnection
               -> FilePath
               -> m (Maybe A.Value)
writeNQuadFile c p =
  runReaderT (writenq (getManager c) p) (getConfig c)
  where
    writenq m _p = do
      CayleyConfig{..} <- ask
      r <- parseUrl (urlBase serverName apiVersion ++ "/write/file/nquad")
             >>= \r -> return r { port = serverPort }
      t <- liftIO $
             try $
               flip httpLbs m
                 =<< formDataBody [partFileSource "NQuadFile" _p] r
      return $ case t of
        Right _r -> A.decode $ responseBody _r
        Left e   -> Just $
          A.object ["error" A..= T.pack (show (e :: SomeException))]

-- | A valid 'Quad' has its subject, predicate and object not empty.
isValid :: Quad -> Bool
isValid Quad{..} = T.empty `notElem` [subject, predicate, object]

-- | Given a subject, a predicate, an object and an optional label,
-- create a valid 'Quad'.
createQuad :: Subject
           -> Predicate
           -> Object
           -> Maybe Label
           -> Maybe Quad
createQuad s p o l =
  if T.empty `notElem` [s,p,o]
    then Just Quad { subject = s, predicate = p, object = o, label = l }
    else Nothing

-- | Get amount of results from a write/delete 'Quad'(s) operation,
-- or an explicite error message.
--
-- >λ> writeNQuadFile conn "testdata.nq" >>= results
-- >Right 11
--
results :: Maybe A.Value
        -> IO (Either String Int)
results m = return $
  case m of
    Just v ->
      case v ^? L.key "result" . L._String of
        Just r  ->
          case APT.parse getAmount r of
            APT.Done "" i -> Right i
            _             -> fail "Can't get amount of results"
        Nothing ->
          case v ^? L.key "error" . L._String of
            Just e  -> fail (show e)
            Nothing -> fail "No JSON response from Cayley server"
    Nothing -> fail "Can't get any response from Cayley server"
  where
  getAmount = do
      _ <- APT.string "Successfully "
      _ <- APT.string "deleted " <|> APT.string "wrote "
      a <- APT.decimal
      _ <- APT.string " quads."
      return a

