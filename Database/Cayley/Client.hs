{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Cayley.Client (
      Quad (..)
    -- * Connect & query
    , defaultCayleyConfig
    , connectCayley
    , query
    -- * REST API operations
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
    , successfulResults
    ) where

import           Control.Applicative                   ((<|>))
import           Control.Lens.Fold                     ((^?))
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson                            as A
import           Data.Aeson.Lens                       (key)
import qualified Data.Attoparsec.Text                  as AT
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData

import           Database.Cayley.Internal
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
query :: CayleyConnection -> Query -> IO (Either String A.Value)
query c q =
    runReaderT (doQuery (getManager c) (encodeUtf8 q)) (getConfig c)
  where
    doQuery m _q = do
        CayleyConfig {..} <- ask
        r <- apiRequest
                 m ("http://"
                    ++ serverName
                    ++ "/api/v"
                    ++ show apiVersion
                    ++ "/query/"
                    ++ show queryLang)
                 serverPort (RequestBodyBS _q)
        return $ case r of
            Just a  ->
                case a ^? key "result" of
                    Just v  -> Right v
                    Nothing ->
                      case a ^? key "error" of
                          Just e  ->
                              case A.fromJSON e of
                                  A.Success s -> Left s
                                  A.Error _e   -> Left _e
                          Nothing ->
                              Left "No JSON response from Cayley server"
            Nothing -> Left "Can't get any response from Cayley server"

-- | Write a 'Quad' with the given subject, predicate, object and optional
-- label. Throw result or extract amount of query 'successfulResults'
-- from it.
--
-- >λ> writeQuad conn "Humphrey" "loves" "Lauren" (Just "In love")
-- >Just (Object (fromList [("result",String "Successfully wrote 1 quads.")]))
--
writeQuad :: CayleyConnection -> Subject -> Predicate -> Object -> Maybe Label -> IO (Maybe A.Value)
writeQuad c s p o l =
   writeQuads c [Quad { subject = s, predicate = p, object = o, label = l }]

-- | Write the given 'Quad'.
write :: CayleyConnection -> Quad -> IO (Maybe A.Value)
write c q = writeQuads c [q]

-- | Delete the 'Quad' defined by the given subject, predicate, object
-- and optional label.
deleteQuad :: CayleyConnection -> Subject -> Predicate -> Object -> Maybe Label -> IO (Maybe A.Value)
deleteQuad c s p o l =
    deleteQuads c [Quad { subject = s, predicate = p, object = o, label = l }]

-- | Delete the given 'Quad'.
delete :: CayleyConnection -> Quad -> IO (Maybe A.Value)
delete c q = deleteQuads c [q]

-- | Write the given list of 'Quad'(s).
writeQuads :: CayleyConnection -> [Quad] -> IO (Maybe A.Value)
writeQuads c qs =
    runReaderT (_write (getManager c) qs) (getConfig c)
  where
    _write m _qs = do
         CayleyConfig {..} <- ask
         apiRequest
             m ("http://"
                ++ serverName
                ++ "/api/v"
                ++ show apiVersion
                ++ "/write")
             serverPort (toRequestBody _qs)

-- | Delete the given list of 'Quad'(s).
deleteQuads :: CayleyConnection -> [Quad] -> IO (Maybe A.Value)
deleteQuads c qs =
    runReaderT (_delete (getManager c) qs) (getConfig c)
  where
    _delete m _qs = do
        CayleyConfig {..} <- ask
        apiRequest
            m ("http://"
               ++ serverName
               ++ "/api/v"
               ++ show apiVersion
               ++ "/delete")
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
        CayleyConfig {..} <- ask
        r <- parseUrl ("http://"
                       ++ serverName
                       ++ "/api/v"
                       ++ show apiVersion
                       ++ "/write/file/nquad")
                 >>= \r -> return r { port = serverPort }
        t <- liftIO $
                 try $
                    flip httpLbs m
                        =<< formDataBody [partFileSource "NQuadFile" _p] r
        return $ case t of
            Right _r -> A.decode $ responseBody _r
            Left e  -> Just $
                A.object ["error" A..= T.pack (show (e :: SomeException))]

-- | A valid 'Quad' has its subject, predicate and object not empty.
isValid :: Quad -> Bool
isValid Quad {..} = T.empty `notElem` [subject, predicate, object]

-- | Given a subject, a predicate, an object and an optional label,
-- create a valid 'Quad'.
createQuad :: Subject -> Predicate -> Object -> Maybe Label -> Maybe Quad
createQuad s p o l =
    if T.empty `notElem` [s,p,o]
        then Just Quad { subject = s, predicate = p, object = o, label = l }
        else Nothing

-- | Get amount of successful results from a write/delete 'Quad'(s)
-- operation, or an explicite error message.
--
-- >λ> writeNQuadFile conn "testdata.nq" >>= successfulResults
-- >Right 11
--
successfulResults :: Maybe A.Value -> IO (Either String Int)
successfulResults m = return $
    case m of
        Just a  ->
            case a ^? key "result" of
                Just v  ->
                    case A.fromJSON v of
                        A.Success s ->
                            case AT.parse getAmount s of
                                AT.Done "" b -> Right b
                                _            ->
                                    Left "Can't get amount of successful results"
                        A.Error e   -> Left e
                Nothing ->
                    case a ^? key "error" of
                        Just e  ->
                            case A.fromJSON e of
                                A.Success s -> Left s
                                A.Error r   -> Left r
                        Nothing -> Left "No JSON response from Cayley server"
        Nothing -> Left "Can't get any response from Cayley server"
  where
    getAmount = do
        _ <- AT.string "Successfully "
        _ <- AT.string "deleted " <|> AT.string "wrote "
        a <- AT.decimal
        _ <- AT.string " quads."
        return a

