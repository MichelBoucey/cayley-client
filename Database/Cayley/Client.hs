{-# LANGUAGE OverloadedStrings #-}

module Database.Cayley.Client
    ( defaultCayleyConfig
    , connectCayley
    , query
    , writeQuad
    , deleteQuad
    , writeQuads
    , deleteQuads
    , writeNQuadFile
    , successfulResults
    ) where
 
import Control.Applicative ((<|>))
import Control.Lens.Fold ((^?))
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.Aeson.Lens (key)
import qualified Data.Attoparsec.Text as AT
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import Database.Cayley.Internal
import Database.Cayley.Types

-- | Get a connection to Cayley with the given configuration.
--
-- >λ> conn <- connectCayley defaultCayleyConfig
--
connectCayley :: CayleyConfig -> IO CayleyConnection
connectCayley c =
    newManager defaultManagerSettings >>= \m -> return $ CayleyConnection (c,m)

-- | Perform a query in Gremlin graph query language per default (or in MQL).
--
-- >λ> query conn "graph.Vertex('Humphrey Bogart').In('name').All()"
-- >Right (Array (fromList [Object (fromList [("id",String "/en/humphrey_bogart")])]))
--
query :: CayleyConnection -> T.Text -> IO (Either String A.Value)
query c q =
    runReaderT (doQuery (getManager c) (encodeUtf8 q)) (getConfig c)
  where
    doQuery m q = do
        c <- ask
        r <- apiRequest
                 m ("http://" ++ serverName c ++ "/api/v" ++
                    show (apiVersion c) ++ "/query/"
                    ++ show (queryLang c))
                 (serverPort c) (RequestBodyBS q)
        case r of
            Just a  ->
                case a ^? key "result" of
                    Just v  -> return $ Right v
                    Nothing ->
                      case a ^? key "error" of
                          Just e  ->
                              case A.fromJSON e of
                                  A.Success s -> return $ Left s
                                  A.Error e   -> return $ Left e
                          Nothing -> return $
                              Left "No JSON response from Cayley"
            Nothing -> return $ Left "Can't get any response from Cayley"

-- | Write a 'Quad' with the given subject, predicate, object and optional
-- label. Throw result or extract amount of query 'successfulResults'
-- from it.
--
-- >λ> writeQuad conn "Humphrey" "loves" "Lauren" (Just "In love")
-- >Just (Object (fromList [("result",String "Successfully wrote 1 triples.")]))
--
writeQuad :: CayleyConnection
          -> T.Text             -- ^ Subject node
          -> T.Text             -- ^ Predicate node
          -> T.Text             -- ^ Object node
          -> Maybe T.Text       -- ^ Label node
          -> IO (Maybe A.Value)
writeQuad c s p o l =
   writeQuads c [Quad { subject = s, predicate = p, object = o, label = l }]

-- | Delete the 'Quad' defined by the given subject, predicate, object
-- and optional label.
deleteQuad :: CayleyConnection
           -> T.Text
           -> T.Text
           -> T.Text
           -> Maybe T.Text
           -> IO (Maybe A.Value)
deleteQuad c s p o l =
    deleteQuads c [Quad { subject = s, predicate = p, object = o, label = l }]

-- | Write the given list of 'Quad'(s).
writeQuads :: CayleyConnection -> [Quad] -> IO (Maybe A.Value)
writeQuads c qs =
    runReaderT (write (getManager c) qs) (getConfig c)
  where
    write m qs = do
        c <- ask
        apiRequest
            m ("http://" ++ serverName c
               ++ "/api/v" ++ show (apiVersion c)
               ++ "/write")
            (serverPort c) (toRequestBody qs)

-- | Delete the given list of 'Quad'(s).
deleteQuads :: CayleyConnection -> [Quad] -> IO (Maybe A.Value)
deleteQuads c qs =
    runReaderT (delete (getManager c) qs) (getConfig c)
  where
    delete m qs = do
        c <- ask
        apiRequest
            m ("http://" ++ serverName c
               ++ "/api/v" ++ show (apiVersion c)
               ++ "/delete")
            (serverPort c)
            (toRequestBody qs)

-- | Write a N-Quad file.
writeNQuadFile c p =
    runReaderT (writenq (getManager c) p) (getConfig c)
  where
    writenq m p = do
        c <- ask
        r <- parseUrl ("http://" ++ serverName c
                       ++ "/api/v" ++ show (apiVersion c)
                       ++ "/write/file/nquad")
                 >>= \r -> return r { port = serverPort c}
        t <- liftIO $
                 try $
                    flip httpLbs m
                        =<< formDataBody [partFileSource "NQuadFile" p] r
        case t of
            Right r -> return $ A.decode $ responseBody r
            Left e  ->
                return $
                    Just $
                        A.object
                            ["error" A..= T.pack (show (e :: SomeException))]

-- | Get amount of successful results from a write/delete 'Quad'(s)
-- operation.
--
-- >λ> writeNQuadFile conn "testdata.nq" >>= successfulResults
-- >Right 9
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
                               AT.Done "" a -> Right a
                               _ -> Left "Can't get amount of successful results"
                        A.Error e -> Left e
                Nothing ->
                    case a ^? key "error" of
                        Just e  ->
                            case A.fromJSON e of
                                A.Success s -> Left s
                                A.Error e   -> Left e
                        Nothing -> Left "No JSON response from Cayley"
        Nothing -> Left "Can't get any response from Cayley"
  where
    getAmount = do
        AT.string "Successfully "
        AT.string "deleted " <|> AT.string "wrote "
        a <- AT.decimal
        AT.string " triples."
        return a
