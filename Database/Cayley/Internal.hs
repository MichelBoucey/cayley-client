{-# LANGUAGE OverloadedStrings #-}

module Database.Cayley.Internal where
 
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Text as T (pack)
import Data.Vector (fromList)
import Network.HTTP.Client

import Database.Cayley.Types

apiRequest :: Manager
           -> String
           -> Int
           -> RequestBody
           -> ReaderT CayleyConfig IO (Maybe A.Value)
apiRequest m u p b = do
    r <- parseUrl u >>= \c ->
             return c { method = "POST", port = p, requestBody = b }
    t <- liftIO $ try $ httpLbs r m
    case t of
        Right r -> return $ A.decode $ responseBody r
        Left e  ->
            return $
                Just $
                    A.object ["error" A..= T.pack (show (e :: SomeException))]

toRequestBody :: [Quad] -> RequestBody
toRequestBody qs = RequestBodyLBS $ A.encode $ fromList $ map A.toJSON qs

getManager (CayleyConnection (_,m)) = m

getConfig (CayleyConnection (c,_)) = c
