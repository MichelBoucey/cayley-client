{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Database.Cayley.Client as C
import Data.HashMap.Strict
import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "write" $
        it "writes a quad to Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            write c Quad { subject = "danny", predicate = "follows", C.object = "sandy", label = Nothing } `shouldReturn` Just (Object (fromList [("result",String "Successfully wrote 1 quads.")]))
