{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.HashMap.Strict
import           Database.Cayley.Client as C
import           Test.Hspec

main :: IO ()
main = hspec $ do

    describe "write" $
        it "writes a quad to Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            write c Quad { subject = "danny", predicate = "follows", C.object = "sandy", label = Nothing } `shouldReturn` Just (Object (fromList [("result",String "Successfully wrote 1 quads.")]))

    describe "query" $
        it "query to Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            Right a <- query c "graph.Vertex('sandy').In('follows').All()"
            encode a `shouldBe` "[{\"id\":\"danny\"}]"

    describe "writeNQuadFile" $
        it "writes quads from a file to Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            (writeNQuadFile c "./tests/testdata.nq" >>= successfulResults) `shouldReturn` Right 15

