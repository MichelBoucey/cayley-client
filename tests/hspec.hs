{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.HashMap.Strict
import           Database.Cayley.Client as C
import           Database.Cayley.Types  as CT
import           Test.Hspec

main :: IO ()
main = hspec $ do

    describe "write" $
        it "writes a quad to Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            write c Quad { subject = "<danny>", predicate = "<follows>", C.object = "<sandy>", label = Nothing } `shouldReturn` Just (Object (fromList [("result",String "Successfully wrote 1 quads.")]))

    describe "query" $
        it "query Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            Right a <- query c "graph.Vertex('<sandy>').In('<follows>').All()"
            encode a `shouldBe` "[{\"id\":\"<danny>\"}]"

    describe "shape" $
        it "returns the description of the last query executed" $ do
            c <- connectCayley defaultCayleyConfig
            queryShape c "graph.Vertex('<sandy>').In('<follows>').All()" `shouldReturn` Right (Shape {nodes = [Node {CT.id = 4, tags = Nothing, values = Just ["<sandy>"], isLinkNode = False, isFixed = True},Node {CT.id = 8, tags = Nothing, values = Just ["<follows>"], isLinkNode = False, isFixed = True},Node {CT.id = 2, tags = Nothing, values = Nothing, isLinkNode = True, isFixed = False},Node {CT.id = 1, tags = Just ["id"], values = Nothing, isLinkNode = False, isFixed = False}], links = [Link {source = 1, target = 4, linkNode = 2}]})

    describe "writeNQuadFile" $
        it "writes quads from a file to Cayley server" $ do
            c <- connectCayley defaultCayleyConfig
            (writeNQuadFile c "./tests/testdata.nq" >>= results) `shouldReturn` Right 15

