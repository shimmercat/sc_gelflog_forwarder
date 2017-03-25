{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import TheHenrik.UDPPacker
import Control.Exception                         (evaluate)

import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString                 as B

main :: IO ()
main = do
    sample_id <- computeMessageId "AlphaBetaTheta"
    sample_id2 <- computeMessageId "AlphaBetaTheta"
    hspec $ do
        describe "messageHeader" $ do
            it "Produces a 12 bytes header" $ do
                LB.length (messageHeader "01234567" 0 5) `shouldBe` 12
            it "Produces 8 bytes ids" $ do
                (B.length sample_id) `shouldBe` 8
            it "Produces different ids" $ do
                sample_id `shouldNotBe` sample_id2
