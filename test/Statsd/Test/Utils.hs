module Statsd.Test.Utils (utilsSpec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Statsd.Utils


utilsSpec :: Spec
utilsSpec = do
    describe "sanitizeKeyName" $ modifyMaxSize (+1000) sanitizeKeyNameSpec

sanitizeKeyNameSpec :: Spec
sanitizeKeyNameSpec = do
    prop "never makes a key longer" $
        \s -> length (sanitizeKeyName s) <= length s
    prop "only returns valid chars" $ do
        let validChars = "_-." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        all (`elem` validChars) . sanitizeKeyName
    it "keeps uppercase, lowercase and numbers"
        (sanitizeKeyName "aB0" `shouldBe` "aB0")
    it "replaces a chain of spaces with a underscore"
        (sanitizeKeyName "hello   world" `shouldBe` "hello_world")
    it "replaces a forwardslash with a dash"
        (sanitizeKeyName "hello/world" `shouldBe` "hello-world")
