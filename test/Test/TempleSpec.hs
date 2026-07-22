module Test.TempleSpec (spec) where

import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.String (fromString)
import Temple (Expr (..), Located (..), Part (..), exprParser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Sage (parse)

qqq :: String
qqq = "\"\"\""

spec :: Spec
spec = do
  describe "multiline string" $ do
    describe "single line" $ do
      it "1" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq ++ qqq
              ]
        parse exprParser input `shouldBe` Right (Located 0 $ MultilineString [])

      it "2" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq ++ "hello" ++ qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello"])

      it "3" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq ++ "hello"
              , qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

    describe "many lines" $ do
      it "1" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "hello" ++ qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello"])

      it "2" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "hello"
              , qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

      it "3" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  hello" ++ qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello"])

      it "4" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  hello"
              , qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

      it "5" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  hello"
              , "  " ++ qqq
              ]
        parse exprParser input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

      it "6" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  a"
              , "    b"
              , "  c" ++ qqq
              ]
        parse exprParser input
          `shouldBe` Right
            ( Located 0 $
                MultilineString
                  [ PartText $ fromString "a\n"
                  , PartText $ fromString "  b\n"
                  , PartText $ fromString "c"
                  ]
            )

      it "7" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  a"
              , "    b"
              , "  c"
              , "  " ++ qqq
              ]
        parse exprParser input
          `shouldBe` Right
            ( Located 0 $
                MultilineString
                  [ PartText $ fromString "a\n"
                  , PartText $ fromString "  b\n"
                  , PartText $ fromString "c\n"
                  ]
            )
