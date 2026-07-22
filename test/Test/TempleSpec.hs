module Test.TempleSpec (spec) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.List (intercalate)
import Data.String (fromString)
import Temple (Expr (..), Located (..), Part (..), exprParser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Sage (eof, parse)

qqq :: String
qqq = "\"\"\""

spec :: Spec
spec = do
  describe "string" $ do
    it "1" $ do
      let
        input1 =
          ByteString.Char8.pack "\"asdf {% include "

        input2 =
          ByteString.Char8.pack "\"test\" %} asdf\""

        input = input1 <> input2

      parse (exprParser <* eof) input
        `shouldBe` Right
          ( Located 0 $
              String
                [ PartText $ fromString "asdf "
                , PartInclude $ Located (ByteString.length input1) (fromString "test")
                , PartText $ fromString " asdf"
                ]
          )

  describe "multiline string" $ do
    describe "single line" $ do
      it "1" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq ++ qqq
              ]
        parse (exprParser <* eof) input `shouldBe` Right (Located 0 $ MultilineString [])

      it "2" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq ++ "hello" ++ qqq
              ]
        parse (exprParser <* eof) input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello"])

      it "3" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq ++ "hello"
              , qqq
              ]
        parse (exprParser <* eof) input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

    describe "many lines" $ do
      it "1" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "hello" ++ qqq
              ]
        parse (exprParser <* eof) input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello"])

      it "2" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "hello"
              , qqq
              ]
        parse (exprParser <* eof) input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

      it "3" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  hello" ++ qqq
              ]
        parse (exprParser <* eof) input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello"])

      it "4" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  hello"
              , qqq
              ]
        parse (exprParser <* eof) input
          `shouldBe` Right (Located 0 $ MultilineString [PartText $ fromString "hello\n"])

      it "5" $ do
        let
          input =
            ByteString.Char8.pack . unlines $
              [ qqq
              , "  hello"
              , "  " ++ qqq
              ]
        parse (exprParser <* eof) input
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
        parse (exprParser <* eof) input
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
        parse (exprParser <* eof) input
          `shouldBe` Right
            ( Located 0 $
                MultilineString
                  [ PartText $ fromString "a\n"
                  , PartText $ fromString "  b\n"
                  , PartText $ fromString "c\n"
                  ]
            )

      it "8" $ do
        let
          input1 =
            ByteString.Char8.pack . intercalate "\n" $
              [ qqq
              , "  a"
              , "  {% include "
              ]

          input2 =
            ByteString.Char8.pack . unlines $
              [ "\"test\" %}"
              , "  c"
              , "  " ++ qqq
              ]

          input = input1 <> input2

        parse (exprParser <* eof) input
          `shouldBe` Right
            ( Located 0 $
                MultilineString
                  [ PartText $ fromString "a\n"
                  , PartInclude $ Located (ByteString.length input1) (fromString "test")
                  , PartText $ fromString "\n"
                  , PartText $ fromString "c\n"
                  ]
            )

      it "9" $ do
        let
          input1 =
            ByteString.Char8.pack . intercalate "\n" $
              [ qqq
              , "  a"
              , "  {{"
              ]

          input2 =
            ByteString.Char8.pack . unlines $
              [ "blah}}"
              , "  c"
              , "  " ++ qqq
              ]

          input = input1 <> input2

        parse (exprParser <* eof) input
          `shouldBe` Right
            ( Located 0 $
                MultilineString
                  [ PartText $ fromString "a\n"
                  , PartExpr $ Located (ByteString.length input1) (Var $ fromString "blah")
                  , PartText $ fromString "\n"
                  , PartText $ fromString "c\n"
                  ]
            )
