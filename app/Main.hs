{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative (many, (<**>))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Foldable (for_)
import Data.List (find, intercalate)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
import qualified Options.Applicative as Options
import System.Exit (exitFailure)
import Temple
  ( Expr
  , Kind (..)
  , Located (..)
  , Template
  , Type (..)
  , TypeError (..)
  , checkExpr
  , emptyInferEnv
  , emptyInferState
  , evalExpr
  , evalTemplate
  , exprParser
  , getRequirements
  , identParser
  , inferTemplate
  , runInferT
  , symbolic
  , templateParser
  , zonkDefault
  )
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage
import qualified Text.Sage as Sage

data Cli
  = Type !FilePath
  | Apply
      !FilePath
      -- | Template arguments
      [String]

data Argument
  = Argument
  { argName :: Text
  , argValue :: Located Expr
  }

argumentParser :: Sage.Parser Argument
argumentParser =
  Argument
    <$> identParser
    <* symbolic '='
    <*> exprParser

cliParser :: Options.Parser Cli
cliParser =
  Options.hsubparser
    ( Options.command "type" (Options.info typeParser (Options.progDesc "Infer the type of a template"))
        <> Options.command
          "apply"
          (Options.info applyParser (Options.progDesc "Apply a template to some arguments"))
    )
  where
    typeParser =
      Type
        <$> Options.strArgument (Options.metavar "FILE" <> Options.help "File to type")

    applyParser =
      Apply
        <$> Options.strArgument (Options.metavar "FILE" <> Options.help "File to type")
        <*> many
          ( Options.strOption $
              Options.long "arg"
                <> Options.metavar "NAME=VALUE"
                <> Options.help "Provide an argument to the template"
          )

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
  case cli of
    Type file -> type_ file
    Apply file args -> apply file args

typeError :: TypeError -> Diagnostic.Report
typeError (TypeMismatch offset expected actual) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "expected "
          ++ renderType expected
          ++ ", got "
          ++ renderType actual
    )
typeError (UnexpectedFields offset actual) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "record has unexpected fields "
          ++ renderFields actual
    )
typeError (MissingFields offset expected) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "record is missing fields "
          ++ renderFields expected
    )
typeError (UnexpectedConstructors offset actual) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "sum has unexpected constructors "
          ++ renderConstructors actual
    )
typeError (MissingConstructors offset expected) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "sum is missing constructors "
          ++ renderConstructors expected
    )
typeError (ArityMismatch offset expected actual) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "constructor requires "
          ++ show expected
          ++ " arguments, got "
          ++ show actual
    )
typeError (KindMismatch offset expected actual) =
  Diagnostic.emit
    (Diagnostic.Offset offset)
    Diagnostic.Caret
    ( fromString $
        "expected kind "
          ++ renderKind expected
          ++ ", got "
          ++ renderKind actual
    )

renderFields :: [(Text, Type)] -> String
renderFields [] = "none"
renderFields xs = intercalate ", " $ fmap (\(name, ty) -> Text.unpack name ++ " : " ++ renderType ty) xs

renderConstructors :: [(Text, [Type])] -> String
renderConstructors [] = "none"
renderConstructors xs =
  intercalate " | " $
    fmap
      ( \(name, tys) ->
          Text.unpack name
            ++ if null tys
              then ""
              else "(" ++ intercalate ", " (fmap renderType tys) ++ ")"
      )
      xs

renderType :: Type -> String
renderType (TMeta v) = "?" ++ show v
renderType TBool = "Bool"
renderType TString = "String"
renderType (TStream ty) = "Stream(" ++ renderType ty ++ ")"
renderType (TRecord fields) = "{" ++ renderType fields ++ "}"
renderType (TRecordField name ty rest) =
  Text.unpack name
    ++ " : "
    ++ renderType ty
    ++ case rest of
      TRowEnd -> ""
      _ -> ", " ++ renderType rest
renderType (TSum ctors) = "Sum(" ++ renderType ctors ++ ")"
renderType (TSumConstructor name tys rest) =
  Text.unpack name
    ++ ( if null tys
           then ""
           else "(" ++ intercalate ", " (fmap renderType tys) ++ ")"
       )
    ++ case rest of
      TRowEnd -> ""
      _ -> " | " ++ renderType rest
renderType TRowEnd = ""

renderKind :: Kind -> String
renderKind KType = "Type"
renderKind KRow = "Row"

displayReport :: FilePath -> Diagnostic.Report -> IO ()
displayReport file report = do
  content <- LazyByteString.readFile file
  LazyByteString.putStrLn
    . Diagnostic.render Diagnostic.defaultConfig (fromString file) content
    $ report

parseTemplate :: FilePath -> IO Template
parseTemplate file = do
  input <- ByteString.readFile file
  case Sage.parse templateParser input of
    Left err -> do
      displayReport file $ Text.Diagnostic.Sage.parseError err
      exitFailure
    Right x -> pure x

inferBindings :: FilePath -> Template -> IO [(Text.Text, Type)]
inferBindings file template = do
  result <-
    runInferT emptyInferEnv emptyInferState $ do
      inferTemplate template
      bindings <- getRequirements
      (traverse . traverse) zonkDefault bindings

  case result of
    Left err -> do
      displayReport file $ typeError err
      exitFailure
    Right (_state, bindings) -> pure bindings

type_ :: FilePath -> IO ()
type_ file = do
  template <- parseTemplate file
  bindings <- inferBindings file template
  for_ bindings $ \(binding, ty) -> do
    Text.putStr binding
    putStr " : "
    putStrLn $ renderType ty

apply :: FilePath -> [String] -> IO ()
apply file args = do
  template <- parseTemplate file

  args' <- for (zip [0 :: Int ..] args) $ \(index, arg) -> do
    case Sage.parse (argumentParser <* Sage.eof) (fromString arg) of
      Left err -> do
        LazyByteString.putStrLn
          . Diagnostic.render
            Diagnostic.defaultConfig
            (fromString $ "(argument " ++ show index ++ ")")
            (fromString arg)
          $ Text.Diagnostic.Sage.parseError err
        exitFailure
      Right arg' -> pure (index, arg, arg')

  bindings <- inferBindings file template

  scope <-
    for bindings $ \(name, ty) -> do
      case find ((name ==) . argName . (\(_, _, x) -> x)) args' of
        Nothing -> do
          putStrLn $ "error: argument " ++ Text.unpack name ++ " not provided"
          exitFailure
        Just (index, argPlain, arg) -> do
          result <- runInferT emptyInferEnv emptyInferState $ checkExpr (argValue arg) ty

          case result of
            Right (_state, ()) ->
              pure (name, argValue arg)
            Left err -> do
              LazyByteString.putStrLn
                . Diagnostic.render
                  Diagnostic.defaultConfig
                  (fromString $ "(argument " ++ show index ++ ")")
                  (fromString argPlain)
                $ typeError err
              exitFailure

  scope' <-
    for scope $ \(name, expr) -> do
      let !value = evalExpr mempty $ locatedValue expr
      pure (name, value)

  LazyByteString.putStrLn $ evalTemplate (Map.fromList scope') template
