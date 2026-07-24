{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative (many, (<**>))
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Foldable (for_)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
import qualified Options.Applicative as Options
import System.Exit (exitFailure)
import Temple
  ( Binding (..)
  , EvalEnv (..)
  , LExpr
  , Located (..)
  , Offset
  , Template
  , Type (..)
  , TypeError (..)
  , checkExpr
  , checkPartIncludeDisabled
  , defaultCtx
  , defaultEvalEnv
  , emptyInferEnv
  , emptyInferState
  , evalExpr
  , evalTemplate
  , exprParser
  , getOffset
  , identParser
  , runInferT
  , symbolic
  , templateParser, renderType, renderKind
  )
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage
import qualified Text.Sage as Sage

data Cli
  = Type !FilePath
  | Apply
      !FilePath
      -- | Template arguments
      [String]
  | Locate
      !FilePath
      -- | Variable to search for
      String

data Argument
  = Argument
  { argName :: Text
  , argValue :: LExpr Offset
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
        <> Options.command
          "locate"
          (Options.info locateParser (Options.progDesc "List a variable's occurrances"))
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

    locateParser =
      Locate
        <$> Options.strArgument (Options.metavar "FILE" <> Options.help "Source file")
        <*> Options.strArgument (Options.metavar "VAR" <> Options.help "Variable to search for")

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
  case cli of
    Type file -> type_ file
    Apply file args -> apply file args
    Locate file var -> locate file var

data MultiReport
  = SingleReport Diagnostic.Report
  | MultiReport
      !Diagnostic.Report
      -- | Next file
      !FilePath
      -- | Report for next file
      MultiReport

typeError :: TypeError Offset -> MultiReport
typeError (NotInScope offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "not in scope")
typeError (TypeMismatch offset expected actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "expected "
            ++ renderType expected
            ++ ", got "
            ++ renderType actual
      )
typeError (UnexpectedFields offset actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "record has unexpected fields "
            ++ renderFields actual
      )
typeError (MissingFields offset expected) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "record is missing fields "
            ++ renderFields expected
      )
typeError (UnexpectedConstructors offset actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "sum has unexpected constructors "
            ++ renderConstructors actual
      )
typeError (MissingConstructors offset expected) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "sum is missing constructors "
            ++ renderConstructors expected
      )
typeError (ArityMismatch offset expected actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "constructor requires "
            ++ show expected
            ++ " arguments, got "
            ++ show actual
      )
typeError (KindMismatch offset expected actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      ( fromString $
          "expected kind "
            ++ renderKind expected
            ++ ", got "
            ++ renderKind actual
      )
typeError (NotRequirement offset _name) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "does not satisfy a known requirement")
typeError (RequirementAlreadySatisfied offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "requirement has previously been satisfied")
typeError (BlockBadRequirementType offset actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString $ "requirement of type " ++ renderType actual ++ " cannot be satisfied by a block")
typeError (FileNotFound offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "file not found")
typeError (ParentParseError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset $ getOffset offset)
        Diagnostic.Caret
        (fromString $ "parse error in parent")
    )
    file
    (SingleReport $ Text.Diagnostic.Sage.parseError err)
typeError (ParentTypeError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset $ getOffset offset)
        Diagnostic.Caret
        (fromString "type error in parent")
    )
    file
    (typeError err)
typeError (IncludeDisabled offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "includes are disabled")
typeError (IncludeParseError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset $ getOffset offset)
        Diagnostic.Caret
        (fromString "parse error in included template")
    )
    file
    (SingleReport $ Text.Diagnostic.Sage.parseError err)
typeError (IncludeTypeError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset $ getOffset offset)
        Diagnostic.Caret
        (fromString "type error in included template")
    )
    file
    (typeError err)
typeError (NotParam offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "does not satisfy a known requirement")
typeError (ParamAlreadyBound offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset $ getOffset offset)
      Diagnostic.Caret
      (fromString "requirement has previously been satisfied")

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

displayMultiReport ::
  FilePath ->
  -- | Read a file
  (FilePath -> IO LazyByteString) ->
  MultiReport ->
  IO ()
displayMultiReport file readFile' (SingleReport report) = displayReport file readFile' report
displayMultiReport file readFile' (MultiReport report nextFile nextReport) = do
  displayReport file readFile' report
  displayMultiReport nextFile readFile' nextReport

displayReport :: FilePath -> (FilePath -> IO LazyByteString) -> Diagnostic.Report -> IO ()
displayReport file readFile' report = do
  content <- readFile' file
  LazyByteString.putStrLn
    . Diagnostic.render Diagnostic.defaultConfig (fromString file) content
    $ report

parseTemplate :: FilePath -> IO (Template Offset)
parseTemplate file = do
  input <- ByteString.readFile file
  case Sage.parse (templateParser file <* Sage.eof) input of
    Left err -> do
      displayReport file LazyByteString.readFile $ Text.Diagnostic.Sage.parseError err
      exitFailure
    Right x -> pure x

inferBindings ::
  FilePath ->
  Template Offset ->
  IO (Map FilePath (Template Offset), [Binding])
inferBindings file template = do
  result <- runExceptT $ Temple.inferBindings file template

  case result of
    Left err -> do
      displayMultiReport file LazyByteString.readFile $ typeError err
      exitFailure
    Right x ->
      pure x

type_ :: FilePath -> IO ()
type_ file = do
  template <- parseTemplate file
  (_deps, bindings) <- inferBindings file template
  for_ bindings $ \binding -> do
    Text.putStr $ bindingName binding
    putStr " : "
    putStrLn . renderType $ bindingType binding

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

  (deps, bindings) <- inferBindings file template

  scope <-
    for bindings $ \binding -> do
      case find ((bindingName binding ==) . argName . (\(_, _, x) -> x)) args' of
        Nothing -> do
          putStrLn $ "error: argument " ++ Text.unpack (bindingName binding) ++ " not provided"
          exitFailure
        Just (index, argPlain, arg) -> do
          result <-
            runInferT (emptyInferEnv ".") emptyInferState $
              checkExpr checkPartIncludeDisabled (argValue arg) (bindingType binding)

          case result of
            Right (_state, ()) ->
              pure (bindingName binding, argValue arg)
            Left err -> do
              displayMultiReport
                (fromString $ "(argument " ++ show index ++ ")")
                (const . pure $ fromString argPlain)
                (typeError err)
              exitFailure

  scope' <-
    for scope $ \(name, expr) -> do
      let !value = evalExpr (defaultEvalEnv "." mempty) $ locatedVal expr
      pure (name, value)

  LazyByteString.putStrLn $
    evalTemplate (defaultEvalEnv file deps){eeScope = Map.fromList scope' <> defaultCtx} template

locate :: FilePath -> String -> IO ()
locate file var = do
  template <- parseTemplate file

  (_deps, bindings) <- inferBindings file template

  case find ((fromString var ==) . bindingName) bindings of
    Nothing -> do
      putStrLn $ "error: variable '" ++ var ++ "' not found"
    Just binding -> do
      let
        makeReports bindingFile bindingOffset rest =
          let
            report =
              Diagnostic.emit
                (Diagnostic.Offset $ getOffset bindingOffset)
                Diagnostic.Caret
                (fromString "variable found")
          in
            ( bindingFile
            , case rest of
                [] -> SingleReport report
                (bindingFile', bindingOffset') : rest' ->
                  let
                    (bindingFile'', reports'') = makeReports bindingFile' bindingOffset' rest'
                  in
                    MultiReport report bindingFile'' reports''
            )

        (reportsFile, reports) =
          case bindingLocations binding of
            (bindingFile, bindingOffset) :| rest ->
              makeReports bindingFile bindingOffset rest

      displayMultiReport reportsFile LazyByteString.readFile reports
