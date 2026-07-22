{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative (many, (<**>))
import Control.Monad (guard)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.Foldable (for_)
import Data.List (find, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)
import qualified Options.Applicative as Options
import System.Exit (exitFailure)
import Temple
  ( EvalEnv (..)
  , Expr
  , InferEnv (..)
  , InferState (..)
  , Kind (..)
  , Located (..)
  , Requirement (..)
  , Template
  , Type (..)
  , TypeError (..)
  , checkExpr
  , checkTemplate
  , defaultCtx
  , defaultEvalEnv
  , defaultScope
  , emptyInferEnv
  , emptyInferState
  , evalExpr
  , evalTemplate
  , exprParser
  , getRequirements
  , identParser
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

data MultiReport
  = SingleReport Diagnostic.Report
  | MultiReport
      !Diagnostic.Report
      -- | Next file
      !FilePath
      -- | Report for next file
      MultiReport

typeError :: TypeError -> MultiReport
typeError (NotInScope offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      (fromString "not in scope")
typeError (TypeMismatch offset expected actual) =
  SingleReport $
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
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      ( fromString $
          "record has unexpected fields "
            ++ renderFields actual
      )
typeError (MissingFields offset expected) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      ( fromString $
          "record is missing fields "
            ++ renderFields expected
      )
typeError (UnexpectedConstructors offset actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      ( fromString $
          "sum has unexpected constructors "
            ++ renderConstructors actual
      )
typeError (MissingConstructors offset expected) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      ( fromString $
          "sum is missing constructors "
            ++ renderConstructors expected
      )
typeError (ArityMismatch offset expected actual) =
  SingleReport $
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
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
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
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      (fromString "block does not satisfy a known requirement")
typeError (RequirementAlreadySatisfied offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      (fromString "requirement has previously been satisfied")
typeError (BlockBadRequirementType offset actual) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      (fromString $ "requirement of type " ++ renderType actual ++ " cannot be satisfied by a block")
typeError (FileNotFound offset) =
  SingleReport $
    Diagnostic.emit
      (Diagnostic.Offset offset)
      Diagnostic.Caret
      (fromString "file not found")
typeError (ParentParseError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset offset)
        Diagnostic.Caret
        (fromString $ "parse error in parent")
    )
    file
    (SingleReport $ Text.Diagnostic.Sage.parseError err)
typeError (ParentTypeError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset offset)
        Diagnostic.Caret
        (fromString $ "type error in parent")
    )
    file
    (typeError err)
typeError (IncludeParseError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset offset)
        Diagnostic.Caret
        (fromString $ "parse error in included template")
    )
    file
    (SingleReport $ Text.Diagnostic.Sage.parseError err)
typeError (IncludeTypeError offset file err) =
  -- TODO: extend `diagnostica` to handle this sort of nesting?
  MultiReport
    ( Diagnostic.emit
        (Diagnostic.Offset offset)
        Diagnostic.Caret
        (fromString $ "type error in included template")
    )
    file
    (typeError err)

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
renderType (TVar v) = Text.unpack v
renderType TBool = "Bool"
renderType TString = "String"
renderType (TFn args retTy) = "Fn(" ++ intercalate ", " (fmap renderType args) ++ ") -> " ++ renderType retTy
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

displayMultiReport :: FilePath -> (FilePath -> IO LazyByteString) -> MultiReport -> IO ()
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

parseTemplate :: FilePath -> IO Template
parseTemplate file = do
  input <- ByteString.readFile file
  case Sage.parse (templateParser file <* Sage.eof) input of
    Left err -> do
      displayReport file LazyByteString.readFile $ Text.Diagnostic.Sage.parseError err
      exitFailure
    Right x -> pure x

defaultInferEnv :: FilePath -> InferEnv
defaultInferEnv currentFile = (emptyInferEnv currentFile){ieScope = defaultScope}

inferBindings :: FilePath -> Template -> IO (Map FilePath Template, [(Text, Type)])
inferBindings file template = do
  result <-
    runInferT (defaultInferEnv file) emptyInferState $ do
      checkTemplate template
      requirements <- getRequirements
      (traverse . traverse)
        zonkDefault
        ( mapMaybe
            ( \req -> do
                guard . not $ reqSatisfied req
                pure (reqName req, reqType req)
            )
            requirements
        )

  case result of
    Left err -> do
      displayMultiReport file LazyByteString.readFile $ typeError err
      exitFailure
    Right (state, bindings) -> pure (isDependencies state, bindings)

type_ :: FilePath -> IO ()
type_ file = do
  template <- parseTemplate file
  (_deps, bindings) <- inferBindings file template
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

  (deps, bindings) <- inferBindings file template

  scope <-
    for bindings $ \(name, ty) -> do
      case find ((name ==) . argName . (\(_, _, x) -> x)) args' of
        Nothing -> do
          putStrLn $ "error: argument " ++ Text.unpack name ++ " not provided"
          exitFailure
        Just (index, argPlain, arg) -> do
          result <- runInferT (emptyInferEnv ".") emptyInferState $ checkExpr (argValue arg) ty

          case result of
            Right (_state, ()) ->
              pure (name, argValue arg)
            Left err -> do
              displayMultiReport
                (fromString $ "(argument " ++ show index ++ ")")
                (const . pure $ fromString argPlain)
                (typeError err)
              exitFailure

  scope' <-
    for scope $ \(name, expr) -> do
      let !value = evalExpr (defaultEvalEnv "." mempty) $ locatedValue expr
      pure (name, value)

  LazyByteString.putStrLn $
    evalTemplate (defaultEvalEnv file deps){eeScope = Map.fromList scope' <> defaultCtx} template
