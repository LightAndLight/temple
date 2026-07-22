{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Temple
  ( -- * Syntax
    Template (..)
  , Part (..)
  , Expr (..)
  , Field (..)
  , Branch (..)
  , Pattern (..)
  , Located (..)

    -- * Parsing
  , templateParser
  , partParser
  , exprParser
  , fieldParser
  , identParser
  , branchParser
  , patternParser

    -- ** Combinators
  , symbolic

    -- * Typing
  , Type (..)
  , Kind (..)
  , TypeError (..)

    -- ** Type inference
  , InferT
  , InferState (..)
  , emptyInferState
  , Requirement (..)
  , getRequirements
  , InferEnv (..)
  , emptyInferEnv
  , defaultScope
  , runInferT
  , checkTemplate
  , inferExpr
  , checkExpr
  , checkPart
  , zonkDefault
  , zonkNoDefault

    -- * Evaluating
  , evalTemplate
  , evalPart
  , evalExpr
  , EvalEnv (..)
  , defaultEvalEnv
  , defaultCtx

    -- ** Values
  , Value (..)
  , valueBool
  , valueString
  , valueRecord
  , valueStream

    -- ** Builtins
  , builtins
  )
where

import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Exception (catch, throwIO)
import Control.Monad (guard, unless, when)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (get, gets, modify, put)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Char as Char
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Tuple as Tuple
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import Text.Sage (Parser, char, notFollowedBy, satisfy, sepBy, skipMany, string, try, (<?>))
import qualified Text.Sage as Sage

data Template
  = TemplateBase
      -- | Template path (relative to working directory)
      !FilePath
      [Part]
  | TemplateChild
      -- | Template path (relative to working directory)
      !FilePath
      -- | Parent template
      !(Located FilePath)
      ![Pragma]
  deriving (Show, Eq)

data Pragma
  = PragmaBlock !(Located Text) ![Part]
  | PragmaWith ![(Located Text, Located Expr)]
  deriving (Show, Eq)

data Part
  = PartText !Text
  | PartExpr !(Located Expr)
  | PartExprStream !(Located Expr)
  | PartInclude
      -- | File to include
      !(Located Text)
  deriving (Show, Eq)

data Located a
  = Located
  { locatedOffset :: !Int
  , locatedValue :: !a
  }
  deriving (Show, Eq, Functor)

data Expr
  = Var !Text
  | String ![Part]
  | MultilineString ![Part]
  | Call !(Located Text) ![Located Expr]
  | Record [(Text, Located Expr)]
  | Field !(Located Expr) !Field
  | Constructor !Text [Located Expr]
  | Match !(Located Expr) ![Branch]
  | IfThenElse !(Located Expr) !(Located Expr) !(Located Expr)
  | Array ![Located Expr]
  | -- | @for <name> in <collection> yield <value>@
    For
      -- | @<name>@
      !Text
      -- | @<collection>@
      !(Located Expr)
      -- | @<value>@
      !(Located Expr)
  deriving (Show, Eq)

data Field
  = FStatic !Text
  | FDynamic !(Located Expr)
  deriving (Show, Eq)

data Branch
  = Branch !(Located Pattern) !(Located Expr)
  deriving (Show, Eq)

data Pattern
  = PConstructor !Text ![Text]
  deriving (Show, Eq)

templateParser :: FilePath -> Parser Template
templateParser path =
  TemplateChild path <$> pragmaExtendsParser <*> many pragmaParser
    <|> TemplateBase path <$> many partParser
  where
    pragmaExtendsParser =
      try (openPragmaParser *> symbol (fromString "extends"))
        *> locatedParser stringLiteralParser
        <* token closePragmaParser

openPragmaParser :: Parser ()
openPragmaParser = void . symbol $ fromString "{%"

closePragmaParser :: Parser ()
closePragmaParser =
  void . string $ fromString "%}"

pragmaParser :: Parser Pragma
pragmaParser =
  between openPragmaParser (token closePragmaParser) $
    ( do
        _ <- symbol $ fromString "block"
        name <- locatedParser identParser <* token closePragmaParser
        template <- many partParser
        openPragmaParser <* symbol (fromString "end") <* symbol (locatedValue name)
        pure $ PragmaBlock name template
    )
      <|> PragmaWith
        <$ symbol (fromString "with")
        <*> commaSep ((,) <$> locatedParser identParser <* symbolic '=' <*> exprParser)

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

between :: Parser left -> Parser right -> Parser a -> Parser a
between l r a = l *> a <* r

token :: Parser a -> Parser a
token p = p <* skipMany (satisfy Char.isSpace)

symbol :: Text -> Parser Text
symbol = token . string

symbolic :: Char -> Parser Char
symbolic = token . char

parens :: Parser a -> Parser a
parens = between (symbolic '(') (symbolic ')')

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbolic ',')

partParser :: Parser Part
partParser =
  PartText . Text.pack
    <$> some
      ( noneOf "\\{}"
          <|> try (char '{' <* notFollowedBy (char '{' <|> char '%'))
          <|> try (char '}' <* notFollowedBy (char '}'))
          <|> (char '\\' *> (char '\\' <|> char '{' <|> char '}'))
      )
    <|> partExprParser
    <|> partIncludeParser

partExprParser :: Parser Part
partExprParser =
  ($)
    <$ symbol (fromString "{{")
    <*> token (PartExprStream <$ symbolic '*' <|> pure PartExpr)
    <*> exprParser
    <* string (fromString "}}")

partIncludeParser :: Parser Part
partIncludeParser =
  PartInclude
    <$ (try (openPragmaParser <* notFollowedBy (symbol $ fromString "end")) <?> "{%")
    <* symbol (fromString "include")
    <*> locatedParser (fmap Text.pack stringLiteralParser)
    <* closePragmaParser

kIf, kThen, kElse, kFor, kIn, kYield, kMatch :: Text
kIf = fromString "if"
kThen = fromString "then"
kElse = fromString "else"
kFor = fromString "for"
kIn = fromString "in"
kYield = fromString "yield"
kMatch = fromString "match"

keywords :: [Text]
keywords =
  [ kIf
  , kThen
  , kElse
  , kFor
  , kIn
  , kYield
  , kMatch
  ]

locatedParser :: Parser a -> Parser (Located a)
locatedParser p = Located <$> Sage.getOffset <*> p

exprParser :: Parser (Located Expr)
exprParser =
  (\offset -> foldl' (\acc item -> Located offset $ Field acc item))
    <$> Sage.getOffset
    <*> atomParser
    <*> many (symbolic '.' *> fieldParser)
    <|> locatedParser recordParser
    <|> locatedParser matchParser
    <|> locatedParser ifThenElseParser
    <|> locatedParser forParser
  where
    recordParser =
      Record
        <$> between
          (symbolic '{')
          (symbolic '}')
          (commaSep $ (,) <$> identParser <* symbolic '=' <*> exprParser)

    matchParser =
      Match <$ symbol kMatch <*> exprParser <*> many branchParser

    ifThenElseParser =
      IfThenElse
        <$ symbol kIf
        <*> exprParser
        <* symbol kThen
        <*> exprParser
        <* symbol kElse
        <*> exprParser

    forParser =
      For <$ symbol kFor <*> identParser <* symbol kIn <*> exprParser <* symbol kYield <*> exprParser

stringLiteralParser :: Parser String
stringLiteralParser =
  token $
    between
      (char '"')
      (char '"')
      ( many $
          noneOf "\\{}\"\n"
            <|> try (char '{' <* notFollowedBy (char '{'))
            <|> try (char '}' <* notFollowedBy (char '}'))
            <|> char '\\' *> (char '\\' <|> char '{' <|> char '}' <|> char '"' <|> ('\n' <$ char 'n'))
      )

atomParser :: Parser (Located Expr)
atomParser =
  locatedParser
    ( (\name -> maybe (Var $ locatedValue name) (Call name))
        <$> locatedParser identParser
        <*> optional (parens $ commaSep exprParser)
        <|> (\name -> Constructor name . fromMaybe [])
          <$> ctorParser
          <*> optional (parens $ commaSep exprParser)
        <|> String <$> stringParser
        <|> MultilineString <$> multilineStringParser
        <|> Array <$> between (symbolic '[') (symbolic ']') (commaSep exprParser)
    )
    <|> parens exprParser
  where
    doubleQuote1 = char '"' <* notFollowedBy (string $ fromString "\"\"")

    stringParser =
      token $
        between
          (try doubleQuote1)
          (char '"')
          ( many $
              fmap
                (PartText . Text.pack)
                ( some $
                    noneOf "\\{}\"\n"
                      <|> try (char '{' <* notFollowedBy (char '{' <|> char '%'))
                      <|> try (char '}' <* notFollowedBy (char '}'))
                      <|> char '\\' *> (char '\\' <|> char '{' <|> char '}' <|> char '"' <|> ('\n' <$ char 'n'))
                )
                <|> partExprParser
                <|> partIncludeParser
          )

    doubleQuote3 = string $ fromString "\"\"\""

    multilineStringParser =
      token . between doubleQuote3 doubleQuote3 $ do
        nl <- optional $ char '\n'
        case nl of
          Nothing -> multilinePartsParser Nothing <|> pure []
          Just{} -> multilineLinesParser

    multilinePartsParser :: Maybe Int -> Parser [Part]
    multilinePartsParser mIndent =
      some
        ( fmap
            (PartText . Text.pack)
            ( (++)
                <$> some
                  ( noneOf "\\{}\"\n"
                      <|> try (char '{' <* notFollowedBy (char '{' <|> char '%'))
                      <|> try (char '}' <* notFollowedBy (char '}'))
                      <|> try doubleQuote1
                      <|> (char '\\' *> (char '\\' <|> char '{' <|> char '}' <|> char '"' <|> ('\n' <$ char 'n')))
                  )
                <*> (fmap pure (char '\n' <* for_ mIndent (optional . indentParser)) <|> pure [])
            )
            <|> partExprParser
            <|> partIncludeParser
            <|> PartText . Text.pack <$> fmap pure (char '\n' <* for_ mIndent (optional . indentParser))
        )

    indentParser total = go total <?> ("indentation (" ++ show total ++ " spaces)")
      where
        go !n
          | n <= 0 = pure ()
          | otherwise =
              char ' ' *> go (n - 1)
                <|> char '\t' *> empty

    multilineLinesParser = do
      indent <-
        Sage.count $
          char ' '
            <|> char '\t' *> empty
      nl <- optional $ char '\n'
      case nl of
        Nothing ->
          (++)
            <$> multilinePartsParser (Just indent)
            <*> fmap concat (many . multilinePartsParser $ Just indent)
        Just{} ->
          (:) (PartText $ fromString "\n")
            <$> multilineLinesParser

fieldParser :: Parser Field
fieldParser =
  FStatic <$> identParser
    <|> FDynamic <$> between (symbolic '{') (symbolic '}') exprParser

branchParser :: Parser Branch
branchParser =
  Branch <$ symbolic '|' <*> locatedParser patternParser <* symbol (fromString "->") <*> exprParser

patternParser :: Parser Pattern
patternParser =
  (\name -> PConstructor name . fromMaybe [])
    <$> ctorParser
    <*> optional (parens $ commaSep identParser)

isIdentContinue :: Char -> Bool
isIdentContinue = (||) <$> Char.isAlphaNum <*> (`elem` "-_")

identParser :: Parser Text
identParser =
  token . try $ do
    ident <-
      fmap Text.pack $
        (:) <$> satisfy isIdentStart <*> many (satisfy isIdentContinue)
    guard . not $ ident `elem` keywords
    pure ident
  where
    isIdentStart = Char.isLower

ctorParser :: Parser Text
ctorParser =
  token $ do
    fmap Text.pack $
      (:) <$> satisfy isCtorStart <*> many (satisfy isIdentContinue)
  where
    isCtorStart = Char.isUpper

data TypeError
  = NotInScope
      -- | Source offset of error
      !Int
  | TypeMismatch
      -- | Source offset of error
      !Int
      -- | Expected
      !Type
      -- | Actual
      !Type
  | UnexpectedFields
      -- | Source offset of error
      !Int
      -- | Actual
      ![(Text, Type)]
  | MissingFields
      -- | Source offset of error
      !Int
      -- | Expected
      ![(Text, Type)]
  | UnexpectedConstructors
      -- | Source offset of error
      !Int
      -- | Actual
      ![(Text, [Type])]
  | MissingConstructors
      -- | Source offset of error
      !Int
      -- | Expected
      ![(Text, [Type])]
  | ArityMismatch
      -- | Source offset of error
      !Int
      -- | Expected
      !Int
      -- | Actual
      !Int
  | KindMismatch
      -- | Source offset of error
      !Int
      -- | Expected
      !Kind
      -- | Actual
      !Kind
  | NotRequirement
      -- | Source offset of error
      !Int
      -- | Offending identifier
      !Text
  | BlockBadRequirementType
      -- | Source offset of error
      !Int
      -- | Actual requirement type
      !Type
  | RequirementAlreadySatisfied
      -- | Source offset of error
      !Int
  | FileNotFound
      -- | Source offset filepath
      !Int
  | ParentParseError
      -- | Source offset of parent filepath
      !Int
      -- | File being parsed
      !FilePath
      Sage.ParseError
  | ParentTypeError
      -- | Source offset of error (in child)
      !Int
      -- | Path of parent file
      !FilePath
      TypeError
  | IncludeParseError
      -- | Source offset of include filepath
      !Int
      -- | File being parsed
      !FilePath
      Sage.ParseError
  | IncludeTypeError
      -- | Source offset of include filepath
      !Int
      -- | File being type checked
      !FilePath
      TypeError

data Type
  = TMeta !Int
  | TVar !Text
  | TBool
  | TString
  | TFn ![Type] Type
  | TStream !Type
  | TRecord !Type
  | TRecordField
      -- | Field name
      !Text
      -- | Field type
      !Type
      -- | Rest
      !Type
  | TSum !Type
  | TSumConstructor
      -- | Constructor name
      !Text
      -- | Constructor arguments
      ![Type]
      -- | Rest
      !Type
  | TRowEnd
  deriving (Show)

subst :: Map Text Type -> Type -> Type
subst sub ty@(TVar v) =
  case Map.lookup v sub of
    Nothing -> ty
    Just ty' -> ty'
subst _ ty@TMeta{} = ty
subst _ TBool = TBool
subst _ TString = TString
subst sub (TFn args ret) = TFn (fmap (subst sub) args) (subst sub ret)
subst sub (TStream item) = TStream (subst sub item)
subst sub (TRecord fields) = TRecord (subst sub fields)
subst sub (TRecordField name ty rest) = TRecordField name (subst sub ty) (subst sub rest)
subst sub (TSum ctors) = TSum (subst sub ctors)
subst sub (TSumConstructor name tys rest) = TSumConstructor name (fmap (subst sub) tys) (subst sub rest)
subst _ TRowEnd = TRowEnd

newtype InferT m a = InferT (ReaderT InferEnv (StateT InferState (ExceptT TypeError m)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader InferEnv, MonadError TypeError)

runInferT :: Monad m => InferEnv -> InferState -> InferT m a -> m (Either TypeError (InferState, a))
runInferT e s (InferT ma) = runExceptT . fmap Tuple.swap . flip runStateT s . flip runReaderT e $ ma

data InferEnv
  = InferEnv
  { ieCurrentFile :: !FilePath
  , ieScope :: !(Map Text TypeScheme)
  }

data TypeScheme = Forall ![Text] Type

emptyInferEnv ::
  -- | Current file
  FilePath ->
  InferEnv
emptyInferEnv currentFile = InferEnv{ieCurrentFile = currentFile, ieScope = mempty}

builtins :: Map Text (Value, TypeScheme)
builtins =
  let
    strip =
      ByteString.Lazy.Char8.dropWhileEnd Char.isSpace
        . ByteString.Lazy.Char8.dropWhile Char.isSpace
  in
    Map.fromList
      [
        ( fromString "strip"
        ,
          ( VFn . Fn $ \case [s] -> VString . strip $ valueString s; _ -> undefined
          , Forall [] $ TFn [TString] TString
          )
        )
      ,
        ( fromString "is-empty"
        ,
          ( VFn . Fn $ \case [s] -> if null $ valueStream s then VTrue else VFalse; _ -> undefined
          , Forall [fromString "a"] $ TFn [TStream $ TVar (fromString "a")] TBool
          )
        )
      ]

-- | @defaultScope = fmap snd 'builtins'@
defaultScope :: Map Text TypeScheme
defaultScope = fmap snd builtins

data InferState
  = InferState
  { isMetavars :: !(IntMap Metavar)
  , isRequirements :: ![Requirement]
  , isDependencies :: !(Map FilePath Template)
  }

data Metavar
  = Metavar
  { metaKind :: Kind
  , metaSolution :: Maybe Type
  }

data Requirement
  = Requirement
  { reqName :: !Text
  , reqType :: !Type
  , reqLocations :: NonEmpty (FilePath, Int)
  -- ^ Places where the binding is introduced.
  , reqSatisfied :: !Bool
  }

data Kind = KType | KRow
  deriving (Show, Eq)

emptyInferState :: InferState
emptyInferState = InferState{isMetavars = mempty, isRequirements = mempty, isDependencies = mempty}

getRequirements :: Monad m => InferT m [Requirement]
getRequirements = InferT $ gets isRequirements

addDependency :: Monad m => FilePath -> Template -> InferT m ()
addDependency path template = InferT $ modify $ \s -> s{isDependencies = Map.insert path template $ isDependencies s}

checkTemplate :: MonadIO m => Template -> InferT m ()
checkTemplate (TemplateBase _file parts) = traverse_ checkPart parts
checkTemplate (TemplateChild file parent pragmas) = do
  let parentPath = takeDirectory file </> locatedValue parent
  mContent <-
    liftIO $
      fmap Just (ByteString.readFile parentPath)
        `catch` \err -> if isDoesNotExistError err then pure Nothing else throwIO err
  case mContent of
    Nothing -> throwError $ FileNotFound (locatedOffset parent)
    Just content -> do
      parentTemplate <-
        case Sage.parse (templateParser file <* Sage.eof) content of
          Left err -> throwError $ ParentParseError (locatedOffset parent) parentPath err
          Right x -> pure x
      local (\env -> env{ieCurrentFile = parentPath}) $
        checkTemplate parentTemplate
          `catchError` (throwError . ParentTypeError (locatedOffset parent) parentPath)
      traverse_ checkPragma pragmas
      addDependency parentPath parentTemplate

checkPragma :: MonadIO m => Pragma -> InferT m ()
checkPragma (PragmaBlock name parts) = do
  mReq <- lookupRequirement $ locatedValue name
  case mReq of
    Nothing ->
      throwError $ NotRequirement (locatedOffset name) (locatedValue name)
    Just req -> do
      if reqSatisfied req
        then
          throwError $ RequirementAlreadySatisfied (locatedOffset name)
        else do
          reqTy <- zonkDefault $ reqType req
          case reqTy of
            TString ->
              satisfyRequirement $ locatedValue name
            _ ->
              throwError $ BlockBadRequirementType (locatedOffset name) reqTy
  traverse_ checkPart parts
checkPragma (PragmaWith vars) =
  for_ vars $ \(name, value) -> do
    mReq <- lookupRequirement $ locatedValue name
    case mReq of
      Nothing ->
        throwError $ NotRequirement (locatedOffset name) (locatedValue name)
      Just req ->
        if reqSatisfied req
          then
            throwError $ RequirementAlreadySatisfied (locatedOffset name)
          else do
            checkExpr value $ reqType req
            satisfyRequirement $ locatedValue name

lookupRequirement :: Monad m => Text -> InferT m (Maybe Requirement)
lookupRequirement name = InferT $ gets (find ((name ==) . reqName) . isRequirements)

satisfyRequirement :: Monad m => Text -> InferT m ()
satisfyRequirement name =
  InferT . modify $ \s ->
    s{isRequirements = modifyRequirement name (\r -> r{reqSatisfied = True}) $ isRequirements s}

modifyRequirement :: Text -> (Requirement -> Requirement) -> [Requirement] -> [Requirement]
modifyRequirement _name _f [] = []
modifyRequirement name f (r : rs) = if reqName r == name then f r : rs else r : modifyRequirement name f rs

checkPart :: MonadIO m => Part -> InferT m ()
checkPart PartText{} = pure ()
checkPart (PartExpr e) = checkExpr e TString
checkPart (PartExprStream e) = checkExpr e (TStream TString)
checkPart (PartInclude target) = do
  currentFile <- asks ieCurrentFile

  let includePath = takeDirectory currentFile </> Text.unpack (locatedValue target)
  mContent <-
    liftIO $
      fmap Just (ByteString.readFile includePath)
        `catch` \err -> if isDoesNotExistError err then pure Nothing else throwIO err
  case mContent of
    Nothing -> throwError $ FileNotFound (locatedOffset target)
    Just content -> do
      includeTemplate <-
        case Sage.parse (templateParser currentFile <* Sage.eof) content of
          Left err -> throwError $ IncludeParseError (locatedOffset target) includePath err
          Right x -> pure x
      local (\env -> env{ieCurrentFile = includePath}) $
        checkTemplate includeTemplate
          `catchError` (throwError . IncludeTypeError (locatedOffset target) includePath)
      addDependency includePath includeTemplate

instantiateTypeScheme :: Monad m => TypeScheme -> InferT m Type
instantiateTypeScheme (Forall vars ty) = do
  sub <- Map.fromList <$> traverse (\var -> (,) var <$> metavar KType) vars
  pure $ subst sub ty

checkExpr ::
  MonadIO m =>
  Located Expr ->
  Type ->
  InferT m ()
checkExpr (Located offset (Var v)) t = do
  mTy <- asks (Map.lookup v . ieScope)
  ty <-
    case mTy of
      Just ty -> instantiateTypeScheme ty
      Nothing -> require offset v
  unify offset t ty
checkExpr (Located offset (String parts)) t = do
  unify offset t TString
  traverse_ checkPart parts
checkExpr (Located offset (MultilineString parts)) t = do
  unify offset t TString
  traverse_ checkPart parts
checkExpr (Located offset (Call name args)) t = do
  argTys <- traverse (const $ metavar KType) args
  mTy <- asks (Map.lookup (locatedValue name) . ieScope)
  ty <-
    case mTy of
      Nothing -> throwError $ NotInScope (locatedOffset name)
      Just ty -> instantiateTypeScheme ty
  unify offset (TFn argTys t) ty
  for_ (zip args argTys) $ \(arg, argTy) -> do
    checkExpr arg argTy
checkExpr (Located offset (Record fields)) t = do
  fieldsWithTys <- traverse (\(name, e) -> (,,) name e <$> metavar KType) fields
  let actual = TRecord $ foldr (\(name, _e, ty) -> TRecordField name ty) TRowEnd fieldsWithTys
  unify offset t actual
  traverse_ (\(_name, e, ty) -> checkExpr e ty) fieldsWithTys
checkExpr (Located _offset (Field e f)) t =
  case f of
    FDynamic _f' ->
      error "TODO: dynamic record fields"
    FStatic f' -> do
      rest <- metavar KRow
      checkExpr e (TRecord $ TRecordField f' t rest)
checkExpr (Located offset (Constructor name args)) t = do
  argTys <- traverse (const $ metavar KType) args
  rest <- metavar KRow
  unify offset t (TSum $ TSumConstructor name argTys rest)
  for_ (zip args argTys) $ \(arg, argTy) ->
    checkExpr arg argTy
checkExpr (Located _offset (Match e bs)) t = do
  eTy <- inferExpr e
  for_ bs $ \(Branch p body) -> do
    bindings <- checkPattern p eTy
    local (\env -> env{ieScope = fmap (Forall []) bindings <> ieScope env}) $ checkExpr body t
checkExpr (Located _offset (IfThenElse cond th el)) t = do
  checkExpr cond TBool
  checkExpr th t
  checkExpr el t
checkExpr (Located offset (Array items)) t = do
  valueTy <- metavar KType
  unify offset t (TStream valueTy)
  for_ items $ \item -> do
    checkExpr item valueTy
checkExpr (Located offset (For name items value)) t = do
  valueTy <- metavar KType
  unify offset t (TStream valueTy)
  itemTy <- metavar KType
  checkExpr items (TStream itemTy)
  local (\env -> env{ieScope = Map.insert name (Forall [] itemTy) $ ieScope env}) $
    checkExpr value valueTy

inferExpr :: MonadIO m => Located Expr -> InferT m Type
inferExpr e = do
  t <- metavar KType
  t <$ checkExpr e t

checkPattern ::
  Monad m =>
  Located Pattern ->
  Type ->
  InferT m (Map Text Type)
checkPattern (Located offset (PConstructor name args)) t = do
  argTys <- traverse (\arg -> (,) arg <$> metavar KType) args
  rest <- metavar KRow
  unify offset t (TSum $ TSumConstructor name (fmap snd argTys) rest)
  pure $ Map.fromList argTys

require ::
  Monad m =>
  -- | Location of variable
  Int ->
  Text ->
  InferT m Type
require offset name = do
  currentFile <- asks ieCurrentFile
  mReq <- lookupRequirement name
  case mReq of
    Nothing -> do
      ty <- metavar KType
      InferT . modify $ \s ->
        s
          { isRequirements =
              isRequirements s
                ++ [ Requirement
                       { reqName = name
                       , reqType = ty
                       , reqLocations = pure (currentFile, offset)
                       , reqSatisfied = False
                       }
                   ]
          }
      pure ty
    Just req -> do
      InferT . modify $ \s ->
        s
          { isRequirements =
              updateRequirement
                req{reqLocations = reqLocations req <> pure (currentFile, offset)}
                (isRequirements s)
          }
      pure $ reqType req

updateRequirement :: Requirement -> [Requirement] -> [Requirement]
updateRequirement _new [] = []
updateRequirement new (req : reqs)
  | reqName new == reqName req = new : reqs
  | otherwise = req : updateRequirement new reqs

metavar :: Monad m => Kind -> InferT m Type
metavar kind = InferT $ do
  s <- get
  let metavars = isMetavars s
  let n = IntMap.size metavars
  put s{isMetavars = IntMap.insert n (Metavar kind Nothing) metavars}
  pure $ TMeta n

unify ::
  Monad m =>
  {-| Source offset that generated the constraint.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Expected
  Type ->
  -- | Actual
  Type ->
  InferT m ()
unify offset (TMeta m) ty = solveL offset m ty
unify offset ty (TMeta m) = solveR offset ty m
unify offset (TVar v) ty =
  case ty of
    TVar v' | v == v' -> pure ()
    _ -> do
      ty' <- zonk False ty
      throwError $ TypeMismatch offset (TVar v) ty'
unify offset TBool ty =
  case ty of
    TBool -> pure ()
    _ -> do
      ty' <- zonk False ty
      throwError $ TypeMismatch offset TBool ty'
unify offset TString ty =
  case ty of
    TString -> pure ()
    _ -> do
      ty' <- zonk False ty
      throwError $ TypeMismatch offset TString ty'
unify offset (TFn args retTy) ty =
  case ty of
    TFn args' retTy' -> do
      unless (length args == length args') . throwError $
        ArityMismatch offset (length args) (length args')
      traverse_ (uncurry $ unify offset) (zip args args')
      unify offset retTy retTy'
    _ -> do
      args' <- traverse zonkNoDefault args
      retTy' <- zonkNoDefault retTy
      ty' <- zonkNoDefault ty
      throwError $ TypeMismatch offset (TFn args' retTy') ty'
unify offset (TStream a) ty =
  case ty of
    TStream a' -> unify offset a a'
    _ -> do
      a' <- zonkNoDefault a
      ty' <- zonkNoDefault ty
      throwError $ TypeMismatch offset (TStream a') ty'
unify offset (TRecord fields) ty =
  case ty of
    TRecord fields' -> do
      (fields1, rest) <- getRecordFields fields
      (fields1', rest') <- getRecordFields fields'
      (unmatched, unmatched') <- unifyFields offset fields1 fields1'
      final <- metavar KRow
      solveRecordTailL offset rest unmatched' final
      solveRecordTailR offset unmatched rest' final
    _ -> do
      fields' <- zonkNoDefault fields
      ty' <- zonkNoDefault ty
      throwError $ TypeMismatch offset (TRecord fields') ty'
unify offset (TSum ctors) ty =
  case ty of
    TSum ctors' -> do
      (ctors1, rest) <- getSumConstructors ctors
      (ctors1', rest') <- getSumConstructors ctors'
      (unmatched, unmatched') <- unifyConstructors offset ctors1 ctors1'
      final <- metavar KRow
      solveSumTailL offset rest unmatched' final
      solveSumTailR offset unmatched rest' final
    _ -> do
      fields' <- zonkNoDefault ctors
      ty' <- zonkNoDefault ty
      throwError $ TypeMismatch offset (TSum fields') ty'
unify _offset TRecordField{} _ = error "don't unify TRecordField"
unify _offset TSumConstructor{} _ = error "don't unify TRecordField"
unify _offset TRowEnd{} _ = error "don't unify TRowEnd"

unifyFields ::
  Monad m =>
  {-| Source offset that generated the constraint.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  [(Text, Type)] ->
  [(Text, Type)] ->
  InferT m ([(Text, Type)], [(Text, Type)])
unifyFields offset expected actual = do
  let !remainingExpected = expected' `Map.difference` actual'
  let !remainingActual = actual' `Map.difference` expected'
  for_ expected $ \(name, ty) ->
    for_ (Map.lookup name actual') $ \ty' -> do
      unify offset ty ty'
  pure (Map.toList remainingExpected, Map.toList remainingActual)
  where
    expected' = Map.fromList expected
    actual' = Map.fromList actual

unifyConstructors ::
  Monad m =>
  {-| Source offset that generated the constraint.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  [(Text, [Type])] ->
  [(Text, [Type])] ->
  InferT m ([(Text, [Type])], [(Text, [Type])])
unifyConstructors offset expected actual = do
  let !remainingExpected = expected' `Map.difference` actual'
  let !remainingActual = actual' `Map.difference` expected'
  for_ expected $ \(name, tys) -> do
    for_ (Map.lookup name actual') $ \tys' -> do
      when (length tys /= length tys') . throwError $
        ArityMismatch offset (length tys) (length tys')
      traverse_ (uncurry $ unify offset) $ zip tys tys'
  pure (Map.toList remainingExpected, Map.toList remainingActual)
  where
    expected' = Map.fromList expected
    actual' = Map.fromList actual

getRecordFields :: Monad m => Type -> InferT m ([(Text, Type)], Maybe Int)
getRecordFields (TRecordField name ty rest) = do
  (fields, end) <- getRecordFields rest
  pure ((name, ty) : fields, end)
getRecordFields TRowEnd =
  pure ([], Nothing)
getRecordFields (TMeta v) = do
  mMeta <- InferT . gets $ IntMap.lookup v . isMetavars
  case mMeta of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta -> maybe (pure ([], Just v)) getRecordFields $ metaSolution meta
getRecordFields ty =
  error $ "not record field: " ++ show ty

getSumConstructors :: Monad m => Type -> InferT m ([(Text, [Type])], Maybe Int)
getSumConstructors (TSumConstructor name tys rest) = do
  (fields, end) <- getSumConstructors rest
  pure ((name, tys) : fields, end)
getSumConstructors TRowEnd =
  pure ([], Nothing)
getSumConstructors (TMeta v) = do
  mMeta <- InferT . gets $ IntMap.lookup v . isMetavars
  case mMeta of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta -> maybe (pure ([], Just v)) getSumConstructors $ metaSolution meta
getSumConstructors ty =
  error $ "not sum constructor: " ++ show ty

kindOf :: Monad m => Type -> InferT m Kind
kindOf (TMeta v) = do
  mMeta <- InferT $ gets (IntMap.lookup v . isMetavars)
  case mMeta of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta -> pure $ metaKind meta
kindOf TVar{} = pure KType
kindOf TBool = pure KType
kindOf TString = pure KType
kindOf TFn{} = pure KType
kindOf TStream{} = pure KType
kindOf TRecord{} = pure KType
kindOf TRecordField{} = pure KRow
kindOf TSum{} = pure KType
kindOf TSumConstructor{} = pure KRow
kindOf TRowEnd = pure KRow

solveL ::
  Monad m =>
  {-| Source offset that generated the solution.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Expected
  Int ->
  -- | Actual
  Type ->
  InferT m ()
solveL offset v ty' = do
  mMeta <- InferT $ gets (IntMap.lookup v . isMetavars)
  case mMeta of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta -> do
      let expectedKind = metaKind meta
      actualKind <- kindOf ty'
      unless (expectedKind == actualKind) . throwError $
        KindMismatch offset expectedKind actualKind
      case metaSolution meta of
        Nothing -> InferT . modify $ \s -> s{isMetavars = IntMap.insert v meta{metaSolution = Just ty'} (isMetavars s)}
        Just ty -> unify offset ty ty'

solveR ::
  Monad m =>
  {-| Source offset that generated the solution.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Expected
  Type ->
  -- | Actual
  Int ->
  InferT m ()
solveR offset ty v = do
  mMeta' <- InferT $ gets (IntMap.lookup v . isMetavars)
  case mMeta' of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta' -> do
      expectedKind <- kindOf ty
      let actualKind = metaKind meta'
      unless (expectedKind == actualKind) . throwError $
        KindMismatch offset expectedKind actualKind
      case metaSolution meta' of
        Nothing -> InferT . modify $ \s -> s{isMetavars = IntMap.insert v meta'{metaSolution = Just ty} (isMetavars s)}
        Just ty' -> unify offset ty ty'

solveRecordTailL ::
  Monad m =>
  {-| Source offset that generated the solution.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Optional metavariable for the "expected" record's tail.
  Maybe Int ->
  -- | Remaining "actual" fields
  [(Text, Type)] ->
  -- | Shared tail of the unified records
  Type ->
  InferT m ()
solveRecordTailL offset rest unmatched' final =
  case rest of
    Nothing ->
      unless (null unmatched') . throwError $
        UnexpectedFields offset unmatched'
    Just v ->
      solveL offset v (foldr (uncurry TRecordField) final unmatched')

solveRecordTailR ::
  Monad m =>
  {-| Source offset that generated the solution.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Remaining "expected" fields
  [(Text, Type)] ->
  -- | Optional metavariable for the "actual" record's tail.
  Maybe Int ->
  -- | Shared tail of the unified records
  Type ->
  InferT m ()
solveRecordTailR offset unmatched rest' final = do
  case rest' of
    Nothing ->
      unless (null unmatched) . throwError $
        MissingFields offset unmatched
    Just v' ->
      solveR offset (foldr (uncurry TRecordField) final unmatched) v'

solveSumTailL ::
  Monad m =>
  {-| Source offset that generated the solution.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Optional metavariable for the "expected" sum's tail.
  Maybe Int ->
  -- | Remaining "actual" constructors
  [(Text, [Type])] ->
  -- | Shared tail of the unified sums
  Type ->
  InferT m ()
solveSumTailL offset rest unmatched' final =
  case rest of
    Nothing ->
      unless (null unmatched') . throwError $
        UnexpectedConstructors offset unmatched'
    Just v ->
      solveL offset v (foldr (uncurry TSumConstructor) final unmatched')

solveSumTailR ::
  Monad m =>
  {-| Source offset that generated the solution.

  If unification fails with a type error, this source offset should inform
  the user of where the type error occurred.
  -}
  Int ->
  -- | Remaining "expected" constructors
  [(Text, [Type])] ->
  -- | Optional metavariable for the "actual" sum's tail.
  Maybe Int ->
  -- | Shared tail of the unified sums
  Type ->
  InferT m ()
solveSumTailR offset unmatched rest' final = do
  case rest' of
    Nothing ->
      unless (null unmatched) . throwError $
        MissingConstructors offset unmatched
    Just v' ->
      solveR offset (foldr (uncurry TSumConstructor) final unmatched) v'

zonkNoDefault ::
  Monad m =>
  Type ->
  InferT m Type
zonkNoDefault = zonk False

zonkDefault ::
  Monad m =>
  Type ->
  InferT m Type
zonkDefault = zonk True

zonk ::
  Monad m =>
  -- | Replace unsolved metas with default types
  Bool ->
  Type ->
  InferT m Type
zonk def (TMeta v) = do
  mmTy <- InferT $ gets (IntMap.lookup v . isMetavars)
  case mmTy of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta -> do
      let
        defTy =
          case metaKind meta of
            KType -> TMeta v
            KRow
              | def -> TRowEnd
              | otherwise -> TMeta v
      maybe (pure defTy) (zonk def) (metaSolution meta)
zonk _def (TVar v) = pure (TVar v)
zonk _def TBool = pure TBool
zonk _def TString = pure TString
zonk def (TFn args retTy) = TFn <$> traverse (zonk def) args <*> zonk def retTy
zonk def (TStream ty) = TStream <$> zonk def ty
zonk def (TRecord fields) = TRecord <$> zonk def fields
zonk def (TRecordField name ty rest) = TRecordField name <$> zonk def ty <*> zonk def rest
zonk def (TSum ctors) = TSum <$> zonk def ctors
zonk def (TSumConstructor name tys rest) = TSumConstructor name <$> traverse (zonk def) tys <*> zonk def rest
zonk _def TRowEnd = pure TRowEnd

data Value
  = VTrue
  | VFalse
  | VString LazyByteString
  | VFn Fn
  | VRecord !(Map Text Value)
  | VConstructor !Text ![Value]
  | VStream [Value]
  deriving (Show)

newtype Fn = Fn ([Value] -> Value)

instance Show Fn where
  show _ = "<function>"

valueBool :: Value -> Bool
valueBool VTrue = True
valueBool VFalse = False
valueBool v = error $ "expected bool, got " ++ show v

valueString :: Value -> LazyByteString
valueString (VString s) = s
valueString v = error $ "expected string, got " ++ show v

valueRecord :: Value -> Map Text Value
valueRecord (VRecord r) = r
valueRecord v = error $ "expected record, got " ++ show v

valueStream :: Value -> [Value]
valueStream (VStream s) = s
valueStream v = error $ "expected stream, got " ++ show v

valueFn :: Value -> [Value] -> Value
valueFn (VFn (Fn f)) = f
valueFn v = error $ "expected function, got " ++ show v

data EvalEnv
  = EvalEnv
  { eeCurrentFile :: !FilePath
  , eeDependencies :: !(Map FilePath Template)
  , eeScope :: !(Map Text Value)
  }

defaultEvalEnv ::
  FilePath ->
  Map FilePath Template ->
  EvalEnv
defaultEvalEnv currentFile dependencies =
  EvalEnv
    { eeCurrentFile = currentFile
    , eeDependencies = dependencies
    , eeScope = defaultCtx
    }

-- | @defaultCtx = fmap fst 'builtins'@
defaultCtx :: Map Text Value
defaultCtx = fmap fst builtins

evalTemplate :: EvalEnv -> Template -> LazyByteString
evalTemplate env (TemplateBase _file parts) =
  foldMap (evalPart env) parts
evalTemplate env (TemplateChild file parent pragmas) =
  let
    parentPath = takeDirectory file </> locatedValue parent
    template =
      fromMaybe (error $ "missing dependency: " ++ parentPath) $
        Map.lookup parentPath (eeDependencies env)
    !ctx' = Map.fromList $ foldMap (evalPragma env) pragmas
  in
    evalTemplate env{eeScope = ctx' <> eeScope env} template

evalPragma :: EvalEnv -> Pragma -> [(Text, Value)]
evalPragma env (PragmaBlock name parts) =
  let
    !value = VString $! foldMap (evalPart env) parts
  in
    [(locatedValue name, value)]
evalPragma env (PragmaWith vars) =
  [(locatedValue name, value) | (name, expr) <- vars, let !value = evalExpr env (locatedValue expr)]

evalPart :: EvalEnv -> Part -> LazyByteString
evalPart _env (PartText t) =
  Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict t
evalPart env (PartExpr e) =
  valueString $ evalExpr env (locatedValue e)
evalPart env (PartExprStream e) =
  foldMap valueString . valueStream $ evalExpr env (locatedValue e)
evalPart env (PartInclude file) =
  let
    includePath = takeDirectory (eeCurrentFile env) </> Text.unpack (locatedValue file)
    template =
      fromMaybe (error $ "missing dependency: " ++ includePath) $
        Map.lookup includePath $
          eeDependencies env
  in
    evalTemplate env{eeCurrentFile = includePath} template

evalExpr :: EvalEnv -> Expr -> Value
evalExpr env (Var v) =
  case Map.lookup v $ eeScope env of
    Nothing -> error $ "not in scope: " ++ Text.unpack v
    Just value -> value
evalExpr env (String parts) =
  VString $! foldMap (evalPart env) parts
evalExpr env (MultilineString parts) =
  VString $! foldMap (evalPart env) parts
evalExpr env (Call name args) =
  let
    !f = valueFn $ evalExpr env (Var $ locatedValue name)
    !args' = fmap (evalExpr env . locatedValue) args
  in
    f args'
evalExpr env (Record fields) =
  VRecord $! Map.fromList ((fmap . fmap) (evalExpr env . locatedValue) fields)
evalExpr env (Field expr field) =
  let
    record = valueRecord $ evalExpr env (locatedValue expr)
    field' =
      case field of
        FStatic f ->
          f
        FDynamic e ->
          Text.Encoding.decodeUtf8
            . LazyByteString.toStrict
            . valueString
            $ evalExpr env (locatedValue e)
  in
    case Map.lookup field' record of
      Nothing ->
        error $
          "field "
            ++ Text.unpack field'
            ++ " not in {"
            ++ intercalate ", " (fmap Text.unpack . Set.toAscList $ Map.keysSet record)
            ++ "}"
      Just value ->
        value
evalExpr env (Constructor name args) =
  let
    !args' = fmap (evalExpr env . locatedValue) args
  in
    VConstructor name args'
evalExpr env (Match e bs) =
  let
    v = evalExpr env (locatedValue e)
    (bindings, body) =
      foldr
        ( \(Branch pattern body') rest ->
            case match (locatedValue pattern) v of
              Nothing -> rest
              Just bindings' -> (bindings', body')
        )
        (error "pattern match failure")
        bs
  in
    evalExpr env{eeScope = bindings <> eeScope env} (locatedValue body)
evalExpr env (IfThenElse cond t e) =
  if valueBool $ evalExpr env (locatedValue cond)
    then evalExpr env (locatedValue t)
    else evalExpr env (locatedValue e)
evalExpr env (Array items) =
  VStream [evalExpr env (locatedValue item) | item <- items]
evalExpr env (For name xs yield) =
  let
    xs' = valueStream $ evalExpr env (locatedValue xs)
  in
    VStream [evalExpr env{eeScope = Map.insert name x' $ eeScope env} (locatedValue yield) | x' <- xs']

match :: Pattern -> Value -> Maybe (Map Text Value)
match (PConstructor name args) v =
  case v of
    VConstructor name' args'
      | name == name' ->
          if length args == length args'
            then Just $ Map.fromList (zip args args')
            else
              error $
                Text.unpack name
                  ++ " requires "
                  ++ show (length args)
                  ++ " arguments, got "
                  ++ show (length args')
      | otherwise ->
          Nothing
    _ ->
      error $ "match expected a constructor, got " ++ show v
