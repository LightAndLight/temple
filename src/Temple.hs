{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Temple
  ( -- * Syntax
    Template (..)
  , TemplateRef (..)
  , Part (..)
  , Pragma (..)
  , LExpr
  , Expr (..)
  , Field (..)
  , Branch (..)
  , Pattern (..)
  , Located (..)
  , renderType
  , renderTypeScheme
  , renderKind

    -- * Parsing
  , parse
  , Offset (..)
  , Sage.ParseError (..)
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
  , TypeScheme (..)
  , Type (..)
  , Kind (..)
  , TypeError (..)
  , typeErrorLoc

    -- ** Type inference
  , Binding (..)
  , inferBindings
  , InferT
  , InferState (..)
  , emptyInferState
  , Requirement (..)
  , getRequirements
  , InferEnv (..)
  , emptyInferEnv
  , defaultInferEnv
  , defaultScope
  , runInferT
  , checkTemplate
  , inferExpr
  , checkExpr
  , checkPart
  , checkPartInclude
  , checkPartIncludeDisabled
  , generaliseType
  , instantiateTypeScheme
  , metavar
  , unify
  , UnifyContext (..)
  , unifyWithContext
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
  , Fn (..)
  , valueBool
  , valueString
  , valueRecord
  , valueStream

    -- ** Builtins
  , builtins
  )
where

import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Monad (guard, unless, when)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (get, gets, modify, put)
import Control.Monad.Trans (MonadTrans (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Char as Char
import Data.Foldable (foldlM, for_, traverse_)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find, intercalate, nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import qualified Data.Text.Read as Text.Read
import qualified Data.Tuple as Tuple
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Text.Sage (Parser, char, notFollowedBy, satisfy, sepBy, skipMany, string, try, (<?>))
import qualified Text.Sage as Sage

{-| A reference to another template.

The format and meaning of a template reference is decided by the application.
For example, the @temple@ CLI treats them as relative file paths, but other
applications may use them as web URLs or datatabase keys.
-}
newtype TemplateRef = TemplateRef String
  deriving (Show, Eq, Ord)

data Template loc
  = TemplateBase
      [Part loc]
  | TemplateChild
      -- | Parent template
      !(Located loc TemplateRef)
      ![Pragma loc]
  deriving (Show, Eq)

data Pragma loc
  = PragmaBlock !(Located loc Text) ![Part loc]
  | PragmaWith ![(Located loc Text, LExpr loc)]
  deriving (Show, Eq)

data Part loc
  = PartText !Text
  | PartExpr !(LExpr loc)
  | PartExprStream !(LExpr loc)
  | PartInclude
      -- | Template to include
      !(Located loc TemplateRef)
      -- | Optional parameter bindings (@with name1 = expr1, name2 = expr2, ..., nameN = exprN@)
      !(Maybe [(Located loc Text, LExpr loc)])
  deriving (Show, Eq)

data Located loc a
  = Located
  { locatedLoc :: !loc
  , locatedVal :: !a
  }
  deriving (Show, Eq, Functor)

type LExpr loc = Located loc (Expr loc)

data Expr loc
  = Var !Text
  | Bool !Bool
  | String ![Part loc]
  | MultilineString ![Part loc]
  | Call !(Located loc Text) ![LExpr loc]
  | Record [(Text, LExpr loc)]
  | Field !(LExpr loc) !(Field loc)
  | Constructor !Text [LExpr loc]
  | Match !(LExpr loc) ![Branch loc]
  | IfThenElse !(LExpr loc) !(LExpr loc) !(LExpr loc)
  | Array ![LExpr loc]
  | -- | @for <name> in <collection> yield <value>@
    For
      -- | @<name>@
      !Text
      -- | @<collection>@
      !(LExpr loc)
      -- | @<value>@
      !(LExpr loc)
  deriving (Show, Eq)

data Field loc
  = FStatic !Text
  | FDynamic !(LExpr loc)
  deriving (Show, Eq)

data Branch loc
  = Branch !(Located loc Pattern) !(LExpr loc)
  deriving (Show, Eq)

data Pattern
  = PConstructor !Text ![Text]
  deriving (Show, Eq)

newtype Offset = Offset {getOffset :: Int}
  deriving (Show, Eq)

parse ::
  -- | Input to parse
  ByteString ->
  Either Sage.ParseError (Template Offset)
parse = Sage.parse templateParser

templateParser :: Parser (Template Offset)
templateParser =
  TemplateChild <$> pragmaExtendsParser <*> many pragmaParser
    <|> TemplateBase <$> many partParser
  where
    pragmaExtendsParser =
      try (openPragmaParser *> symbol (fromString "extends"))
        *> locatedParser (TemplateRef <$> stringLiteralParser)
        <* token closePragmaParser

openPragmaParser :: Parser ()
openPragmaParser = void . symbol $ fromString "{%"

closePragmaParser :: Parser ()
closePragmaParser =
  void . string $ fromString "%}"

pragmaParser :: Parser (Pragma Offset)
pragmaParser =
  between openPragmaParser (token closePragmaParser) $
    ( do
        _ <- symbol $ fromString "block"
        name <- locatedParser identParser <* token closePragmaParser
        template <- many partParser
        openPragmaParser <* symbol (fromString "end") <* symbol (locatedVal name)
        pure $ PragmaBlock name template
    )
      <|> PragmaWith <$> withParser

withParser :: Parser [(Located Offset Text, LExpr Offset)]
withParser =
  symbol (fromString "with")
    *> commaSep ((,) <$> locatedParser identParser <* symbolic '=' <*> exprParser)

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

partParser :: Parser (Part Offset)
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

partExprParser :: Parser (Part Offset)
partExprParser =
  ($)
    <$ symbol (fromString "{{")
    <*> token (PartExprStream <$ symbolic '*' <|> pure PartExpr)
    <*> exprParser
    <* string (fromString "}}")

partIncludeParser :: Parser (Part Offset)
partIncludeParser =
  PartInclude
    <$ (try (openPragmaParser <* notFollowedBy (symbol $ fromString "end")) <?> "{%")
    <* symbol (fromString "include")
    <*> locatedParser (TemplateRef <$> stringLiteralParser)
    <*> optional withParser
    <* closePragmaParser

kTrue, kFalse, kIf, kThen, kElse, kFor, kIn, kYield, kMatch :: Text
kTrue = fromString "true"
kFalse = fromString "false"
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

locatedParser :: Parser a -> Parser (Located Offset a)
locatedParser p = Located <$> fmap Offset Sage.getOffset <*> p

exprParser :: Parser (LExpr Offset)
exprParser =
  (\offset -> foldl' (\acc item -> Located offset $ Field acc item))
    <$> fmap Offset Sage.getOffset
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

atomParser :: Parser (LExpr Offset)
atomParser =
  locatedParser
    ( (\name -> maybe (Var $ locatedVal name) (Call name))
        <$> locatedParser identParser
        <*> optional (parens $ commaSep exprParser)
        <|> Bool <$> (True <$ symbol kTrue <|> False <$ symbol kFalse)
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

    multilinePartsParser :: Maybe Int -> Parser [Part Offset]
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

fieldParser :: Parser (Field Offset)
fieldParser =
  FStatic <$> identParser
    <|> FDynamic <$> between (symbolic '{') (symbolic '}') exprParser

branchParser :: Parser (Branch Offset)
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

data TypeError loc
  = NotInScope
      !loc
  | TypeMismatch
      !loc
      -- | Expected
      !Type
      -- | Actual
      !Type
  | UnexpectedFields
      !loc
      -- | Actual
      ![(Text, Type)]
  | MissingFields
      !loc
      -- | Expected
      ![(Text, Type)]
  | UnexpectedConstructors
      !loc
      -- | Actual
      ![(Text, [Type])]
  | MissingConstructors
      !loc
      -- | Expected
      ![(Text, [Type])]
  | ArityMismatch
      !loc
      -- | Expected
      !Int
      -- | Actual
      !Int
  | KindMismatch
      !(UnifyContext loc)
      -- | Expected
      !Kind
      -- | Actual
      !Kind
  | NotRequirement
      !loc
      -- | Offending identifier
      !Text
  | BlockBadRequirementType
      !loc
      -- | Actual requirement type
      !Type
  | RequirementAlreadySatisfied
      !loc
  | FileNotFound
      !loc
  | ParentParseError
      !loc
      -- | Template being parsed
      !TemplateRef
      Sage.ParseError
  | ParentTypeError
      -- | Location of error (in child)
      !loc
      -- | Template being type checked
      !TemplateRef
      (TypeError loc)
  | IncludeDisabled
      -- | Location of include filepath
      !loc
  | IncludeParseError
      -- | Location of include filepath
      !loc
      -- | Template being parsed
      !TemplateRef
      Sage.ParseError
  | IncludeTypeError
      -- | Location of include filepath
      !loc
      -- | File being type checked
      !TemplateRef
      (TypeError loc)
  | NotParam
      !loc
  | ParamAlreadyBound
      !loc
  deriving (Show)

typeErrorLoc :: TypeError loc -> loc
typeErrorLoc err =
  case err of
    NotInScope loc -> loc
    TypeMismatch loc _ _ -> loc
    UnexpectedFields loc _ -> loc
    MissingFields loc _ -> loc
    UnexpectedConstructors loc _ -> loc
    MissingConstructors loc _ -> loc
    ArityMismatch loc _ _ -> loc
    KindMismatch loc _ _ -> ucLocation loc
    NotRequirement loc _ -> loc
    BlockBadRequirementType loc _ -> loc
    RequirementAlreadySatisfied loc -> loc
    FileNotFound loc -> loc
    ParentParseError loc _ _ -> loc
    ParentTypeError loc _ _ -> loc
    IncludeDisabled loc -> loc
    IncludeParseError loc _ _ -> loc
    IncludeTypeError loc _ _ -> loc
    NotParam loc -> loc
    ParamAlreadyBound loc -> loc

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

renderTypeScheme :: TypeScheme -> String
renderTypeScheme (Forall vars ty) =
  case vars of
    [] -> renderType ty
    _ -> "forall " ++ unwords (fmap Text.unpack vars) ++ ". " ++ renderType ty

renderKind :: Kind -> String
renderKind KType = "Type"
renderKind KRow = "Row"

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

substMetas :: (Int -> Type) -> Type -> Type
substMetas f (TMeta v) = f v
substMetas _f t@TVar{} = t
substMetas _f TBool = TBool
substMetas _f TString = TString
substMetas f (TFn args retTy) = TFn (fmap (substMetas f) args) (substMetas f retTy)
substMetas f (TStream t) = TStream (substMetas f t)
substMetas f (TRecord fields) = TRecord (substMetas f fields)
substMetas f (TRecordField name t rest) = TRecordField name (substMetas f t) (substMetas f rest)
substMetas f (TSum ctors) = TSum (substMetas f ctors)
substMetas f (TSumConstructor name tys rest) = TSumConstructor name (fmap (substMetas f) tys) (substMetas f rest)
substMetas _f TRowEnd = TRowEnd

metavarsOf :: Type -> [Int]
metavarsOf (TMeta v) = [v]
metavarsOf TVar{} = []
metavarsOf TBool = []
metavarsOf TString = []
metavarsOf (TFn args retTy) = foldMap metavarsOf args <> metavarsOf retTy
metavarsOf (TStream ty) = metavarsOf ty
metavarsOf (TRecord fields) = metavarsOf fields
metavarsOf (TRecordField _name ty rest) = metavarsOf ty <> metavarsOf rest
metavarsOf (TSum ctors) = metavarsOf ctors
metavarsOf (TSumConstructor _name tys rest) = foldMap metavarsOf tys <> metavarsOf rest
metavarsOf TRowEnd = []

freeTypeVars :: Type -> Set Text
freeTypeVars TMeta{} = mempty
freeTypeVars (TVar v) = Set.singleton v
freeTypeVars TBool = mempty
freeTypeVars TString = mempty
freeTypeVars (TFn args retTy) = foldMap freeTypeVars args <> freeTypeVars retTy
freeTypeVars (TStream ty) = freeTypeVars ty
freeTypeVars (TRecord fields) = freeTypeVars fields
freeTypeVars (TRecordField _name ty rest) = freeTypeVars ty <> freeTypeVars rest
freeTypeVars (TSum ctors) = freeTypeVars ctors
freeTypeVars (TSumConstructor _name tys rest) = foldMap freeTypeVars tys <> freeTypeVars rest
freeTypeVars TRowEnd = mempty

data Binding
  = Binding
  { bindingName :: !Text
  , bindingScheme :: !TypeScheme
  , bindingLocations :: !(NonEmpty (TemplateRef, Offset))
  }

inferBindings ::
  MonadIO m =>
  -- | How to resolve a 'TemplateRef'
  (TemplateRef -> m (Maybe ByteString)) ->
  -- | Current template
  TemplateRef ->
  Template Offset ->
  ExceptT (TypeError Offset) m (Map TemplateRef (Template Offset), [Binding])
inferBindings readTemplateRef currentTemplate template = do
  result <-
    lift . runInferT (defaultInferEnv readTemplateRef currentTemplate) emptyInferState $ do
      checkTemplate template
      requirements <- getRequirements
      traverse
        generaliseBinding
        ( mapMaybe
            ( \req -> do
                guard . not $ reqSatisfied req
                pure (reqName req, reqType req, reqLocations req)
            )
            requirements
        )

  case result of
    Left err ->
      throwError err
    Right (state, bindings) ->
      pure (isDependencies state, bindings)
  where
    generaliseBinding ::
      Monad m =>
      (Text, Type, NonEmpty (TemplateRef, Offset)) ->
      InferT loc m Binding
    generaliseBinding (name, ty, locations) = do
      ty' <- zonkDefault ty
      pure $ Binding name (generaliseType ty') locations

varNameSupply :: [Text]
varNameSupply =
  fmap Text.pack $
    [[c] | c <- ['a' .. 'z']] ++ [c : show n | n <- [1 :: Int ..], c <- ['a' .. 'z']]

generaliseType :: Type -> TypeScheme
generaliseType ty =
  Forall
    boundVars
    ( substMetas
        ( \v ->
            maybe
              (error $ "no name for " ++ show v ++ " in " ++ show nameFor)
              TVar
              (IntMap.lookup v nameFor)
        )
        ty
    )
  where
    metas = nub $ metavarsOf ty
    boundVars = take (length metas) $ filter (`Set.notMember` freeTypeVars ty) varNameSupply
    nameFor = IntMap.fromList $ zip metas boundVars

newtype InferT loc m a
  = InferT (ReaderT (InferEnv m) (StateT (InferState loc) (ExceptT (TypeError loc) m)) a)
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadReader (InferEnv m), MonadError (TypeError loc))

instance MonadTrans (InferT loc) where
  lift = InferT . lift . lift . lift

runInferT ::
  Monad m =>
  InferEnv m ->
  InferState loc ->
  InferT loc m a ->
  m (Either (TypeError loc) (InferState loc, a))
runInferT e s (InferT ma) = runExceptT . fmap Tuple.swap . flip runStateT s . flip runReaderT e $ ma

data InferEnv m
  = InferEnv
  { ieCurrentTemplate :: !TemplateRef
  , ieReadTemplateRef :: !(TemplateRef -> m (Maybe ByteString))
  , ieScope :: !(Map Text TypeScheme)
  }

data TypeScheme = Forall ![Text] Type
  deriving (Show)

emptyInferEnv ::
  -- | How to resolve a 'TemplateRef'
  (TemplateRef -> m (Maybe ByteString)) ->
  -- | Current template
  TemplateRef ->
  InferEnv m
emptyInferEnv readTemplateRef currentTemplate =
  InferEnv{ieReadTemplateRef = readTemplateRef, ieCurrentTemplate = currentTemplate, ieScope = mempty}

defaultInferEnv ::
  -- | How to resolve a 'TemplateRef'
  (TemplateRef -> m (Maybe ByteString)) ->
  -- | Current template
  TemplateRef ->
  InferEnv m
defaultInferEnv readTemplateRef currentTemplate = (emptyInferEnv readTemplateRef currentTemplate){ieScope = defaultScope}

builtins :: Map Text (Value, TypeScheme)
builtins =
  let
    strip =
      ByteString.Lazy.Char8.dropWhileEnd Char.isSpace
        . ByteString.Lazy.Char8.dropWhile Char.isSpace

    plaintext :: LazyByteString -> LazyByteString
    plaintext input =
      let (prefix, rest) = ByteString.Lazy.Char8.break (\c -> c == '<' || c == '&') input
      in case ByteString.Lazy.Char8.uncons rest of
           Nothing -> prefix
           Just (c, rest') ->
             case c of
               '<' -> prefix <> plaintext (skip rest')
               '&' ->
                 case reference rest' of
                   Just (c', rest'') -> prefix <> c' <> plaintext rest''
                   Nothing -> prefix <> fromString "&" <> plaintext rest'
               _ -> undefined
      where
        skip :: LazyByteString -> LazyByteString
        skip x
          | Just x' <- ByteString.Lazy.Char8.stripPrefix (fromString "!--") x =
              dropThrough (fromString "-->") x'
          | Just x' <- ByteString.Lazy.Char8.stripPrefix (fromString "![CDATA[") x =
              dropThrough (fromString "]]>") x'
          | otherwise = inTag x

        -- \| Lazy version of 'ByteString.breakSubstring'
        breakSubstringLBS :: ByteString -> LazyByteString -> (ByteString, LazyByteString)
        breakSubstringLBS pat
          | ByteString.null pat = \rest -> (mempty, rest)
          | otherwise = go id mempty . LazyByteString.toChunks
          where
            keep = ByteString.length pat - 1

            go acc buffer [] = (ByteString.concat (acc [buffer]), mempty)
            go acc buffer (chunk : chunks) =
              let
                s = buffer <> chunk
                (before, rest) = ByteString.breakSubstring pat s
              in
                if ByteString.null rest
                  then
                    let (done, buffer') = ByteString.splitAt (ByteString.length s - keep) s
                    in go (acc . (done :)) buffer' chunks
                  else
                    (ByteString.concat (acc [before]), LazyByteString.fromChunks (rest : chunks))

        dropThrough :: ByteString -> LazyByteString -> LazyByteString
        dropThrough end x =
          let (_, x') = breakSubstringLBS end x
          in if LazyByteString.null x'
               then fromString ""
               else LazyByteString.drop (fromIntegral $ ByteString.length end) x'

        inTag :: LazyByteString -> LazyByteString
        inTag x =
          let x' = ByteString.Lazy.Char8.dropWhile (\c -> c /= '>' && c /= '"' && c /= '\'') x
          in case ByteString.Lazy.Char8.uncons x' of
               Nothing ->
                 fromString ""
               Just ('>', x'') ->
                 x''
               Just (quoteChar, x'') ->
                 inTag (LazyByteString.drop 1 (ByteString.Lazy.Char8.dropWhile (/= quoteChar) x''))

        reference :: LazyByteString -> Maybe (LazyByteString, LazyByteString)
        reference x = do
          let (name, rest) = ByteString.Lazy.Char8.span (\c -> Char.isAlphaNum c || c == '#') x
          rest' <- LazyByteString.stripPrefix (fromString ";") rest
          c <- entity name
          pure (c, rest')

        entity :: LazyByteString -> Maybe LazyByteString
        entity name
          | Just num <- LazyByteString.stripPrefix (fromString "#") name = numeric num
          | otherwise = lookup name named
          where
            named =
              [ (fromString "amp", fromString "&")
              , (fromString "lt", fromString "<")
              , (fromString "gt", fromString ">")
              , (fromString "quot", fromString "\"")
              , (fromString "apos", fromString "'")
              , (fromString "nbsp", fromString "\xa0")
              ]

            numeric :: LazyByteString -> Maybe LazyByteString
            numeric num
              | Just h <-
                  LazyByteString.stripPrefix (fromString "x") num <|> LazyByteString.stripPrefix (fromString "X") num =
                  toChar . Text.Read.hexadecimal . Text.Encoding.decodeUtf8 $ LazyByteString.toStrict h
              | otherwise =
                  toChar . Text.Read.decimal . Text.Encoding.decodeUtf8 $ LazyByteString.toStrict num

            toChar :: Either a (Int, Text) -> Maybe LazyByteString
            toChar (Right (n, r))
              | Text.null r
              , n >= 0
              , n <= 0x10FFFF =
                  Just . LazyByteString.fromStrict . Text.Encoding.encodeUtf8 . Text.singleton $ Char.chr n
            toChar _ =
              Nothing
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
      ,
        ( fromString "plaintext"
        ,
          ( VFn . Fn $ \case [s] -> VString (plaintext $ valueString s); _ -> undefined
          , Forall [] $ TFn [TString] TString
          )
        )
      ]

-- | @defaultScope = fmap snd 'builtins'@
defaultScope :: Map Text TypeScheme
defaultScope = fmap snd builtins

data InferState loc
  = InferState
  { isMetavars :: !(IntMap Metavar)
  , isRequirements :: ![Requirement loc]
  , isDependencies :: !(Map TemplateRef (Template loc))
  }

data Metavar
  = Metavar
  { metaKind :: Kind
  , metaSolution :: Maybe Type
  }

data Requirement loc
  = Requirement
  { reqName :: !Text
  , reqType :: !Type
  , reqLocations :: NonEmpty (TemplateRef, loc)
  -- ^ Places where the binding is introduced.
  , reqSatisfied :: !Bool
  }

data Kind = KType | KRow
  deriving (Show, Eq)

emptyInferState :: InferState loc
emptyInferState = InferState{isMetavars = mempty, isRequirements = mempty, isDependencies = mempty}

getRequirements :: Monad m => InferT loc m [Requirement loc]
getRequirements = InferT $ gets isRequirements

addDependency :: Monad m => TemplateRef -> Template loc -> InferT loc m ()
addDependency ref template = InferT $ modify $ \s -> s{isDependencies = Map.insert ref template $ isDependencies s}

checkTemplate :: MonadIO m => Template Offset -> InferT Offset m ()
checkTemplate (TemplateBase parts) = traverse_ (checkPart checkPartInclude) parts
checkTemplate (TemplateChild parent pragmas) = do
  readTemplateRef <- asks ieReadTemplateRef
  let parentRef = locatedVal parent
  mContent <- lift $ readTemplateRef parentRef
  case mContent of
    Nothing -> throwError $ FileNotFound (locatedLoc parent)
    Just content -> do
      parentTemplate <-
        case Sage.parse (templateParser <* Sage.eof) content of
          Left err -> throwError $ ParentParseError (locatedLoc parent) parentRef err
          Right x -> pure x
      local (\env -> env{ieCurrentTemplate = parentRef}) $
        checkTemplate parentTemplate
          `catchError` (throwError . ParentTypeError (locatedLoc parent) parentRef)
      traverse_ checkPragma pragmas
      addDependency parentRef parentTemplate

checkPragma :: MonadIO m => Pragma Offset -> InferT Offset m ()
checkPragma (PragmaBlock name parts) = do
  mReq <- lookupRequirement $ locatedVal name
  case mReq of
    Nothing ->
      throwError $ NotRequirement (locatedLoc name) (locatedVal name)
    Just req -> do
      if reqSatisfied req
        then
          throwError $ RequirementAlreadySatisfied (locatedLoc name)
        else do
          reqTy <- zonkDefault $ reqType req
          case reqTy of
            TString ->
              satisfyRequirement $ locatedVal name
            _ ->
              throwError $ BlockBadRequirementType (locatedLoc name) reqTy
  traverse_ (checkPart checkPartInclude) parts
checkPragma (PragmaWith vars) =
  for_ vars $ \(name, value) -> do
    mReq <- lookupRequirement $ locatedVal name
    case mReq of
      Nothing ->
        throwError $ NotRequirement (locatedLoc name) (locatedVal name)
      Just req ->
        if reqSatisfied req
          then
            throwError $ RequirementAlreadySatisfied (locatedLoc name)
          else do
            checkExpr checkPartInclude value $ reqType req
            satisfyRequirement $ locatedVal name

lookupRequirement :: Monad m => Text -> InferT loc m (Maybe (Requirement loc))
lookupRequirement name = InferT $ gets (find ((name ==) . reqName) . isRequirements)

satisfyRequirement :: Monad m => Text -> InferT loc m ()
satisfyRequirement name =
  InferT . modify $ \s ->
    s{isRequirements = modifyRequirement name (\r -> r{reqSatisfied = True}) $ isRequirements s}

modifyRequirement ::
  Text -> (Requirement loc -> Requirement loc) -> [Requirement loc] -> [Requirement loc]
modifyRequirement _name _f [] = []
modifyRequirement name f (r : rs) = if reqName r == name then f r : rs else r : modifyRequirement name f rs

checkPartInclude ::
  MonadIO m =>
  Located Offset TemplateRef ->
  Maybe [(Located Offset Text, LExpr Offset)] ->
  InferT Offset m ()
checkPartInclude target mWith = do
  readTemplateRef <- asks ieReadTemplateRef

  let includeRef = locatedVal target
  mContent <- lift $ readTemplateRef includeRef
  case mContent of
    Nothing -> throwError $ FileNotFound (locatedLoc target)
    Just content -> do
      includeTemplate <-
        case Sage.parse (templateParser <* Sage.eof) content of
          Left err -> throwError $ IncludeParseError (locatedLoc target) includeRef err
          Right x -> pure x

      let
        checkIncludeTemplate =
          local (\env -> env{ieCurrentTemplate = includeRef}) $
            checkTemplate includeTemplate
              `catchError` (throwError . IncludeTypeError (locatedLoc target) includeRef)
      case mWith of
        Nothing -> checkIncludeTemplate
        Just bindings -> do
          includeRequirements <- do
            currentRequirements <- InferT $ gets isRequirements
            InferT . modify $ \s -> s{isRequirements = []}
            checkIncludeTemplate
            includeRequirements <- InferT $ gets isRequirements
            InferT . modify $ \s -> s{isRequirements = currentRequirements}
            pure includeRequirements

          includeRequirements' <- foldlM bindRequirement includeRequirements bindings
          for_ includeRequirements' $ \req -> do
            unless (reqSatisfied req) $ do
              mExisting <- lookupRequirement $ reqName req
              case mExisting of
                Nothing ->
                  InferT . modify $ \s -> s{isRequirements = isRequirements s ++ [req]}
                Just existing -> do
                  unify (locatedLoc target) (reqType existing) (reqType req)
                  InferT . modify $ \s ->
                    s
                      { isRequirements =
                          updateRequirement
                            existing{reqLocations = reqLocations existing <> reqLocations req}
                            (isRequirements s)
                      }

      addDependency includeRef includeTemplate
  where
    bindRequirement reqs (name, value) =
      case find ((locatedVal name ==) . reqName) reqs of
        Nothing ->
          throwError $ NotParam (locatedLoc name)
        Just req
          | reqSatisfied req ->
              throwError $ ParamAlreadyBound (locatedLoc name)
          | otherwise -> do
              checkExpr checkPartInclude value $ reqType req
              pure $ modifyRequirement (locatedVal name) (\r -> r{reqSatisfied = True}) reqs

checkPartIncludeDisabled ::
  MonadIO m =>
  Located loc TemplateRef ->
  Maybe [(Located loc Text, LExpr loc)] ->
  InferT loc m ()
checkPartIncludeDisabled target _mWith =
  throwError $ IncludeDisabled (locatedLoc target)

checkPart ::
  MonadIO m =>
  {-| How to check 'PartInclude'

  See: 'checkPartInclude', 'checkPartIncludeDisabled'
  -}
  (Located loc TemplateRef -> Maybe [(Located loc Text, LExpr loc)] -> InferT loc m ()) ->
  Part loc ->
  InferT loc m ()
checkPart _fInclude PartText{} = pure ()
checkPart fInclude (PartExpr e) = checkExpr fInclude e TString
checkPart fInclude (PartExprStream e) = checkExpr fInclude e (TStream TString)
checkPart fInclude (PartInclude target mWith) = fInclude target mWith

instantiateTypeScheme :: Monad m => TypeScheme -> InferT loc m Type
instantiateTypeScheme (Forall vars ty) = do
  sub <- Map.fromList <$> traverse (\var -> (,) var <$> metavar KType) vars
  pure $ subst sub ty

checkExpr ::
  MonadIO m =>
  (Located loc TemplateRef -> Maybe [(Located loc Text, LExpr loc)] -> InferT loc m ()) ->
  LExpr loc ->
  Type ->
  InferT loc m ()
checkExpr _fInclude (Located offset (Var v)) t = do
  mTy <- asks (Map.lookup v . ieScope)
  ty <-
    case mTy of
      Just ty -> instantiateTypeScheme ty
      Nothing -> require offset v
  unify offset t ty
checkExpr _fInclude (Located offset Bool{}) t = do
  unify offset t TBool
checkExpr fInclude (Located offset (String parts)) t = do
  unify offset t TString
  traverse_ (checkPart fInclude) parts
checkExpr fInclude (Located offset (MultilineString parts)) t = do
  unify offset t TString
  traverse_ (checkPart fInclude) parts
checkExpr fInclude (Located offset (Call name args)) t = do
  argTys <- traverse (const $ metavar KType) args
  mTy <- asks (Map.lookup (locatedVal name) . ieScope)
  ty <-
    case mTy of
      Nothing -> throwError $ NotInScope (locatedLoc name)
      Just ty -> instantiateTypeScheme ty
  unify offset (TFn argTys t) ty
  for_ (zip args argTys) $ \(arg, argTy) -> do
    checkExpr fInclude arg argTy
checkExpr fInclude (Located offset (Record fields)) t = do
  fieldsWithTys <- traverse (\(name, e) -> (,,) name e <$> metavar KType) fields
  let actual = TRecord $ foldr (\(name, _e, ty) -> TRecordField name ty) TRowEnd fieldsWithTys
  unify offset t actual
  traverse_ (\(_name, e, ty) -> checkExpr fInclude e ty) fieldsWithTys
checkExpr fInclude (Located _offset (Field e f)) t =
  case f of
    FDynamic _f' ->
      error "TODO: dynamic record fields"
    FStatic f' -> do
      rest <- metavar KRow
      checkExpr fInclude e (TRecord $ TRecordField f' t rest)
checkExpr fInclude (Located offset (Constructor name args)) t = do
  argTys <- traverse (const $ metavar KType) args
  rest <- metavar KRow
  unify offset t (TSum $ TSumConstructor name argTys rest)
  for_ (zip args argTys) $ \(arg, argTy) ->
    checkExpr fInclude arg argTy
checkExpr fInclude (Located _offset (Match e bs)) t = do
  eTy <- inferExpr fInclude e
  for_ bs $ \(Branch p body) -> do
    bindings <- checkPattern p eTy
    local (\env -> env{ieScope = fmap (Forall []) bindings <> ieScope env}) $ checkExpr fInclude body t
checkExpr fInclude (Located _offset (IfThenElse cond th el)) t = do
  checkExpr fInclude cond TBool
  checkExpr fInclude th t
  checkExpr fInclude el t
checkExpr fInclude (Located offset (Array items)) t = do
  valueTy <- metavar KType
  unify offset t (TStream valueTy)
  for_ items $ \item -> do
    checkExpr fInclude item valueTy
checkExpr fInclude (Located offset (For name items value)) t = do
  valueTy <- metavar KType
  unify offset t (TStream valueTy)
  itemTy <- metavar KType
  checkExpr fInclude items (TStream itemTy)
  local (\env -> env{ieScope = Map.insert name (Forall [] itemTy) $ ieScope env}) $
    checkExpr fInclude value valueTy

inferExpr ::
  MonadIO m =>
  (Located loc TemplateRef -> Maybe [(Located loc Text, LExpr loc)] -> InferT loc m ()) ->
  LExpr loc ->
  InferT loc m Type
inferExpr fInclude e = do
  t <- metavar KType
  t <$ checkExpr fInclude e t

checkPattern ::
  Monad m =>
  Located loc Pattern ->
  Type ->
  InferT loc m (Map Text Type)
checkPattern (Located offset (PConstructor name args)) t = do
  argTys <- traverse (\arg -> (,) arg <$> metavar KType) args
  rest <- metavar KRow
  unify offset t (TSum $ TSumConstructor name (fmap snd argTys) rest)
  pure $ Map.fromList argTys

require ::
  Monad m =>
  -- | Location of variable
  loc ->
  Text ->
  InferT loc m Type
require offset name = do
  currentTemplate <- asks ieCurrentTemplate
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
                       , reqLocations = pure (currentTemplate, offset)
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
                req{reqLocations = reqLocations req <> pure (currentTemplate, offset)}
                (isRequirements s)
          }
      pure $ reqType req

updateRequirement :: Requirement loc -> [Requirement loc] -> [Requirement loc]
updateRequirement _new [] = []
updateRequirement new (req : reqs)
  | reqName new == reqName req = new : reqs
  | otherwise = req : updateRequirement new reqs

-- | Generate a unification variable.
metavar :: Monad m => Kind -> InferT loc m Type
metavar kind = InferT $ do
  s <- get
  let metavars = isMetavars s
  let n = IntMap.size metavars
  put s{isMetavars = IntMap.insert n (Metavar kind Nothing) metavars}
  pure $ TMeta n

unify ::
  (HasCallStack, Monad m) =>
  loc ->
  -- | Expected
  Type ->
  -- | Actual
  Type ->
  InferT loc m ()
unify offset expected actual = unifyWithContext (UnifyContext offset expected actual) expected actual

data UnifyContext loc
  = UnifyContext
  { ucLocation :: !loc
  -- ^ Source location that caused unification.
  , ucExpected :: !Type
  , ucActual :: !Type
  }
  deriving (Show)

unifyWithContext ::
  (HasCallStack, Monad m) =>
  UnifyContext loc ->
  -- | Expected
  Type ->
  -- | Actual
  Type ->
  InferT loc m ()
unifyWithContext ctx = go
  where
    typeMismatch = do
      let UnifyContext offset expected actual = ctx
      expected' <- zonk False expected
      actual' <- zonk False actual
      throwError $ TypeMismatch offset expected' actual'

    go (TMeta m) ty = solveL ctx m ty
    go ty (TMeta m) = solveR ctx ty m
    go (TVar v) ty =
      case ty of
        TVar v' | v == v' -> pure ()
        _ -> typeMismatch
    go TBool ty =
      case ty of
        TBool -> pure ()
        _ -> typeMismatch
    go TString ty =
      case ty of
        TString -> pure ()
        _ -> typeMismatch
    go (TFn args retTy) ty =
      case ty of
        TFn args' retTy' -> do
          unless (length args == length args') . throwError $
            ArityMismatch (ucLocation ctx) (length args) (length args')
          traverse_ (uncurry $ go) (zip args args')
          go retTy retTy'
        _ -> typeMismatch
    go (TStream a) ty =
      case ty of
        TStream a' -> go a a'
        _ -> typeMismatch
    go (TRecord fields) ty =
      case ty of
        TRecord fields' -> do
          (fields1, rest) <- getRecordFields fields
          (fields1', rest') <- getRecordFields fields'
          (unmatched, unmatched') <- unifyFields ctx fields1 fields1'
          final <- metavar KRow
          solveRecordTailL ctx rest unmatched' final
          solveRecordTailR ctx unmatched rest' final
        _ -> typeMismatch
    go (TSum ctors) ty =
      case ty of
        TSum ctors' -> do
          (ctors1, rest) <- getSumConstructors ctors
          (ctors1', rest') <- getSumConstructors ctors'
          (unmatched, unmatched') <- unifyConstructors ctx ctors1 ctors1'
          final <- metavar KRow
          solveSumTailL ctx rest unmatched' final
          solveSumTailR ctx unmatched rest' final
        _ -> typeMismatch
    go TRecordField{} _ = error "don't unify TRecordField"
    go TSumConstructor{} _ = error "don't unify TRecordField"
    go TRowEnd{} _ = error "don't unify TRowEnd"

unifyFields ::
  Monad m =>
  UnifyContext loc ->
  [(Text, Type)] ->
  [(Text, Type)] ->
  InferT loc m ([(Text, Type)], [(Text, Type)])
unifyFields ctx expected actual = do
  let !remainingExpected = expected' `Map.difference` actual'
  let !remainingActual = actual' `Map.difference` expected'
  for_ expected $ \(name, ty) ->
    for_ (Map.lookup name actual') $ \ty' -> do
      unifyWithContext ctx ty ty'
  pure (Map.toList remainingExpected, Map.toList remainingActual)
  where
    expected' = Map.fromList expected
    actual' = Map.fromList actual

unifyConstructors ::
  Monad m =>
  UnifyContext loc ->
  [(Text, [Type])] ->
  [(Text, [Type])] ->
  InferT loc m ([(Text, [Type])], [(Text, [Type])])
unifyConstructors ctx expected actual = do
  let !remainingExpected = expected' `Map.difference` actual'
  let !remainingActual = actual' `Map.difference` expected'
  for_ expected $ \(name, tys) -> do
    for_ (Map.lookup name actual') $ \tys' -> do
      when (length tys /= length tys') . throwError $
        ArityMismatch (ucLocation ctx) (length tys) (length tys')
      traverse_ (uncurry $ unifyWithContext ctx) $ zip tys tys'
  pure (Map.toList remainingExpected, Map.toList remainingActual)
  where
    expected' = Map.fromList expected
    actual' = Map.fromList actual

getRecordFields :: Monad m => Type -> InferT loc m ([(Text, Type)], Maybe Int)
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

getSumConstructors :: Monad m => Type -> InferT loc m ([(Text, [Type])], Maybe Int)
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

kindOf :: Monad m => Type -> InferT loc m Kind
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
  (HasCallStack, Monad m) =>
  UnifyContext loc ->
  -- | Expected
  Int ->
  -- | Actual
  Type ->
  InferT loc m ()
solveL ctx v ty' = do
  mMeta <- InferT $ gets (IntMap.lookup v . isMetavars)
  case mMeta of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta -> do
      let expectedKind = metaKind meta
      actualKind <- kindOf ty'
      unless (expectedKind == actualKind) . throwError $
        KindMismatch ctx expectedKind actualKind
      case metaSolution meta of
        Nothing -> InferT . modify $ \s -> s{isMetavars = IntMap.insert v meta{metaSolution = Just ty'} (isMetavars s)}
        Just ty -> unifyWithContext ctx{ucExpected = ty} ty ty'

solveR ::
  (HasCallStack, Monad m) =>
  UnifyContext loc ->
  -- | Expected
  Type ->
  -- | Actual
  Int ->
  InferT loc m ()
solveR ctx ty v = do
  mMeta' <- InferT $ gets (IntMap.lookup v . isMetavars)
  case mMeta' of
    Nothing -> error $ "missing metavar: " ++ show v
    Just meta' -> do
      expectedKind <- kindOf ty
      let actualKind = metaKind meta'
      unless (expectedKind == actualKind) . throwError $
        KindMismatch ctx expectedKind actualKind
      case metaSolution meta' of
        Nothing -> InferT . modify $ \s -> s{isMetavars = IntMap.insert v meta'{metaSolution = Just ty} (isMetavars s)}
        Just ty' -> unifyWithContext ctx{ucActual = ty'} ty ty'

solveRecordTailL ::
  (HasCallStack, Monad m) =>
  UnifyContext loc ->
  -- | Optional metavariable for the "expected" record's tail.
  Maybe Int ->
  -- | Remaining "actual" fields
  [(Text, Type)] ->
  -- | Shared tail of the unified records
  Type ->
  InferT loc m ()
solveRecordTailL ctx rest unmatched' final =
  case rest of
    Nothing ->
      unless (null unmatched') . throwError $
        UnexpectedFields (ucLocation ctx) unmatched'
    Just v ->
      solveL ctx v (foldr (uncurry TRecordField) final unmatched')

solveRecordTailR ::
  (HasCallStack, Monad m) =>
  UnifyContext loc ->
  -- | Remaining "expected" fields
  [(Text, Type)] ->
  -- | Optional metavariable for the "actual" record's tail.
  Maybe Int ->
  -- | Shared tail of the unified records
  Type ->
  InferT loc m ()
solveRecordTailR ctx unmatched rest' final = do
  case rest' of
    Nothing ->
      unless (null unmatched) . throwError $
        MissingFields (ucLocation ctx) unmatched
    Just v' ->
      solveR ctx (foldr (uncurry TRecordField) final unmatched) v'

solveSumTailL ::
  Monad m =>
  UnifyContext loc ->
  -- | Optional metavariable for the "expected" sum's tail.
  Maybe Int ->
  -- | Remaining "actual" constructors
  [(Text, [Type])] ->
  -- | Shared tail of the unified sums
  Type ->
  InferT loc m ()
solveSumTailL ctx rest unmatched' final =
  case rest of
    Nothing ->
      unless (null unmatched') . throwError $
        UnexpectedConstructors (ucLocation ctx) unmatched'
    Just v ->
      solveL ctx v (foldr (uncurry TSumConstructor) final unmatched')

solveSumTailR ::
  Monad m =>
  UnifyContext loc ->
  -- | Remaining "expected" constructors
  [(Text, [Type])] ->
  -- | Optional metavariable for the "actual" sum's tail.
  Maybe Int ->
  -- | Shared tail of the unified sums
  Type ->
  InferT loc m ()
solveSumTailR ctx unmatched rest' final = do
  case rest' of
    Nothing ->
      unless (null unmatched) . throwError $
        MissingConstructors (ucLocation ctx) unmatched
    Just v' ->
      solveR ctx (foldr (uncurry TSumConstructor) final unmatched) v'

zonkNoDefault ::
  Monad m =>
  Type ->
  InferT loc m Type
zonkNoDefault = zonk False

zonkDefault ::
  Monad m =>
  Type ->
  InferT loc m Type
zonkDefault = zonk True

zonk ::
  Monad m =>
  -- | Replace unsolved metas with default types
  Bool ->
  Type ->
  InferT loc m Type
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

valueBool :: HasCallStack => Value -> Bool
valueBool VTrue = True
valueBool VFalse = False
valueBool v = error $ "expected bool, got " ++ show v

valueString :: HasCallStack => Value -> LazyByteString
valueString (VString s) = s
valueString v = error $ "expected string, got " ++ show v

valueRecord :: HasCallStack => Value -> Map Text Value
valueRecord (VRecord r) = r
valueRecord v = error $ "expected record, got " ++ show v

valueStream :: HasCallStack => Value -> [Value]
valueStream (VStream s) = s
valueStream v = error $ "expected stream, got " ++ show v

valueFn :: HasCallStack => Value -> [Value] -> Value
valueFn (VFn (Fn f)) = f
valueFn v = error $ "expected function, got " ++ show v

data EvalEnv loc
  = EvalEnv
  { eeCurrentTemplate :: !TemplateRef
  , eeDependencies :: !(Map TemplateRef (Template loc))
  , eeScope :: !(Map Text Value)
  }

defaultEvalEnv ::
  TemplateRef ->
  Map TemplateRef (Template loc) ->
  EvalEnv loc
defaultEvalEnv currentTemplate dependencies =
  EvalEnv
    { eeCurrentTemplate = currentTemplate
    , eeDependencies = dependencies
    , eeScope = defaultCtx
    }

-- | @defaultCtx = fmap fst 'builtins'@
defaultCtx :: Map Text Value
defaultCtx = fmap fst builtins

evalTemplate :: HasCallStack => EvalEnv loc -> Template loc -> LazyByteString
evalTemplate env (TemplateBase parts) =
  foldMap (\part -> withFrozenCallStack (evalPart env part)) parts
evalTemplate env (TemplateChild parent pragmas) =
  let
    parentRef = locatedVal parent
    template =
      fromMaybe (error $ "missing dependency: " ++ show parentRef) $
        Map.lookup parentRef (eeDependencies env)
    !ctx' = Map.fromList $ foldMap (evalPragma env) pragmas
  in
    withFrozenCallStack
      (evalTemplate env{eeCurrentTemplate = parentRef, eeScope = ctx' <> eeScope env} template)

evalPragma :: EvalEnv loc -> Pragma loc -> [(Text, Value)]
evalPragma env (PragmaBlock name parts) =
  let
    !value = VString $! foldMap (\part -> withFrozenCallStack (evalPart env part)) parts
  in
    [(locatedVal name, value)]
evalPragma env (PragmaWith vars) =
  [(locatedVal name, value) | (name, expr) <- vars, let !value = evalExpr env (locatedVal expr)]

evalPart :: HasCallStack => EvalEnv loc -> Part loc -> LazyByteString
evalPart _env (PartText t) =
  Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict t
evalPart env (PartExpr e) =
  valueString $ evalExpr env (locatedVal e)
evalPart env (PartExprStream e) =
  foldMap valueString . valueStream $ evalExpr env (locatedVal e)
evalPart env (PartInclude ref mWith) =
  let
    includeRef = locatedVal ref
    scope =
      case mWith of
        Nothing ->
          eeScope env
        Just bindings ->
          Map.fromList
            [ (locatedVal name, value) | (name, expr) <- bindings, let !value = evalExpr env (locatedVal expr)
            ]
            <> eeScope env
    template =
      fromMaybe (error $ "missing dependency: " ++ show includeRef) $
        Map.lookup includeRef $
          eeDependencies env
  in
    withFrozenCallStack (evalTemplate env{eeCurrentTemplate = includeRef, eeScope = scope} template)

evalExpr :: EvalEnv loc -> Expr loc -> Value
evalExpr env (Var v) =
  case Map.lookup v $ eeScope env of
    Nothing -> error $ "not in scope: " ++ Text.unpack v
    Just value -> value
evalExpr _env (Bool b) =
  if b then VTrue else VFalse
evalExpr env (String parts) =
  VString $! foldMap (\part -> withFrozenCallStack (evalPart env part)) parts
evalExpr env (MultilineString parts) =
  VString $! foldMap (\part -> withFrozenCallStack (evalPart env part)) parts
evalExpr env (Call name args) =
  let
    !f = valueFn $ evalExpr env (Var $ locatedVal name)
    !args' = fmap (evalExpr env . locatedVal) args
  in
    f args'
evalExpr env (Record fields) =
  VRecord $! Map.fromList ((fmap . fmap) (evalExpr env . locatedVal) fields)
evalExpr env (Field expr field) =
  let
    record = valueRecord $ evalExpr env (locatedVal expr)
    field' =
      case field of
        FStatic f ->
          f
        FDynamic e ->
          Text.Encoding.decodeUtf8
            . LazyByteString.toStrict
            . valueString
            $ evalExpr env (locatedVal e)
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
    !args' = fmap (evalExpr env . locatedVal) args
  in
    VConstructor name args'
evalExpr env (Match e bs) =
  let
    v = evalExpr env (locatedVal e)
    (bindings, body) =
      foldr
        ( \(Branch pattern body') rest ->
            case match (locatedVal pattern) v of
              Nothing -> rest
              Just bindings' -> (bindings', body')
        )
        (error "pattern match failure")
        bs
  in
    evalExpr env{eeScope = bindings <> eeScope env} (locatedVal body)
evalExpr env (IfThenElse cond t e) =
  if valueBool $ evalExpr env (locatedVal cond)
    then evalExpr env (locatedVal t)
    else evalExpr env (locatedVal e)
evalExpr env (Array items) =
  VStream [evalExpr env (locatedVal item) | item <- items]
evalExpr env (For name xs yield) =
  let
    xs' = valueStream $ evalExpr env (locatedVal xs)
  in
    VStream [evalExpr env{eeScope = Map.insert name x' $ eeScope env} (locatedVal yield) | x' <- xs']

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
