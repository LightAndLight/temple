{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , getRequirements
  , InferEnv (..)
  , emptyInferEnv
  , runInferT
  , inferTemplate
  , inferExpr
  , checkExpr
  , inferPart
  , zonkDefault
  , zonkNoDefault

    -- * Evaluating
  , evalTemplate
  , evalPart
  , evalExpr

    -- ** Values
  , Value (..)
  , valueBool
  , valueString
  , valueRecord
  , valueStream
  )
where

import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Monad (guard, unless, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State.Class (get, gets, modify, put)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Char as Char
import Data.Foldable (for_, traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
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
import Text.Sage (Parser, char, notFollowedBy, satisfy, sepBy, skipMany, string, try, (<?>))
import qualified Text.Sage as Sage

newtype Template = Template [Part]
  deriving (Show, Eq)

data Part
  = PartText !Text
  | PartExpr !(Located Expr)
  | PartExprStream !(Located Expr)
  deriving (Show, Eq)

data Located a
  = Located
  { locatedOffset :: !Int
  , locatedValue :: !a
  }
  deriving (Show, Eq)

data Expr
  = Var !Text
  | String ![Part]
  | MultilineString ![Part]
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

templateParser :: Parser Template
templateParser =
  Template <$> many partParser

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
      ( noneOf "\\$"
          <|> (char '\\' *> (char '\\' <|> char '$'))
      )
    <|> partExprParser

partExprParser :: Parser Part
partExprParser =
  ($)
    <$ char '$'
    <*> token (PartExprStream <$ char '$' <|> pure PartExpr)
    <*> between (symbolic '(') (char ')') exprParser

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
      Record <$>
        between
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

atomParser :: Parser (Located Expr)
atomParser =
  locatedParser
    ( Var <$> identParser
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
                    noneOf "\\$\"\n"
                      <|> char '\\' *> (char '\\' <|> char '$' <|> char '"' <|> ('\n' <$ char 'n'))
                )
                <|> partExprParser
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
                  ( noneOf "\\$\"\n"
                      <|> (char '\\' *> (char '\\' <|> char '$' <|> char '"' <|> ('\n' <$ char 'n')))
                      <|> try doubleQuote1
                  )
                <*> (fmap pure (char '\n' <* for_ mIndent (optional . indentParser)) <|> pure [])
            )
            <|> partExprParser
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
  = TypeMismatch
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

data Type
  = TMeta !Int
  | TBool
  | TString
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

newtype InferT m a = InferT (ReaderT InferEnv (StateT InferState (ExceptT TypeError m)) a)
  deriving (Functor, Applicative, Monad, MonadReader InferEnv, MonadError TypeError)

runInferT :: Monad m => InferEnv -> InferState -> InferT m a -> m (Either TypeError (InferState, a))
runInferT e s (InferT ma) = runExceptT . fmap Tuple.swap . flip runStateT s . flip runReaderT e $ ma

data InferEnv
  = InferEnv
  { ieScope :: !(Map Text Type)
  }

emptyInferEnv :: InferEnv
emptyInferEnv = InferEnv{ieScope = mempty}

data InferState
  = InferState
  { isMetavars :: !(IntMap Metavar)
  , isRequirements :: ![(Text, Type)]
  }

data Metavar
  = Metavar
  { metaKind :: Kind
  , metaSolution :: Maybe Type
  }

data Kind = KType | KRow
  deriving (Show, Eq)

emptyInferState :: InferState
emptyInferState = InferState{isMetavars = mempty, isRequirements = mempty}

getRequirements :: Monad m => InferT m [(Text, Type)]
getRequirements = InferT $ gets isRequirements

inferTemplate :: Monad m => Template -> InferT m ()
inferTemplate (Template parts) = traverse_ inferPart parts

inferPart :: Monad m => Part -> InferT m ()
inferPart PartText{} = pure ()
inferPart (PartExpr e) = checkExpr e TString
inferPart (PartExprStream e) = checkExpr e (TStream TString)

checkExpr ::
  Monad m =>
  Located Expr ->
  Type ->
  InferT m ()
checkExpr (Located offset (Var v)) t = do
  mTy <- asks (Map.lookup v . ieScope)
  ty <-
    case mTy of
      Just ty -> pure ty
      Nothing -> require v
  unify offset t ty
checkExpr (Located offset (String parts)) t = do
  traverse_ inferPart parts
  unify offset t TString
checkExpr (Located offset (MultilineString parts)) t = do
  traverse_ inferPart parts
  unify offset t TString
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
    local (\env -> env{ieScope = bindings <> ieScope env}) $ checkExpr body t
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
  local (\env -> env{ieScope = Map.insert name itemTy $ ieScope env}) $
    checkExpr value valueTy

inferExpr :: Monad m => Located Expr -> InferT m Type
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

require :: Monad m => Text -> InferT m Type
require name = do
  mTy <- lookup name <$> getRequirements
  case mTy of
    Nothing -> do
      ty <- metavar KType
      InferT $ do
        modify $ \s -> s{isRequirements = isRequirements s ++ [(name, ty)]}
      pure ty
    Just ty ->
      pure ty

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
kindOf TBool = pure KType
kindOf TString = pure KType
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
zonk _def TBool = pure TBool
zonk _def TString = pure TString
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
  | VRecord !(Map Text Value)
  | VConstructor !Text ![Value]
  | VStream [Value]
  deriving (Show)

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

evalTemplate :: Map Text Value -> Template -> LazyByteString
evalTemplate ctx (Template parts) =
  foldMap
    (evalPart ctx)
    parts

evalPart :: Map Text Value -> Part -> LazyByteString
evalPart _ctx (PartText t) =
  Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict t
evalPart ctx (PartExpr e) =
  valueString $ evalExpr ctx (locatedValue e)
evalPart ctx (PartExprStream e) =
  foldMap valueString . valueStream $ evalExpr ctx (locatedValue e)

evalExpr :: Map Text Value -> Expr -> Value
evalExpr ctx (Var v) =
  case Map.lookup v ctx of
    Nothing -> error $ "not in scope: " ++ Text.unpack v
    Just value -> value
evalExpr ctx (String parts) =
  VString $ foldMap (evalPart ctx) parts
evalExpr ctx (MultilineString parts) =
  VString $ foldMap (evalPart ctx) parts
evalExpr ctx (Record fields) =
  VRecord $! Map.fromList ((fmap . fmap) (evalExpr ctx . locatedValue) fields)
evalExpr ctx (Field expr field) =
  let
    record :: Map Text Value
    record = valueRecord $ evalExpr ctx (locatedValue expr)

    field' :: Text
    field' =
      case field of
        FStatic f ->
          f
        FDynamic e ->
          Text.Encoding.decodeUtf8
            . LazyByteString.toStrict
            . valueString
            $ evalExpr ctx (locatedValue e)
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
evalExpr ctx (Constructor name args) =
  let
    !args' = fmap (evalExpr ctx . locatedValue) args
  in
    VConstructor name args'
evalExpr ctx (Match e bs) =
  let
    v = evalExpr ctx (locatedValue e)
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
    evalExpr (bindings <> ctx) (locatedValue body)
evalExpr ctx (IfThenElse cond t e) =
  if valueBool $ evalExpr ctx (locatedValue cond)
    then evalExpr ctx (locatedValue t)
    else evalExpr ctx (locatedValue e)
evalExpr ctx (Array items) =
  VStream [evalExpr ctx (locatedValue item) | item <- items]
evalExpr ctx (For name xs yield) =
  let
    xs' = valueStream $ evalExpr ctx (locatedValue xs)
  in
    VStream [evalExpr (Map.insert name x' ctx) (locatedValue yield) | x' <- xs']

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
