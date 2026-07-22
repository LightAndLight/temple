{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Temple.Compiler () where

import qualified Data.Vector.Storable as Storable
import Data.IntMap (IntMap)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import qualified Temple
import Control.Monad.State (StateT, runStateT)
import Data.Functor.Identity (runIdentity)
import qualified Data.Tuple as Tuple
import Data.Foldable (traverse_)
import qualified Data.Text.Encoding as Text.Encoding
import Control.Monad.State.Class (gets, modify)
import qualified Data.IntMap as IntMap
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Bits ((.&.), shiftR)


data Template
  = Template
  { templateStrings :: !ByteString
  , templateCode :: ![Inst]
  }

{- Instruction layout:

|-- opcode (8 bits) --|-- payload (24 bits) --|

-}

data Inst
  -- | Push a 24-bit value onto the stack.
  = Push
      -- | Value (24 bits)
      !Word32
  -- | Duplicate the top of the stack
  | Dup
  -- | Shift the top of the stack by @amount@
  | ShiftL
      -- | @amount@ (24 bits)
      !Word32
  -- | Logical-or the top of the stack with @rhs@
  | Or
      -- | @rhs@ (24 bits)
      !Word32
  -- | Add the top of the stack to @rhs@
  | Add
      -- | @rhs@ (24 bits)
      !Word32
  -- | Write out the string (offset, len) that's on top of the stack
  | Write

compile :: Temple.Template -> Template
compile template = runIdentity $ do
  (state, ()) <- runCompileT emptyCompileState $ compileTemplate template
  pure $ Template (LazyByteString.toStrict $ csStrings state) (csCode state)

newtype CompileT m a = CompileT (StateT CompileState m a)
  deriving (Functor, Applicative, Monad)

runCompileT :: Monad m => CompileState -> CompileT m a -> m (CompileState, a)
runCompileT s (CompileT ma) = Tuple.swap <$> runStateT ma s

data CompileState
  = CompileState
  { csStrings :: LazyByteString
  , csStringsSize :: !Word32
  , csCode :: [Inst]
  }

emptyCompileState :: CompileState
emptyCompileState = CompileState{ csStrings = mempty, csStringsSize = 0, csCode = mempty }

compileTemplate :: Monad m => Temple.Template -> CompileT m ()
compileTemplate (Temple.TemplateBase _file parts) = traverse_ compilePart parts
compileTemplate (Temple.TemplateChild _file _parentFile _pragmas) = error "TODO"

compilePart :: Monad m => Temple.Part -> CompileT m ()
compilePart (Temple.PartText t) = writeString $ Text.Encoding.encodeUtf8 t
compilePart (Temple.PartExpr e) = do
  compileExpr $ Temple.locatedValue e
  emit Write
compilePart (Temple.PartExprStream s) = _

compileExpr :: Monad m => Temple.Expr -> CompileT m ()
compileExpr (Temple.Var v) = _
compileExpr (Temple.String v) = _
compileExpr (Temple.MultilineString v) = _
compileExpr (Temple.Call name args) = _
compileExpr (Temple.Record fields) = _
compileExpr (Temple.Field record field) = _
compileExpr (Temple.Constructor name args) = _
compileExpr (Temple.Match e branches) = _
compileExpr (Temple.IfThenElse cond th el) = _
compileExpr (Temple.Array items) = _
compileExpr (Temple.For name items value) = _

intToWord32 :: (Monad m, Integral a) => a -> CompileT m Word32 
intToWord32 i = do
  let i' = (fromIntegral i :: Integer)
  if i' > fromIntegral (maxBound :: Word32)
    then _err
    else pure $ fromIntegral i'

allocString :: Monad m => ByteString -> CompileT m (Word32, Word32)
allocString str = do
  index <- CompileT $ gets csStringsSize
  len <- intToWord32 $ ByteString.length str
  newIndex <- do
    let newIndex = (fromIntegral index :: Integer) + fromIntegral len
    if newIndex > fromIntegral (maxBound :: Word32)
      then _err
      else pure (fromIntegral newIndex :: Word32)
  CompileT . modify $ \s -> s{csStrings = csStrings s <> LazyByteString.fromStrict str, csStringsSize = newIndex}
  pure (index, len)

emit :: Monad m => Inst -> CompileT m ()
emit i = CompileT . modify $ \s -> s{csCode = csCode s ++ [i]}

iPush32 :: Monad m => Word32 -> CompileT m ()
iPush32 v
  | v > 2^(24::Int) = do
      emit (Push $ v .&. 0xFFFF)
      emit (ShiftL 16)
      emit (Or $ (v `shiftR` 16) .&. 0xFFFF)
  | otherwise = emit (Push v)

writeString :: Monad m => ByteString -> CompileT m ()
writeString s = do
  (offset, len) <- allocString s
  iPush32 offset
  iPush32 len
  emit Write
