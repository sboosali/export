{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Export.Marshall where
import Export.Vinyl
import Export.Extra
import Export.Function

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Aeson (FromJSON,ToJSON,encode,eitherDecode')

import GHC.Exts (Constraint)
import Text.Read (readMaybe)
import Foreign

{-| a marshaller between @f a@ and @a@.

@
Marshaller f m from_f into_f
@

naming:

* @f@: a functor. the foreign\/serialized\/persisted type.
parametrized over @a@ to enable greater type-safety
(e.g. @f ~ 'Ptr'@).
* @m@: a monad that the marshalling can use and fail in.
(e.g. @m ~ IO@).
* @from_f@: a class for marshalling from @f@ (e.g. 'peek')
* @into_f@: a class for marshalling into @f@ (e.g. 'poke')
* @a@: (anything that safisfies the constraints.
the type to be marshalled.)

-}

data Marshaller
 (f      :: * -> *)
 (m      :: * -> *)
 (from_f :: * -> Constraint)
 (into_f :: * -> Constraint)

 = Marshaller
  { marshallFrom :: forall a. (from_f a) => f a -> m a
  , marshallInto :: forall a. (into_f a) => a   -> m (f a)
  }

-- | marshall via strings
stringMarshaller :: Marshaller (C String) Maybe Read Show
stringMarshaller = Marshaller{..}
 where
 --NOTE explicit signatures are necessary (because of RankNTypes?)

 marshallFrom :: (Read a) => C String a -> Maybe a
 marshallFrom = getConst >>> readMaybe
 -- >>> maybe (throwM "") return

 marshallInto :: (Show a) => a -> Maybe (C String a)
 marshallInto = show >>> C >>> return

-- | marshall via text
textMarshaller :: Marshaller (C T.Text) Maybe Read Show
textMarshaller = Marshaller{..}
 where

 marshallFrom :: (Read a) => C T.Text a -> Maybe a
 marshallFrom = getConst >>> T.unpack >>> readMaybe

 marshallInto :: (Show a) => a -> Maybe (C T.Text a)
 marshallInto = show >>> T.pack >>> C >>> return

-- | marshall via pointers
pointerMarshaller :: Marshaller Ptr IO Storable Storable
pointerMarshaller = Marshaller{..}
  where

  marshallFrom :: (Storable a) => Ptr a -> IO a
  marshallFrom = peek

  marshallInto :: (Storable a) => a -> IO (Ptr a)
  marshallInto = new

-- | marshall via JSON
jsonMarshaller :: Marshaller (C B.ByteString) (Either String) FromJSON ToJSON
jsonMarshaller = Marshaller{..}
  where

  marshallFrom :: (FromJSON a) => C B.ByteString a -> Either String a
  marshallFrom = getConst >>> eitherDecode'

  marshallInto :: (ToJSON a) => a -> Either String (C B.ByteString a)
  marshallInto = encode >>> C >>> return
  -- eitherDecode' :: FromJSON a => ByteString -> Either String a

{-| transform a function by marshalling its inputs and output.

e.g. usage:

>>> let hs_String_or = stringMarshaller `marshalling` u_or
>>> :t hs_String_or
hs_String_or
 :: Function Maybe (Const String) "or" '[Bool, Bool] Bool
>>> hs_String_or `call_` (C "False" :& C "True" :& Z)
Just "True"

Note that it consumes Booleans encoded as Strings, and
produces a Boolean encoded as a String too. (see 'call_')

e.g. specialization:

@
marshalling
 :: ( Monad m

    , 'EachHas' Read inputs
      -- the inputs can all be 'Read'.

    , Show output)
      -- the output can be 'Show'n.

 => Marshaller (C String) Maybe Read Show

 -> Function I     'I'          name inputs output
    -- take a function that consumes haskell types and always succeeds...

 -> Function Maybe ('C' String) name inputs output
    -- ...to a function that consumes strings may fail (when parsing them).
@

-}
marshalling
 :: forall f m from_f into_f name input output.
    ( Monad m
    , EachHas from_f input
    ,         into_f output
    )
 => Marshaller f m from_f into_f
 -> Function I I name input output
 -> Function m f name input output
marshalling Marshaller{..} (Function function) = Function $ \inputs -> do
  _inputs <- rtraverseFrom (P::P from_f) _marshallFrom inputs
  let Identity (Identity _output) = function _inputs
  output <- marshallInto _output
  return output

 where
 _marshallFrom :: forall x. (from_f x) => f x -> m x
 _marshallFrom = marshallFrom
 --TODO "marshallingWith"?
