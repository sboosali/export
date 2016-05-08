{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds, InstanceSigs #-}
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

{-| a canonical marshalling for a functor @f@.

@
Marshall f m from_f into_f
@

naming:

* @f@: a functor. the foreign\/serialized\/persisted type.
parametrized over @a@ to enable greater type-safety
(e.g. @f ~ "Data.Storeable.Ptr"@).
* @m@: a monad that the marshalling can use and fail in.
(e.g. @m ~ IO@).
* @from_f@: a class for marshalling from @f@ (e.g. "Data.Storeable.peek")
* @into_f@: a class for marshalling into @f@ (e.g. "Data.Storeable.poke")
* @a@: (anything that safisfies the constraints.
the type to be marshalled.)

The @FunctionalDependencies@ (@f -> m from_f into_f@) state that @Marshall@
is like a single-parameter typeclass on @f@. i.e. you can only have one
@Marshall f ...@ for any @f@,
but different @f@'s can use the same monad or the same constraints.

-}

-- use data (like Iso), not a class?

class (Monad m) =>

    Marshall (f      :: * -> *)
             (m      :: * -> *)
             (from_f :: * -> Constraint)
             (into_f :: * -> Constraint)

    | f -> m from_f into_f
 where

 from :: (from_f a) => f a -> m a --TODO marshallFrom
 into :: (into_f a) => a   -> m (f a) --TODO marshallInto

from_
 :: ( Marshall (C b) m from_f into_f
    , from_f a
    )
 => b
 -> m a
from_ = C >>> from

into_
 :: ( Marshall (C b) m from_f into_f
    , into_f a
    )
 => a
 -> m b
into_ = into >>> fmap getConst


-- | marshall via strings
instance Marshall (C String) Maybe Read Show where

 from :: (Read a) => C String a -> Maybe a
 from = getConst >>> readMaybe
 -- >>> maybe (throwM "") return

 into :: (Show a) => a -> Maybe (C String a)
 into = show >>> C >>> return


-- | marshall via text
instance Marshall (C T.Text) Maybe Read Show where

 from = getConst >>> T.unpack >>> readMaybe

 into = show >>> T.pack >>> C >>> return


-- | marshall via pointers
instance Marshall Ptr IO Storable Storable where
  from = peek
  into = new

-- | marshall via JSON
instance Marshall (C B.ByteString) (Either String) FromJSON ToJSON where
  -- not Value. use newtype?
  from = getConst >>> eitherDecode'
  into = encode >>> C >>> return
  -- eitherDecode' :: FromJSON a => ByteString -> Either String a

{-| transform a 'Function' by 'Marshall'ing its inputs and output.

e.g. usage:

>>> let hs_Marshall_or = marshalled hs_or
>>> hs_Marshall_or `call_` (C "False" :& C "True" :& Z)
Just "True"
>>> :t hs_Marshall_or
hs_Marshall_or
  :: (from_f Bool, into_f Bool, Marshall f m from_f into_f) =>
     Function m f "or" '[Bool, Bool] Bool

>>> let hs_String_or = hs_Marshall_or :: Function Maybe (Const String) "or" [Bool,Bool] Bool
>>> :t hs_String_or
hs_String_or
 :: Function Maybe (Const String) "or" '[Bool, Bool] Bool
>>> hs_String_or `call_` (C "False" :& C "True" :& Z)

Note that (1) its type was inferred (and can be specialized)
and (2) it consumes Booleans encoded as Strings, and
produces a Boolean encoded as a String too. (see 'call_')

e.g. specialization:

@
marshalled
 :: ( Marshall (C String) Maybe Read Show

    , "Export.Vinyl.EachHas" Read inputs
      -- the inputs can all be 'Read'.

    , Show output)
      -- the output can be 'Show'n.

 => Function I     'I'          name inputs output
    -- take a function that consumes haskell types and always succeeds...

 -> Function Maybe ('C' String) name inputs output
    -- ...to a function that consumes strings may fail (when parsing them).
@

-}
marshalled
 :: forall f m from_f into_f name input output.
    ( Marshall f m from_f into_f
    , EachHas from_f input
    ,         into_f output
    )
 => Function I I name input output
 -> Function m f name input output
marshalled (Function function) = Function $ \inputs -> do
  _inputs <- rtraverseFrom (P::P from_f) _from inputs
  let Identity (Identity _output) = function _inputs
  output <- into _output
  return output

 where
 _from :: forall x. (from_f x) => f x -> m x
 _from = from
