{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds, InstanceSigs #-}
{-


kwargs: named/defaulting arguments

callKwargs
 :: (bs \in as, RecALl Default (as \diff bs))
 => Funtion ... as c -> (bs -> c)

class Default function_name argument_name argument_type where
 def'


f :: Int -> String -> Bool
f x y = ...
>>> liftFunction 'f
==>
f' :: Function Id "f" [Int,String] Bool
f' = ...

NOTE needs f's signature, otherwise:

>>> liftFunction 'f [''Int,''String]
==>
f' :: Function Id "f" [Int,String] Bool


g :: Int -> String -> Bool
g x y = ...
>>> makeFunctionKwargs 'g
==>
-- | takes @x :: 'Int'@ and @y :: 'String'@
g' :: Function Id Field "g" ["x" ::: Int, "y" ::: String] Bool
g' = ...

TODO th needs function declaration introspection and haddock gen, otherwise:
>>> makeFunctionKwargs 'g ["x", "y"]
==>
g' :: Function Id Field "g" ["x" ::: Int, "y" ::: String] Bool


optinally-optional arguments
 (meta :: Rec Maybe input) -- value-level
 (docstring :: String) -- value-levelm unlike (name :: Symbol)


-}
module Export.Types where
import Export.Vinyl
import Export.Extra

--import Control.Monad.Catch (MonadThrow(..))

--import Control.Applicative (Const(..))
import GHC.TypeLits (Symbol)
import GHC.Exts (Constraint)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

{-| an effectful, uncurried, named function.

@
Function m f name inputs output
@

naming:

* @m@: a monad
* @f@: a functor
* @name@: the name
* @input@:
* @output@:

@name@ is type-level (not value-level) to support static validation
(e.g. checking that the 'Symbol' a valid python identifier).

-}
data Function--TODO Export.Function
 (m      :: * -> *)
 (f      :: * -> *)
 (name   :: Symbol)
 (inputs :: [*])
 (output :: *)
  -- docstring? general metadata.

  = Function (Rec f inputs -> m (f output))

  deriving (Functor)

{-| no effects, any inputs.

-}
type HaskellFunction = Function I I

----------------------------------------------------------------------------

{-| a canonical marshalling for a functor @f@.

@
Marshall from_f into_f m f
@

naming:

* @m@: a monad that the marshalling can use and fail in.
(e.g. @m ~ IO@).
* @f@: a functor. the foreign\/serialized\/persisted type.
parametrized over @a@ to enable greater type-safety
(e.g. @f ~ "Data.Storeable.Ptr"@).
* @from_f@: a class for marshalling from @f@ (e.g. "Data.Storeable.peek")
* @into_f@: a class for marshalling into @f@ (e.g. "Data.Storeable.poke")
* @a@: (anything that safisfies the constraints.
the type to be marshalled.)

The @FunctionalDependencies@ (@f -> m from_f into_f@) state that @Marshall@
is like a single-parameter typeclass on @f@. i.e. you can only have one
@Marshall f ...@ for any @f@,
but different @f@'s can use the same monad or the same constraints.

-}
-- class (MonadThrow m) => Marshall from_f into_f m f
--TODO use data (like Iso), not a class?

class (Monad m, Functor f) => --TODO Export.Marshall
    Marshall (from_f :: * -> Constraint)
             (into_f :: * -> Constraint)
             (m      :: * -> *)
             (f      :: * -> *) --TODO make first param
    | f -> m from_f into_f
 where

 from :: (from_f a) => f a -> m a --TODO marshallFrom
 into :: (into_f a) => a   -> m (f a) --TODO marshallInto

from_
 :: forall
     (from_f :: * -> Constraint)
     (into_f :: * -> Constraint)
     (m      :: * -> *)
     (b      :: *)
     (a      :: *).
    (Marshall from_f into_f m (C b), from_f a)
 => b
 -> m a
from_ = Const >>> from

into_ :: (Marshall from_f into_f m (C b), into_f a) => a -> m b
into_ = into >>> fmap getConst

{-

instance Marshall Storeable Storeable IO Ptr

instance Marshall FromJSON ToJSON (Either String) (C JSON)
-}

instance Marshall Read Show Maybe (C String) where

 from :: (Read a) => C String a -> Maybe a
 from = getConst >>> readMaybe
 -- >>> maybe (throwM "") return

 into :: (Show a) => a -> Maybe (C String a)
 into = show >>> Const >>> return
