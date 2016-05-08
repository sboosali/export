{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds #-}
module Export.Function where
import Export.Vinyl
import Export.Extra
import Export.Curry

import Data.Tagged (Tagged(..))

import GHC.TypeLits
import Control.Arrow (Kleisli(..))
import Data.Typeable
import Numeric.Natural

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
data Function
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

{-| no name.

e.g. for convenience

>>> 'newFunction' nameless (+)

-}
nameless :: P ""
nameless = P


getInputs
 :: forall function. ( EachHas Typeable (Inputs function)
                     , RecApplicative (Inputs function)
                     )
 => function
 -> [TypeRep]
getInputs _ = recordToList ts
 where
 ts = rmap ftypeRep ds                :: Rec (Const TypeRep)  (Inputs function)
 ds = reifyConstraint0 pTypeable ps   :: Rec (Dict0 Typeable) (Inputs function)
 ps = rproxy                          :: Rec P                (Inputs function)
 pTypeable = (P::P Typeable)
 ftypeRep :: forall a. Dict0 Typeable a -> C TypeRep a
 ftypeRep Dict0 = typeRep (P::P a) & Const

getOutput :: forall function. (Typeable (Output function)) => function -> TypeRep
getOutput _ = typeRep (P::P (Output function))

getSignature :: forall function. (Typeable function) => function -> TypeRep
getSignature _ = typeRep (P::P function)

getArity
 :: forall function. (KnownNat (Length (Inputs function)))
 => function
 -> Tagged (Length (Inputs function)) Natural
getArity _ = tLength (P::P (Inputs function))

{-| make an exportable 'Function' from any haskell function.

>>> let hs_or = newFunction (P::P "or") (||)
>>> :t hs_or
hs_or :: Function I I "or" '[Bool, Bool] Bool

(see 'P').

TODO:

>>> :set -XVisibleTypeApplication
>>> let hs_or = newFunction @"or" (&&)

newFunction
 :: forall name function.
 -> function
 -> ...

-}
newFunction
 :: forall name function proxy.
  ( RUncurry function
             (Inputs function)
             (Output function)
  )
 => proxy name
 -> function
 -> (Function I I name (Inputs function) (Output function))
newFunction _ function
 = Function $ (fmap Identity . fmap Identity) (rUncurry function)

{-| export an effectful unary function.

-}
fromKleisli
 :: forall name m a b proxy.
 ( Functor m
 , RUncurry (a -> m b)
            '[a]
            (m b)
 )
 => proxy name
 -> Kleisli m a b
 -> Function m I name '[a] b
fromKleisli _ (Kleisli function)
 = Function $ ((fmap . fmap) Identity) (rUncurry function)

{-| call a 'Function', like a haskell function.

>>> (||) False True
True
>>> hs_or `call` (False :* True :* Z)
True

@call (Function function) = function@

-}
call
 :: Function m f name inputs output
 -> (Rec f inputs -> m (f output))
call (Function function) = function

{- | like 'call', but un-wraps 'Const', for convenience.

-}
call_
 :: (Functor m)
 => Function m (C a) name inputs output
 -> (Rec (C a) inputs -> m a)
call_ (Function function) = function >>> fmap getConst

-- haskellFunction
 -- :: (Curry (Rec f input) (m (f output)) function)
 -- => Function m f name inputs output
 -- -> function
-- haskellFunction (Function function)
--  = (fmap getIdentity . fmap getIdentity) (rCurry function)

{-| the name of the function,
on both levels (value-level and type-level).

(ignores its input)

>>> unTagged (functionName hs_or)
"or"
>>> :kind! functionName hs_or
Tagged "or" String

-}
functionName
 :: forall m f name inputs output. (KnownSymbol name)
 => Function m f name inputs output
 -> Tagged name String
functionName _ = Tagged (symbolVal (P::P name))
