{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, ConstraintKinds, GADTs #-}
{-|

related:

* <https://github.com/nh2/call-haskell-from-anything call-haskell-from-anything>

* <https://haskell-servant.github.io/ servant>

-}
module Export where
import Export.Types
import Export.Curry
import Export.Extra
import Export.Vinyl

import Data.Tagged (Tagged(..))

import Control.Arrow (Kleisli(..))
import Data.Typeable
import GHC.TypeLits


tInputs
 :: forall function. ( EachHas Typeable (Inputs function)
                     , RecApplicative (Inputs function)
                     )
 => function
 -> [TypeRep]
tInputs _ = recordToList ts
 where
 ts = rmap ftypeRep ds                :: Rec (Const TypeRep)  (Inputs function)
 ds = reifyConstraint0 pTypeable ps   :: Rec (Dict0 Typeable) (Inputs function)
 ps = rproxy                          :: Rec P                (Inputs function)
 pTypeable = (P::P Typeable)
 ftypeRep :: forall a. Dict0 Typeable a -> C TypeRep a
 ftypeRep Dict0 = typeRep (P::P a) & Const

tOutput :: forall function. (Typeable (Output function)) => function -> TypeRep
tOutput _ = typeRep (P::P (Output function))

tSignature :: forall function. (Typeable function) => function -> TypeRep
tSignature _ = typeRep (P::P function)

{-| make an exportable 'Function' from any haskell function.

>>> let hs_or = newFunction (P::P "or") (||)
>>> :kind! hs_or
Function I I "or" [Bool,Bool] Bool

TODO:

>>> :set -XVisibleTypeApplication
>>> let hs_and = newFunction @"and" (&&)

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
 -> (HaskellFunction name (Inputs function) (Output function))
newFunction _ function
 = Function $ (fmap Identity . fmap Identity) (rUncurry function)

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

-- haskellFunction
 -- :: (Curry (Rec f input) (m (f output)) function)
 -- => Function m f name inputs output
 -- -> function
-- haskellFunction (Function function)
--  = (fmap getIdentity . fmap getIdentity) (rCurry function)

{-|

the name of the function, on both levels (value-level and type-level).

(ignores its input)

>>> unTagged (functionName hs_and)
"and"
>>> :kind! functionName hs_and
Tagged "and" String

-}
functionName
 :: forall m f name inputs output. (KnownSymbol name)
 => Function m f name inputs output
 -> Tagged name String
functionName _ = Tagged (symbolVal (P::P name))

{- e.g.

@
let f :: a -> b -> c
let hs_f = newFunction2 (P::P "f") f
let newFunction2 :: proxy name -> function -> (Function I I name (Inputs function) (Output function))
    newFunction2 = newFunction
@

specializing:

@
newFunction2 _ :: (RUncurry (a -> b -> c) (Inputs (a -> b -> c)) (Output (a -> b -> c)))
               => (a -> b -> c) -> (Function I I name (Inputs (a -> b -> c)) (Output (a -> b -> c)))
newFunction2 _ :: (RUncurry (a -> b -> c) [a,b] c))
               => (a -> b -> c) -> (Function I I name [a,b] c)
(Function I I _ [a,b] c) ~ (Rec f [a,b] -> I (I c))
@

@
fmap   :: (a -> b) -> f a -> f b
fmap   :: (c -> I c) -> ((->) a ((->) b (I c)) -> ((->) a ((->) b (I c))
fmap   :: (c -> I c) -> (a -> b -> c) -> (a -> b -> I c)
fmap I :: (a -> b -> c) -> (a -> b -> I c)
@
-}

{-| no name.

e.g. for convenience

>>> 'newFunction' nameless (+)

-}
nameless :: P ""
nameless = P

{-| 'Marshall' the inputs and output of a 'Function'.

e.g.

@
marshalled
 :: (Marshall Storeable Storeable IO Ptr, RecAll Storeable input, Storeable output)
 =>     Function I   name input output
 -> IO (Function Ptr name input output)
@

@
marshalled
 :: (Marshall ToJSON FromJSON Maybe (Const JSON), RecAll FromJSON input, ToJSON output)
 =>        Function I            name input output
 -> Maybe (Function (Const JSON) name input output)
@

-}
-- marshalled
--  :: forall f m from_f into_f name input output.
--     ( Marshall from_f into_f m f
--     , EachHas from_f input
--     ,         into_f output
--     )
--  => Function I I name input output
--  -> Function m f name input output
-- marshalled (Function function) = Function $ \inputs -> do
--   _inputs <- rtraverseFrom (P::P from_f) _from inputs
--   let _output = function _inputs
--   output <- into _output
--   return output
--
--  where
--  _from :: forall x. (from_f x) => f x -> m x
--  _from = from
