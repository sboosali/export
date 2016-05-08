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

{-
e.g.

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

{-| no name.

e.g. for convenience

>>> 'newFunction' nameless (+)

-}
nameless :: P ""
nameless = P

{-| transform a 'Function' by 'Marshall'ing its inputs and output.

e.g. usage:

>>> let hs_Marshall_or = marshalled hs_or
>>> hs_Marshall_or `call_` (C "False" :& C "True" :& Z)
Just "True"
>>> :t hs_Marshall_or
hs_Marshall_or
  :: (from_f Bool, into_f Bool, Marshall from_f into_f m f) =>
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
 :: ( Marshall Read Show Maybe (C String)

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
    ( Marshall from_f into_f m f
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
