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

import Data.Tagged (Tagged(..))
import Data.Vinyl
import Data.Vinyl.Functor
--import Data.Vinyl.TypeLevel hiding (Nat(..))

import GHC.TypeLits
import Control.Arrow (Kleisli(..))
import Data.Proxy
import Data.Typeable
import GHC.Exts (Constraint)


-- "Number of parameters must match family declaration"
type family Length (as :: [k]) :: Nat where
 Length '[] = 0
 Length (_a ': as) = 1 + Length as -- UndecidableInstances

{-|

>>> :set -XDataKinds
>>> import Data.Proxy
>>> tLength (Proxy :: Proxy [Int,String])
2

-}
tLength
 :: forall (as :: [k]) proxy. (KnownNat (Length as))
 => proxy as
 -> Integer
tLength _ = natVal (P::P (Length as))


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
 ps = rproxy                          :: Rec Proxy            (Inputs function)
 pTypeable = (P::P Typeable)
 ftypeRep :: forall a. Dict0 Typeable a -> Const TypeRep a
 ftypeRep Dict0 = typeRep (P::P a) & Const

tOutput :: forall function. (Typeable (Output function)) => function -> TypeRep
tOutput _ = typeRep (P::P (Output function))

tSignature :: forall function. (Typeable function) => function -> TypeRep
tSignature _ = typeRep (P::P function)

rproxy :: RecApplicative as => Rec Proxy as
rproxy = rpure Proxy

-- rdict :: RecApplicative as => Rec (Dict0 c) as
-- rdict = reifyConstraint0

type family EachHas (c :: * -> Constraint) (rs :: [u]) :: Constraint where
  EachHas c '[] = ()
  EachHas c (r ': rs) = (c r, EachHas c rs)

reifyConstraint1
  :: EachHas c rs
  => proxy c
  -> Rec I rs
  -> Rec (Dict1 c) rs
reifyConstraint1 proxy = \case
 RNil -> RNil
 (Identity x :& xs) -> (Dict x) :& reifyConstraint1 proxy xs

reifyConstraint0
  :: EachHas c rs
  => proxy c
  -> Rec P rs
  -> Rec (Dict0 c) rs
reifyConstraint0 proxy = \case
 RNil -> RNil
 (Proxy :& xs) -> Dict0 :& reifyConstraint0 proxy xs

type Dict1 = Dict

data Dict0 c a where
  Dict0 :: c a => Dict0 c a

type family Replicate (n :: Nat) (a :: *) :: [*] where
  Replicate 0 _a = '[]
  Replicate n a  = a ': Replicate (n-1) a

type Vec n a = Rec I (Replicate n a)

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


{-|

@
@

e.g.

@
marshall
 :: (Marshall Storeable Storeable IO Ptr, RecAll Storeable input, Storeable output)
 =>     Function I   name input output
 -> IO (Function Ptr name input output)
@

@
marshall
 :: (Marshall ToJSON FromJSON Maybe (Const JSON), RecAll FromJSON input, ToJSON output)
 =>        Function I            name input output
 -> Maybe (Function (Const JSON) name input output)
@

-}

{-
marshall
 :: (Marshall into_f from_f m f, RecAll from_f input, into_f output)
 => Function I I name input output
 -> Function m f name input output
marshall (Function function) = Function \inputs -> do
  _inputs <- rtraverse (from) inputs
  let _output = function _inputs
  output <- (into) _output
  return output
  -}
