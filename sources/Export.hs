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

import Data.Vinyl
import Data.Vinyl.Functor
--import Data.Vinyl.TypeLevel hiding (Nat(..))

import GHC.TypeLits
--import Control.Arrow (Kleisli(..))
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
tLength _ = natVal (Proxy :: Proxy (Length as))


tInputs
 :: forall function. ( EachHas Typeable (Inputs function)
                     , RecApplicative (Inputs function)
                     )
 => function
 -> [TypeRep]
tInputs _ = recordToList ts
 where
 ts = rmap ftypeRep ds      :: Rec (Const TypeRep)  (Inputs function)
 ds = reifyConstraint0 pTypeable ps   :: Rec (Dict0 Typeable) (Inputs function)
 ps = rproxy                :: Rec Proxy            (Inputs function)
 pTypeable = (Proxy::Proxy Typeable)
 ftypeRep :: forall a. Dict0 Typeable a -> Const TypeRep a
 ftypeRep Dict0 = typeRep (Proxy::Proxy a) & Const

tOutput :: forall function. (Typeable (Output function)) => function -> TypeRep
tOutput _ = typeRep (Proxy::Proxy (Output function))

tSignature :: forall function. (Typeable function) => function -> TypeRep
tSignature _ = typeRep (Proxy::Proxy function)

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

newFunction
 :: proxy name
 -> function
 -> (HaskellFunction name (Input function) (Output function))
newFunction _ function
 = Function $ runcurry function

fromKleisli
 :: proxy name
 -> Kleisli m a b
 -> Function m I name '[a] b
fromKleisli _ (Kleisli function)
 = Function $ runcurry function

-- Curry (Rec f input) (m (f output))
functionBody :: Function m f name input output -> (Rec f input -> m (f output))
functionBody (Function body) = rcurry body

functionName :: Function m f name input output -> Tagged String name
functionName = Tagged (symbolVal (name :: Proxy name)) -- reifySymbol
-}
