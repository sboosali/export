{-# LANGUAGE DataKinds, PolyKinds, PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds, GADTs #-}
{-

(re-exports and some extensions).

-}
module Export.Vinyl
  ( module Export.Vinyl
  , module Data.Vinyl
  , module Data.Vinyl.Functor
  ) where

import Data.Vinyl
import Data.Vinyl.Functor
--import Data.Vinyl.TypeLevel hiding (Nat(..))

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy


(&:) :: a -> Rec I as -> Rec I (a ': as)
(&:) x xs = Identity x :& xs
infixr 7 &:

pattern (:*) :: a -> Rec I as -> Rec I (a ': as)
pattern (:*) x xs = Identity x :& xs
infixr 7 :*

pattern Z :: forall (f :: k -> *). Rec f '[]
pattern Z = RNil

type I = Identity

type C = Const

type P = Proxy

pattern I :: a -> Identity a
pattern I x = Identity x

pattern C :: forall a (b :: k). a -> Const a b
pattern C x = Const x

pattern P :: forall (a :: k). Proxy a
pattern P = Proxy

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

rproxy :: RecApplicative as => Rec Proxy as
rproxy = rpure Proxy

-- rdict :: RecApplicative as => Rec (Dict0 c) as
-- rdict = reifyConstraint0

type family EachHas (c :: * -> Constraint) (rs :: [u]) :: Constraint where
  EachHas c '[] = ()
  EachHas c (r ': rs) = (c r, EachHas c rs)

type Dict1 = Dict

data Dict0 c a where
  Dict0 :: c a => Dict0 c a

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

type family Replicate (n :: Nat) (a :: *) :: [*] where
  Replicate 0 _a = '[]
  Replicate n a  = a ': Replicate (n-1) a

type Vec n a = Rec I (Replicate n a)
