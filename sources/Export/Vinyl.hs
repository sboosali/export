{-# LANGUAGE DataKinds, PolyKinds, PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds, GADTs, RankNTypes #-}
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

import Data.Functor.Product
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

type (:*:) = Product

pattern I :: a -> Identity a
pattern I x = Identity x

pattern C :: forall a (b :: k). a -> Const a b
pattern C x = Const x

pattern P :: forall (a :: k). Proxy a
pattern P = Proxy

pattern (:*:) :: f a -> g a -> Product f g a
pattern f :*: g = (Pair f g)

-- "Number of parameters must match family declaration"
type family Length (as :: [k]) :: Nat where
 Length '[] = 0
 Length (_a ': as) = 1 + Length as -- UndecidableInstances

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

reifyConstraint2
  :: EachHas c rs
  => proxy c
  -> Rec f rs
  -> Rec (f :*: Dict0 c) rs
reifyConstraint2 proxy = \case
 RNil -> RNil
 (x :& xs) -> (x :*: Dict0) :& reifyConstraint2 proxy xs

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

-- {-| a restricted 'rtraverse', for convenience.
--
-- -}
-- rtraverse'
--  :: ( Applicative h
--     , EachHas c rs
--     )
--  => (forall x. (c x) => x -> h (g x))
--  -> proxy c
--  -> Rec I rs
--  -> h (Rec g rs)
-- rtraverse' u rs = rtraverse _u _rs
--  where
--  _rs = reify rs
--  _u = u

-- {-| like 'rtraverse', for convenience.
--
-- -}
-- rtraverse'
--  :: ( Applicative h
--     , EachHas c rs
--     )
--  => (forall x. (c (f x)) => f x -> h (g x))
--  -> proxy c
--  ->    Rec f rs
--  -> h (Rec g rs)
-- rtraverse' u rs = rtraverse _u _rs
--  where
--  _rs = reify rs
--  _u = u

{-| a restricted 'rtraverse', for convenience.

e.g.

>>> let rs = (C "False" :& C "0" :& Z) :: Rec (C String) [Bool,Integer]
>>> rtraverseFrom (P::P Read) readMaybe rs  -- 'readMaybe'
{False, 0}

-- i.e. @Just (I False :& I 0 :& Z)@

specializing:

@
rtraverse'
 :: ( Applicative Maybe
    , EachHas Read [Bool,Integer]
    )
 => (forall x. (Show x => String -> Maybe x)
 -> Proxy Read
 ->        Rec (C String) [Bool,Integer]
 -> Maybe (Rec I          [Bool,Integer])
@

-}
rtraverseFrom
 :: forall c f rs h proxy.
    ( Applicative h
    , EachHas c rs  --or Marshall
    )
 => proxy c
 -> (forall x. (c x) => f x -> h x)
 ->    Rec f rs
 -> h (Rec I rs)
rtraverseFrom proxy u rs = rtraverse _u _rs
 where
 _u :: forall x. (f :*: Dict0 c) x -> h (I x)
 _u (Pair fa Dict0) = I <$> u fa -- match on Dict0 exposes the @c x@
 _rs = reifyConstraint2 proxy rs
 -- _u (Const a :*: Dict0) = I <$> u a -- match on Dict0 exposes the @c x@
 --_u (Const a) = I (u a)
