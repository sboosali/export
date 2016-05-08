{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, KindSignatures #-}
module Export.Curry where
import Export.Types

import Data.Vinyl


{-| Uncurry any function (i.e. of any arity, zero or more).

Read @(RUncurry function inputs output)@ as
"the @function@ uncurries into these @inputs@ and this @output@".

relate a (curried) function to its inputs and output.
"left-associates" the right-associative @(->)@.

the uncurried function is strict in each input, which is okay,
given its use by "Function" (i.e. receiving external arguments, w
hich even after marshalling are probably evaluated).

>>> let uLessThan = rUncurry (<)
>>> :t (<)
Ord a => a -> a -> Bool
>>> :t uLessThan
Ord a => Rec Identity [a,a] -> Bool
>>> uLessThan (0 :& 1 :& RNil) == (<) 0 1  -- same order
True

>>> rUncurry "value" RNil == "value"
True

@
rcurry :: Curry input output -> (Rec Id input -> output)
@

naming: "record uncurry".

-}
class (inputs ~ Inputs function, output ~ Output function) =>
 RUncurry (function :: *) (inputs :: [*]) (output :: *) where
  rUncurry :: function -> (Rec I inputs -> output)

-- | if you can uncurry a function into its inputs and its output,
-- then you can uncurry that function with one extra input too
-- (i.e. the induction).
instance (RUncurry function inputs output) => RUncurry (input -> function) (input ': inputs) output where
  rUncurry function = \(Identity input :& inputs) -> rUncurry (function input) inputs

-- | a value uncurries to itself (i.e. the base case).
instance (Inputs value ~ '[], Output value ~ value) => RUncurry value '[] value where
  rUncurry value = \RNil -> value


{-|

>>> :kind! Inputs (Int -> String -> Bool)
[Int,String]

-}
type family Inputs (function :: k) :: [*] where
  Inputs (value -> function) = value ': Inputs function
  Inputs _value              = '[]

{-|

>>> :kind! Output (Int -> String -> Bool)
Bool

-}
type family Output (function :: k) :: * where
  Output (_value -> function) = Output function
  Output value                = value


-- | specializations. possibly useful for type inference / cleaner build error messages / being explicit.
-- uncurry2 :: (a -> b -> r) -> (Rec I [a,b] -> r)
-- uncurry2 = rUncurry
--
-- uncurry3 :: (a -> b -> c -> r) -> (Rec I [a,b,c] -> r)
-- uncurry3 = rUncurry
--
-- uncurry4 :: (a -> b -> c -> d -> r) -> (Rec I [a,b,c,d] -> r)
-- uncurry4 = rUncurry


{-| "record curry".

@
rcurry :: (Inputs function -> Output function) -> function
@

rcurry :: (HList input -> output) -> Curry input output
rcurry function = undefined
-}
