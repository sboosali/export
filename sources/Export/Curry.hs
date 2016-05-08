{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, KindSignatures #-}
module Export.Curry where
import Export.Types

import Data.Vinyl


{-| Uncurry any function (i.e. of any arity, zero or more).

Read @(RUncurry function inputs output)@ as
"the @function@ uncurries into these @inputs@ and this @output@".

multi parameter type classes are "relations": thic class relates
a (curried) function to its inputs and output.
it "left-associates" the right-associative @(->)@.

the uncurried function is strict in each input, which is okay,
given its use by "Function" (i.e. receiving external arguments, w
hich even after marshalling are probably evaluated).

>>> :t (<)
Ord a => a -> a -> Bool
>>> let uLessThan = rUncurry (<)
>>> :t uLessThan
Ord a => Rec Identity [a,a] -> Bool
>>>  (<) 0 1  ==  uLessThan (0 :* 1 :* RNil)  -- same order too
True

>>> rUncurry "value" RNil == "value"
True

@
TODO:
rCurry :: Curry input output -> (Rec Id input -> output)
@

naming: "record uncurry".

(instances don't use `OverlappingInstances`,
because they perform induction on the `inputs` type).

-}
class (inputs ~ Inputs function, output ~ Output function) =>
 RUncurry (function :: *) (inputs :: [*]) (output :: *) where
  rUncurry :: function -> (Rec I inputs -> output)

{-| if you can uncurry a function into its inputs and its output
(@RUncurry function inputs output@),
then you can uncurry that function with one extra input too
(@RUncurry (input -> function) (input ': inputs) output)@.

i.e. the induction.

-}
instance (RUncurry function inputs output) => RUncurry (input -> function) (input ': inputs) output where
  rUncurry function = \(Identity input :& inputs) -> rUncurry (function input) inputs

{- | a value uncurries to itself
(@(Inputs value ~ '[], Output value ~ value) => RUncurry value '[] value@).

i.e. the base case.
-}
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
