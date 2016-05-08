{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances #-}
{-|

related:

* <https://github.com/nh2/call-haskell-from-anything call-haskell-from-anything>

* <https://haskell-servant.github.io/ servant>

-}
module Export where
--import Export.Types
--import Curry

import GHC.TypeLits
--import Control.Arrow (Kleisli(..))
import Data.Proxy


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


-- tInputs
--
-- tOutput


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
