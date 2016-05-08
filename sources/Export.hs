{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, ConstraintKinds, GADTs #-}
{-| "export" haskell functions into:

* servers
* command-line interfaces
* C foreign functions
* and more

example:

@
import Export

-- 1. uncurry any haskell function
u_or = newFunction (P::P "or") (||)
-- :: Function I I "or" '[Bool, Bool] Bool
-- (inferred)

-- 2. make it marshallable
hs_or = marshalled u_or
-- :: Function Maybe (Const String) "or" '[Bool, Bool] Bool
-- (inferred, specialized)

-- 3. call it
main = do

 print $ u_or  '`call`'  ('I' False :& I True :& Z)
 -- Identity True

 print $ hs_or '`call_`' ('C' "False" :& C "True" :& Z)
 -- Just "True"

-- 4. \"export\" it
TODO
@

related:

* <https://github.com/nh2/call-haskell-from-anything call-haskell-from-anything>

* <https://haskell-servant.github.io/ servant>

-}
module Export
 ( module Export.Marshall
 , module Export.Function
 , module Export.Curry
 , module Export.Vinyl
 ) where

import Export.Marshall
import Export.Function
import Export.Curry
import Export.Vinyl
