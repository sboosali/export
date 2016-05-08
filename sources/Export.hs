{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, ConstraintKinds, GADTs #-}
{-| "export" haskell functions into:

* servers
* command-line interfaces
* C foreign functions
* and more

example:

@
{- LANGUAGE DataKinds, ConstraintKinds #-}
{- LANGUAGE NoMonomorphismRestriction #-}

import Export
import Foreign

-- 1. uncurry any haskell function
u_or = 'newFunction' ('P'::P "or") (||)

-- :: Function I I "or" '[Bool, Bool] Bool
-- (inferred)

-- 2. make it marshallable
hs_or = 'marshalled' u_or

-- :: Function Maybe (Const String) "or" '[Bool, Bool] Bool
-- (inferred, specialized)

-- 3. call it
main = do

-- unmarshalled:

 print $ u_or  `call`  ('I' False :& I True :& Z)  -- 'call'
 -- ==> True

 -- marshalled via 'String':

 print $ hs_or `call_` ('C' \"False\" :& C \"True\" :& Z)  -- 'call_'
 -- ==> Just "True"

 -- marshalled via 'Ptr':

 pFalse <- 'new' False
 pTrue  <- new True
 pOr    <- hs_or `call_` (pFalse :& pTrue :& Z)
 print =<< 'peek' pOr
 -- ==> True



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

--TODO heterogeneous arguments in example
