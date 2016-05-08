{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, ConstraintKinds, GADTs #-}
{-| TODO example

related:

* <https://github.com/nh2/call-haskell-from-anything call-haskell-from-anything>

* <https://haskell-servant.github.io/ servant>

-}
module Export
 ( module Export.Marshall
 , module Export.Function
 , module Export.Curry
 ) where

import Export.Marshall
import Export.Function
import Export.Curry
