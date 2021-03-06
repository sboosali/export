{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, ConstraintKinds, GADTs #-}
-- | "export" haskell functions into:
--
-- * servers
-- * command-line interfaces
-- * C foreign functions
-- * and more
--
-- *
--
-- Example:
--
-- > {-# LANGUAGE DataKinds, ConstraintKinds #-}
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
--
-- @
-- import Export
-- import Foreign
-- import qualified Data.ByteString.Lazy.Char8 as B
-- @
--
-- **
--
-- First, we uncurry the function:
--
-- @
-- hs_or = 'newFunction' ('P'::P "or") (||)
-- -- (inferred) hs_or :: Function 'I' I "or" '[Bool, Bool] Bool
-- @
--
-- which means:
--
-- * @hs_or@ takes two bools to a bool (@'[Bool, Bool]@ is a type-level list, which are n-ary tuples).
-- * it's named @"or"@ (which information is available at the type level)
-- * each input lives in the identity monad.
-- * the output also lives in the identity monad.
--
-- (the @hs_@ prefix implies it's being exported, as a @c_@ implies we're importing a foreign C function).
-- 
-- 'newFunction' works on any Haskell function (any arity, any types).
-- 
-- We can call normal(/curried) functions (obviously):
--
-- >>> (||) False True
-- True
--
-- Likewise, we can call the uncurried 'Function':
--
-- >>> call hs_or (I False :& I True :& Z)  -- see 'call', 'I', ':&', 'Z'
-- True
--
-- Or more readably:
--
-- >>> hs_or \`call` (False :* True :* Z)    -- see 'call', ':*'
-- True
--
-- @(False :* True :* Z)@ is an unlabelled record @{False, True}@.
--
-- ** Second, we make the function marshallable.
--
-- *** Marshall via 'String':
--
-- @
--  let hs_String_or = 'stringMarshaller' \`marshalling` hs_or   -- 'marshalling'
--
--  print $ hs_String_or \`call_` ('C' \"False\" :& C \"True\" :& Z)  -- 'call_'
--  -- ==> Just \"True"
--
--  -- marshalled via JSON ('B.ByteString'):
--  let hs_JSON_or = jsonMarshaller \`marshalling` hs_or
--
--  print $ hs_JSON_or \`call_` (B.pack "false" ':#' B.pack "true" :# Z)
--  -- ==> Right \"true"
--
--  -- marshalled via "Foreign.Ptr":
--  let hs_Ptr_or = 'pointerMarshaller' \`marshalling` hs_or
--
--  pFalse <- 'new' False
--  pTrue  <- new True
--  pOr    <- hs_Ptr_or \`call` (pFalse ':&' pTrue :& Z)
--  print =<< 'peek' pOr
--  -- ==> True
-- @
--
-- @
-- 3. \"export\" it
-- TODO
-- @
--
-- related:
--
-- * <https://haskell-servant.github.io/ servant>
-- * <https://github.com/nh2/call-haskell-from-anything call-haskell-from-anything>
-- * <http://hackage.haskell.org/package/optparse-generic-1.1.0/docs/Options-Generic.html optparse-generic>
-- * <http://hackage.haskell.org/package/server-generic-1.0.0/docs/Server-Generic.html server-generic>
-- * <https://hackage.haskell.org/package/typed-spreadsheet-1.1.0/docs/Typed-Spreadsheet.html typed-spreadsheet>
-- * < >
--
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
