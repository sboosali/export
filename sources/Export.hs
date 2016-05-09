{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, ConstraintKinds, GADTs #-}
-- | "export" haskell functions into:
--
-- * servers
-- * command-line interfaces
-- * C foreign functions
-- * and more
--
-- example:
--
-- > {-# LANGUAGE DataKinds, ConstraintKinds #-}
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
--
-- @
-- import Export
-- import Foreign
-- import qualified Data.ByteString.Lazy.Char8 as B
-- @
-- @
-- -- 1. uncurry any haskell function
-- u_or = 'newFunction' ('P'::P "or") (||)
--
-- -- :: Function I I "or" '[Bool, Bool] Bool
-- -- (inferred)
-- @
-- @
-- -- 2. make it marshallable
-- main = do
--
--  -- curried:
--
--  print $ (||) False True
--  -- ==> True
--
--  -- un-marshalled:
--
--  print $ u_or  \`call`  ('I' False :& I True :& Z)  -- 'call'
--  -- ==> True
--
--  -- marshalled via 'String':
--  let hs_String_or = 'stringMarshaller' \`marshalling` u_or   -- 'marshalling'
--
--  print $ hs_String_or \`call_` ('C' \"False\" :& C \"True\" :& Z)  -- 'call_'
--  -- ==> Just \"True"
--
--  -- marshalled via JSON ('B.ByteString'):
--  let hs_JSON_or = jsonMarshaller \`marshalling` u_or
--
--  print $ hs_JSON_or \`call_` (B.pack "false" ':#' B.pack "true" :# Z)
--  -- ==> Right \"true"
--
--  -- marshalled via "Foreign.Ptr":
--  let hs_Ptr_or = 'pointerMarshaller' \`marshalling` u_or
--
--  pFalse <- 'new' False
--  pTrue  <- new True
--  pOr    <- hs_Ptr_or \`call` (pFalse ':&' pTrue :& Z)
--  print =<< 'peek' pOr
--  -- ==> True
-- @
-- @
-- 3. \"export\" it
-- TODO
-- @
--
-- related:
--
-- * <https://github.com/nh2/call-haskell-from-anything call-haskell-from-anything>
--
-- * <https://haskell-servant.github.io/ servant>
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
