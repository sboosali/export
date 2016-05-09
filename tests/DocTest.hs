{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest

main = do
 doctest
  [ "sources/Export/Marshall.hs"
  , "sources/Export/Function.hs"
  -- , "sources/Export/Curry.hs" -- parses inst decls as doctests??
  ]
 -- extensions declared in .cabal are ignored by doctest
