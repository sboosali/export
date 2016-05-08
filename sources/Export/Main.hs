{-# LANGUAGE DataKinds #-}
{- |

@
stack build && stack exec export-example
@

(you can read the source as an example.)

(exported for the `executable`.)

-}
module Export.Main where
import Export
--import Export.Types
import Export.Vinyl
import Export.Curry
import Export.Extra

import Data.Proxy

main :: IO ()
main = do

 print $ tLength (P::P '[Int,String])

 let cLessThan_mono = (<) :: Int -> Int -> Bool
 let uLessThan_mono = rUncurry cLessThan_mono
 print $ uLessThan_mono (0 &: 1 &: RNil)

 let cLessThan_poly = (<) :: Ord a => a -> a -> Bool
 let uLessThan_poly = rUncurry cLessThan_poly
 print $ uLessThan_poly ((0::Int) &: 1 &: RNil)

 print $ tInputs    cLessThan_mono
 print $ tOutput    cLessThan_mono
 print $ tSignature cLessThan_mono

 print $ ('a' :* 'b' :* 'c' :* RNil :: Vec 3 Char)

 let hs_or = newFunction (P::P "or") (||)
 print $ hs_or `call` (False :* True :* Z)
  -- (         False :*          True :* Z   )
  -- (         False :*          True :* RNil)
  -- (I        False :& I        True :& RNil)
  -- (Identity False :& Identity True :& RNil)
 print $ (hs_or & functionName)
