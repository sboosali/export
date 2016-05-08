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

import Text.Read (readMaybe)

main :: IO ()
main = do

 print $ tLength (P::P '[Int,String])

 let cLessThan_mono = (<) :: Int -> Int -> Bool
 let uLessThan_mono = rUncurry cLessThan_mono
 print $ uLessThan_mono (0 &: 1 &: RNil)

 let cLessThan_poly = (<) :: Ord a => a -> a -> Bool
 let uLessThan_poly = rUncurry cLessThan_poly
 print $ uLessThan_poly ((0::Int) &: 1 &: RNil)

 print $ getInputs    cLessThan_mono
 print $ getOutput    cLessThan_mono
 print $ getSignature cLessThan_mono
 print $ getArity     cLessThan_mono

 print $ ('a' :* 'b' :* 'c' :* RNil :: Vec 3 Char)

 let rs = (C "False" :& C "0" :& Z) :: Rec (C String) [Bool,Integer]
 print $ rtraverseFrom (P::P Read) (getConst >>> readMaybe) rs

 let hs_or = newFunction (P::P "or") (||)
 -- inferred :: Function I I "or" [Bool,Bool] Bool
 print $ hs_or `call` (False :* True :* Z)

 print $ (hs_or & functionName)

 let hs_String_or = marshalled hs_or
 -- inferred :: Function Maybe (C String) "or" [Bool,Bool] Bool
 print $ hs_String_or `call_` (C "False" :& C "True" :& Z)
 print $ (fmap getConst . call hs_String_or) (C "False" :& C "True" :& Z)
 -- Just "True"
