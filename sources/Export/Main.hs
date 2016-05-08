{-# LANGUAGE DataKinds, ConstraintKinds, NoMonomorphismRestriction #-}
{- |

@
stack build && stack exec export-example
@

(you can read the source as an example.)

(exported for the `executable`.)

-}
module Export.Main where
import Export
--import Export.Vinyl
import Export.Extra

import Foreign
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do

 putStrLn "\nrUncurry..."
 let cLessThan_mono = (<) :: Int -> Int -> Bool
 let uLessThan_mono = rUncurry cLessThan_mono
 print $ uLessThan_mono (0 :* 1 :* RNil)

 let cLessThan_poly = (<) :: Ord a => a -> a -> Bool
 let uLessThan_poly = rUncurry cLessThan_poly
 print $ uLessThan_poly ((0::Int) :* 1 :* RNil)

 putStrLn "\ngetSignature..."
 print $ tLength (P::P '[Int,String])
 print $ getSignature cLessThan_mono
 print $ getInputs    cLessThan_mono
 print $ getOutput    cLessThan_mono
 print $ getArity     cLessThan_mono

 putStrLn "\n(:*)..."
 print $ ('a' :* 'b' :* 'c' :* RNil :: Vec 3 Char)

 putStrLn "\nrtraverseFrom..."
 let rs = (C "True" :& C "1" :& Z) :: Rec (C String) [Bool,Integer]
 print $ rtraverseFrom (P::P Read) (getConst >>> readMaybe) rs

 putStrLn "\nnewFunction..."
 let u_or = newFunction (P::P "or") (||)
 -- inferred :: Function I I "or" [Bool,Bool] Bool
 print $ u_or `call` (False :* True :* Z)

 print $ (u_or & functionName)

 -- let hs_or = marshalled u_or

 putStrLn "\nString-marshalled..."
 let hs_String_or = marshalled u_or
 -- inferred :: Function Maybe (C String) "or" [Bool,Bool] Bool
 print $ hs_String_or `call_` ("False" :# "True" :# Z)
 -- Just "True"

 putStrLn "\nJSON-marshalled..."
 let hs_JSON_or = marshalled u_or
 -- inferred :: Function (Either String) (C ByteString) "or" [Bool,Bool] Bool
 print $ hs_JSON_or `call_` (B.pack "false" :# B.pack "true" :# Z)
 -- Right "true"

 putStrLn "\nPtr-marshalled..."
 let hs_Ptr_or = marshalled u_or
 -- inferred :: Function IO Ptr "or" [Bool,Bool] Bool
 pFalse <- new False
 pTrue  <- new True
 pOr    <- hs_Ptr_or `call` (pFalse :& pTrue :& Z)
 print pOr
 print =<< peek pOr
