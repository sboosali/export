{-# LANGUAGE DataKinds #-}
module Export.Main where
import Export

import Data.Proxy

main :: IO ()
main = do
 print $ tLength (Proxy :: Proxy [Int,String])
