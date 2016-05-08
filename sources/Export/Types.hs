{-# LANGUAGE DataKinds, PatternSynonyms, PolyKinds #-}
{-|


kwargs: named/defaulting arguments

callKwargs
 :: (bs \in as, RecALl Default (as \diff bs))
 => Funtion ... as c -> (bs -> c)

class Default function_name argument_name argument_type where
 def'


f :: Int -> String -> Bool
f x y = ...
>>> liftFunction 'f
==>
f' :: Function Id "f" [Int,String] Bool
f' = ...

NOTE needs f's signature, otherwise:

>>> liftFunction 'f [''Int,''String]
==>
f' :: Function Id "f" [Int,String] Bool


g :: Int -> String -> Bool
g x y = ...
>>> makeFunctionKwargs 'g
==>
-- | takes @x :: 'Int'@ and @y :: 'String'@
g' :: Function Id Field "g" ["x" ::: Int, "y" ::: String] Bool
g' = ...

TODO th needs function declaration introspection and haddock gen, otherwise:
>>> makeFunctionKwargs 'g ["x", "y"]
==>
g' :: Function Id Field "g" ["x" ::: Int, "y" ::: String] Bool


optinally-optional arguments
 (meta :: Rec Maybe input) -- value-level
 (docstring :: String) -- value-levelm unlike (name :: Symbol)


-}
module Export.Types
 ( module Export.Types
 , Identity(..)
 ) where
--import Export.Extra

import Data.Vinyl
import Data.Vinyl.Functor
--import Control.Monad.Catch (MonadThrow(..))

--import Control.Applicative (Const(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol)

{-|

@
Marshall from into m f
@

naming:

* @f@: a functor. the foreign/serialized/persisted type. parametrized over @a@ for possible type-safety.
* @m@: a monad that the marshalling can use and fail in.
* @from@: from @f@
* @into@: into @f@
* @a@: anything that safisfies the constraints. the type to be marshalled.

-}
{-
class (MonadThrow m) => Marshall from into m f
 from :: (from a) => f a -> m a
 into :: (into a) => a   -> m (f a)

into_ :: (Marshall from into m (C b), into a) => a -> m b
into_ = into >>> fmap getConst

from_ :: (Marshall from into m (C b), from a) => b -> m a
from_ = Const >>> from

{-
instance Marshall

instance Marshall Storeable Storeable IO Ptr

instance Marshall FromJSON ToJSON (Either String) (C JSON)
-}

instance Marshall Show Read Maybe (C String)
 into :: (Show a) => a -> Maybe (C String a)
 into = return . Const . show
 from :: (Read a) => C String a -> Maybe a
 from = maybe (throwM "") return . readMay . getConst
-}

--------------------------------------------------------------------------------

{-| an effectful, uncurried, named function.

@
Function m f name inputs output
@

naming:

* @m@: a monad
* @f@: a functor
* @name@: the name
* @input@:
* @output@:

@name@ is type-level (not value-level) to support static validation
(e.g. checking that the 'Symbol' a valid python identifier).

-}
data Function
 (m      :: * -> *)
 (f      :: * -> *)
 (name   :: Symbol)
 (inputs :: [*])
 (output :: *)
  -- docstring? general metadata.

  = Function (Rec f inputs -> m (f output))

  deriving (Functor)

{-| no effects, any inputs.

-}
type HaskellFunction = Function I I

type I = Identity

type C = Const

type P = Proxy

(&:) :: a -> Rec I as -> Rec I (a ': as)
(&:) x xs = Identity x :& xs
infixr 7 &:

pattern (:*) :: a -> Rec I as -> Rec I (a ': as)
pattern (:*) x xs = Identity x :& xs
infixr 7 :*

pattern Z :: forall (f :: k -> *). Rec f '[]
pattern Z = RNil

pattern P :: forall (a :: k). Proxy a
pattern P = Proxy

pattern I :: a -> Identity a
pattern I x = Identity x
