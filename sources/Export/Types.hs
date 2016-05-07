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
module Export.Types where
import Export.Extra()

--import Control.Monad.Catch (MonadThrow(..))

--import Control.Applicative (Const(..))

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

instance Marshall

instance Marshall Storeable Storeable IO Ptr

instance Marshall FromJSON ToJSON (Either String) (C JSON)

instance Marshall Show Read Maybe (C String)
 into :: (Show a) => a -> Maybe (C String a)
 into = return . Const . show
 from :: (Read a) => C String a -> Maybe a
 from = maybe (throwM "") return . readMay . getConst
-}

--------------------------------------------------------------------------------

{-| an effectful, uncurried, named function.

@
Function f name input output
@

naming:

* @m@: a monad
* @f@: a functor
* @name@:
* @input@:
* @output@:

-}
{-
data Function
 (m      :: * -> *)
 (f      :: * -> *)
 (name   :: Symbol)
 (input  :: [*])
 (output :: *)
  -- docstring?
  = Function (Rec f input -> m (f output))
  deriving (Functor)

instance Profunctor (Function m f name) where
 lmap before (Function function) = Function (before   >>> function)
 rmap after  (Function function) = Function (function >>> fmap after)
-}

{-| no effects, any inputs.

type HaskellFunction = Function I I

type I = Identity

type C = Const
-}
