{-# LANGUAGE 
    BangPatterns
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses 
  , OverloadedStrings
  , PatternSynonyms
  , ExplicitForAll
#-}

-- | Types relevant to forms and their validation.
module Ditto.Types (
  -- * FormId
    FormId(..)
  , FormRange(..)
  , encodeFormId
  , formIdentifier
  -- * Form result types
  , Value(..)
  , View(..)
  , Proved(..)
  , Result(.., Error, Ok)
  ) where

import Control.Applicative (Alternative(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

------------------------------------------------------------------------------
-- FormId
------------------------------------------------------------------------------

-- | An ID used to identify forms
data FormId
  = FormId
      {-# UNPACK #-} !Text           -- ^ Global prefix for the form
      {-# UNPACK #-} !(NonEmpty Int) -- ^ Stack indicating field. Head is most specific to this item
  | FormIdName 
      {-# UNPACK #-} !Text -- ^ Local name of the input
      {-# UNPACK #-} !Int  -- ^ Index of the input
  deriving (Eq, Ord, Show)

instance IsString FormId where
  fromString x = FormIdName (T.pack x) 0

-- | Encoding a @FormId@: use this instead of @show@ for
-- the name of the input / query string parameter
encodeFormId :: FormId -> Text
encodeFormId (FormId p xs) =
  let ids = fmap (T.pack . show) (NE.toList xs)
  in p <> "-val-" <> T.intercalate "." ids
encodeFormId (FormIdName x _) = x

-- | get the head 'Int' from a 'FormId'
formIdentifier :: FormId -> Int
formIdentifier (FormId _ (x :| _)) = x
formIdentifier (FormIdName _ x) = x

-- | A range of ID's to specify a group of forms
data FormRange
  = FormRange FormId FormId
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Form result types
------------------------------------------------------------------------------

-- | views, values as a result of the environment, etc.

-- | Function which creates the form view
newtype View err v = View { unView :: [(FormRange, err)] -> v }
  deriving (Semigroup, Monoid, Functor)

-- | used to represent whether a value was found in the form
-- submission data, missing from the form submission data, or expected
-- that the default value should be used
data Value a
  = Default
  | Missing
  | Found a
  deriving (Eq, Show, Functor, Traversable, Foldable)

instance Applicative Value where
  pure = Found
  (Found f) <*> (Found x) = Found (f x)
  Default <*> _ = Default
  Missing <*> _ = Missing
  Found{} <*> Default = Default
  Found{} <*> Missing = Default

instance Alternative Value where
  empty = Missing
  x@Found{} <|> _ = x
  Default <|> _ = Default
  Missing <|> x = x

instance Semigroup a => Semigroup (Value a) where
  Missing <> Missing = Missing
  Default <> Missing = Default
  Missing <> Default = Default
  Default <> Default = Default
  Found x <> Found y = Found (x <> y)
  Found x <> _ = Found x
  _ <> Found y = Found y

-- | Type for failing computations
-- Similar to @Either@ but with an accumilating @Applicative@ instance
newtype Result e ok = Result { getResult :: Either [(FormRange, e)] ok }
  deriving (Show, Eq, Functor, Foldable, Traversable, Monad)

pattern Error :: forall e ok. [(FormRange, e)] -> Result e ok
pattern Error e = Result (Left e)
pattern Ok :: forall e ok. ok -> Result e ok
pattern Ok ok = Result (Right ok)
{-# COMPLETE Error, Ok #-}

instance Applicative (Result e) where
  pure = Ok
  Error x <*> Error y = Error $ x ++ y
  Error x <*> Ok _ = Error x
  Ok _ <*> Error y = Error y
  Ok x <*> Ok y = Ok $ x y

-- | Proved records a value, the location that value came from, and something that was proved about the value.
data Proved a = Proved
  { pos :: FormRange
  , unProved :: a
  } deriving (Show, Functor, Foldable, Traversable)

