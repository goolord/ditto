{-# LANGUAGE 
    BangPatterns
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses 
  , OverloadedStrings
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
  , Result(..)
  ) where

import Control.Applicative (Alternative(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Torsor
import qualified Data.Text as T

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
  p <> "-val-" <> (T.intercalate "." $ foldr (\a as -> T.pack (show a) : as) [] xs)
encodeFormId (FormIdName x _) = x

-- | get the head 'Int' from a 'FormId'
formIdentifier :: FormId -> Int
formIdentifier (FormId _ (x :| _)) = x
formIdentifier (FormIdName _ x) = x

instance Torsor FormId Int where
  add i (FormId p (x :| xs)) = FormId p $ (x + i) :| xs
  add i (FormIdName n x) = FormIdName n $ x + i 
  difference a b = formIdentifier a - formIdentifier b

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
data Result e ok
  = Error [(FormRange, e)]
  | Ok ok
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Proved records a value, the location that value came from, and something that was proved about the value.
data Proved a = Proved
  { pos :: FormRange
  , unProved :: a
  } deriving (Show, Functor, Foldable, Traversable)
