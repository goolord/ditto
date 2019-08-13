{-# LANGUAGE 
    BangPatterns
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses 
  , OverloadedStrings
#-}

module Ditto.Types where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Torsor
import qualified Data.Text as T

-- | An ID used to identify forms
data FormId
  = FormId
      {-# UNPACK #-} !Text           -- ^ Global prefix for the form
      {-# UNPACK #-} !(NonEmpty Int) -- ^ Stack indicating field. Head is most specific to this item
  | FormIdCustom 
      {-# UNPACK #-} !Text -- ^ Local name of the input
      {-# UNPACK #-} !Int  -- ^ Index of the input
  deriving (Eq, Ord, Show)

encodeFormId :: FormId -> Text
encodeFormId (FormId p xs) =
  p <> "-val-" <> (T.intercalate "." $ foldr (\a as -> T.pack (show a) : as) [] xs)
encodeFormId (FormIdCustom x _) = x

-- | get the head 'Int' from a 'FormId'
formIdentifier :: FormId -> Int
formIdentifier (FormId _ (x :| _)) = x
formIdentifier (FormIdCustom _ x) = x

instance Torsor FormId Int where
  add i (FormId p (x :| xs)) = FormId p $ (x + i) :| xs
  add i (FormIdCustom n x) = FormIdCustom n $ x + i 
  difference a b = formIdentifier a - formIdentifier b

-- | A range of ID's to specify a group of forms
data FormRange
  = FormRange FormId FormId
  deriving (Eq, Show)

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

instance Semigroup a => Semigroup (Value a) where
  Missing <> Missing = Missing
  Default <> Missing = Default
  Missing <> Default = Default
  Default <> Default = Default
  Found x <> Found y = Found (x <> y)
  Found x <> _ = Found x
  _ <> Found y = Found y

-- | Type for failing computations
data Result e ok
  = Error [(FormRange, e)]
  | Ok ok
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Proved records a value, the location that value came from, and something that was proved about the value.
data Proved a = Proved
  { pos :: FormRange
  , unProved :: a
  } deriving (Show, Functor, Foldable, Traversable)
