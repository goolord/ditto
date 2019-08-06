{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module for the core result type, and related functions
--
module Ditto.Result
  ( Result (..)
  , getResult
  , FormId (..)
  , zeroId
  , mapId
  , FormRange(..)
  , incrementFormId
  , unitRange
  , isInRange
  , isSubRange
  , retainErrors
  , retainChildErrors
  , formIdentifier
  )
where

import Control.Applicative (Applicative (..))
import Data.List (intercalate, foldl')
import Data.List.NonEmpty (NonEmpty(..))
import Torsor
import qualified Data.List.NonEmpty as NE

-- | Type for failing computations
data Result e ok
  = Error [(FormRange, e)]
  | Ok ok
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Monad (Result e) where
  return = Ok
  Error x >>= _ = Error x
  Ok x >>= f = f x

instance Applicative (Result e) where
  pure = Ok
  Error x <*> Error y = Error $ x ++ y
  Error x <*> Ok _ = Error x
  Ok _ <*> Error y = Error y
  Ok x <*> Ok y = Ok $ x y

instance Semigroup ok => Semigroup (Result e ok) where
  Error x <> Error y = Error $ x ++ y
  Ok _ <> Error y = Error y
  Error x <> _ = Error x
  Ok a <> Ok b = Ok $ a <> b

instance Semigroup ok => Monoid (Result e ok) where
  mempty = Error []

-- | convert a 'Result' to 'Maybe' discarding the error message on 'Error'
getResult :: Result e ok -> Maybe ok
getResult (Error _) = Nothing
getResult (Ok r) = Just r

-- | An ID used to identify forms
data FormId
  = FormId
      String -- ^ Global prefix for the form
      (NonEmpty Int) -- ^ Stack indicating field. Head is most specific to this item
  | FormIdCustom 
      String -- ^ Local name of the input
      Int -- ^ Index of the input
  deriving (Eq, Ord)

instance Torsor FormId Int where
  add i (FormId p (x :| xs)) = FormId p $ (x + i) :| xs
  add i (FormIdCustom n x) = FormIdCustom n $ x + i 
  difference a b = formIdentifier a - formIdentifier b

-- | The zero ID, i.e. the first ID that is usable
zeroId :: String -> FormId
zeroId prefix = FormId prefix (pure 0)

-- | map a function over the @NonEmpty Int@ inside a 'FormId'
mapId :: (NonEmpty Int -> NonEmpty Int) -> FormId -> FormId
mapId f (FormId p is) = FormId p $ f is
mapId f (FormIdCustom n i) = let (i' :| _) = f (pure i) in FormIdCustom n i'

instance Show FormId where
  show (FormId p xs) =
    p ++ "-fval-" ++ (intercalate "." $ reverseMap show $ NE.toList xs)
  show (FormIdCustom x _) = x

reverseMap :: Foldable t => (a -> b) -> t a -> [b]
reverseMap f = foldl' (\as a -> f a : as ) []

-- | get the head 'Int' from a 'FormId'
formIdentifier :: FormId -> Int
formIdentifier (FormId _ (x :| _)) = x
formIdentifier (FormIdCustom _ x) = x

-- | A range of ID's to specify a group of forms
data FormRange
  = FormRange FormId FormId
  deriving (Eq, Show)

instance Semigroup FormRange where
  (FormRange start _) <> (FormRange _ end) = FormRange start end

-- | Increment a form ID
incrementFormId :: FormId -> FormId
incrementFormId fid = add 1 fid

-- | create a 'FormRange' from a 'FormId'
unitRange :: FormId -> FormRange
unitRange i = FormRange i $ incrementFormId i

-- | Check if a 'FormId' is contained in a 'FormRange'
isInRange
  :: FormId -- ^ Id to check for
  -> FormRange -- ^ Range
  -> Bool -- ^ If the range contains the id
isInRange a (FormRange b c) = 
     formIdentifier a >= formIdentifier b 
  && formIdentifier a < formIdentifier c

-- | Check if a 'FormRange' is contained in another 'FormRange'
isSubRange
  :: FormRange -- ^ Sub-range
  -> FormRange -- ^ Larger range
  -> Bool -- ^ If the sub-range is contained in the larger range
isSubRange (FormRange a b) (FormRange c d) =
     formIdentifier a >= formIdentifier c 
  && formIdentifier b <= formIdentifier d

-- | Select the errors for a certain range
retainErrors :: FormRange -> [(FormRange, e)] -> [e]
retainErrors range = map snd . filter ((== range) . fst)

-- | Select the errors originating from this form or from any of the children of
-- this form
retainChildErrors :: FormRange -> [(FormRange, e)] -> [e]
retainChildErrors range = map snd . filter ((`isSubRange` range) . fst)
