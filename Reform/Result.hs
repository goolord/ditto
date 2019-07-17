-- | Module for the core result type, and related functions
--
module Text.Reform.Result
  ( Result (..)
  , getResult
  , FormId
  , zeroId
  , mapId
  , formIdList
  , FormRange (..)
  , incrementFormId
  , unitRange
  , isInRange
  , isSubRange
  , retainErrors
  , retainChildErrors
  )
where

import Data.List (intercalate)
import Control.Applicative (Applicative (..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
-- import Data.List (intercalate)

-- | Type for failing computations
--
data Result e ok
  = Error [(FormRange, e)]
  | Ok ok
  deriving (Show, Eq)

instance Functor (Result e) where

  fmap _ (Error x) = Error x
  fmap f (Ok x) = Ok (f x)

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

-- | convert a 'Result' to 'Maybe' discarding the error message on 'Error'
getResult :: Result e ok -> Maybe ok
getResult (Error _) = Nothing
getResult (Ok r) = Just r

-- | An ID used to identify forms
--
data FormId
  = FormId
      { -- | Global prefix for the form
        formPrefix :: String
      , -- | Stack indicating field. Head is most specific to this item
        formIdList :: NonEmpty Integer
      }
  | FormIdCustom String
  deriving (Eq, Ord)

-- | The zero ID, i.e. the first ID that is usable
--
zeroId :: String -> FormId
zeroId p = FormId
  { formPrefix = p
  , formIdList = pure 0
  }

-- | map a function over the @NonEmpty Integer@ inside a 'FormId'
mapId :: (NonEmpty Integer -> NonEmpty Integer) -> FormId -> FormId
mapId f (FormId p is) = FormId p $ f is
mapId _ x = x

instance Show FormId where
  show (FormId p xs) =
    p ++ "-fval[" ++ (intercalate "." $ reverse $ map show $ NE.toList xs) ++ "]"
  show (FormIdCustom x) = x

-- | get the head 'Integer' from a 'FormId'
formId :: FormId -> Integer
formId = NE.head . formIdList

-- | A range of ID's to specify a group of forms
--
data FormRange
  = FormRange FormId FormId
  deriving (Eq, Show)

-- | Increment a form ID
--
incrementFormId :: FormId -> FormId
incrementFormId (FormId p (x :| xs)) = FormId p $ (x + 1) :| xs
incrementFormId x@FormIdCustom{} = x

-- | create a 'FormRange' from a 'FormId'
unitRange :: FormId -> FormRange
unitRange i = FormRange i $ incrementFormId i

-- | Check if a 'FormId' is contained in a 'FormRange'
--
isInRange
  :: FormId -- ^ Id to check for
  -> FormRange -- ^ Range
  -> Bool -- ^ If the range contains the id
isInRange a (FormRange b c) = formId a >= formId b && formId a < formId c

-- | Check if a 'FormRange' is contained in another 'FormRange'
--
isSubRange
  :: FormRange -- ^ Sub-range
  -> FormRange -- ^ Larger range
  -> Bool -- ^ If the sub-range is contained in the larger range
isSubRange (FormRange a b) (FormRange c d) =
  formId a >= formId c &&
    formId b <=
    formId d

-- | Select the errors for a certain range
--
retainErrors :: FormRange -> [(FormRange, e)] -> [e]
retainErrors range = map snd . filter ((== range) . fst)

-- | Select the errors originating from this form or from any of the children of
-- this form
--
retainChildErrors :: FormRange -> [(FormRange, e)] -> [e]
retainChildErrors range = map snd . filter ((`isSubRange` range) . fst)
