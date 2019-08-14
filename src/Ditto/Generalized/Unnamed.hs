{-# LANGUAGE 
    ScopedTypeVariables
#-}

-- | This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.
--
-- Additionally, the inputs generated with the functions from this module will have their names/ids automatically enumerated.
--
-- For named formlets, see @Ditto.Generalized.Named@
module Ditto.Generalized.Unnamed
  ( G.Choice(..)
  , input
  , inputMaybe
  , inputNoData
  , inputFile
  , inputMulti
  , inputChoice
  , inputList
  , label
  , errors
  , childErrors
  , withErrors
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Ditto.Backend
import Ditto.Core
import Ditto.Types
import qualified Ditto.Generalized.Internal as G

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input :: (Environment m input, FormError input err) 
  => (input -> Either err a) 
  -> (FormId -> a -> view) 
  -> a 
  -> Form m input err view a
input = G.input getFormId

-- | used for elements like @\<input type=\"submit\"\>@ which are not always present in the form submission data.
inputMaybe :: (Environment m input, FormError input err)
  => (input -> Either err a)
  -> (FormId -> Maybe a -> view)
  -> Maybe a
  -> Form m input err view (Maybe a)
inputMaybe = G.inputMaybe getFormId

-- | used for elements like @\<input type=\"reset\"\>@ which take a value, but are never present in the form data set.
inputNoData :: (Environment m input)
  => (FormId -> view)
  -> Form m input err view ()
inputNoData = G.inputNoData getFormId

-- | used for @\<input type=\"file\"\>@
inputFile :: forall m input err view. (Environment m input, FormInput input, FormError input err)
  => (FormId -> view)
  -> Form m input err view (FileType input)
inputFile = G.inputFile getFormId

-- | used for groups of checkboxes, @\<select multiple=\"multiple\"\>@ boxes
inputMulti :: forall m input err view a lbl. (FormError input err, FormInput input, Environment m input, Eq a)
  => [(a, lbl)] -- ^ value, label, initially checked
  -> (input -> Either err [a])
  -> (FormId -> [G.Choice lbl a] -> view) -- ^ function which generates the view
  -> (a -> Bool) -- ^ isChecked/isSelected initially
  -> Form m input err view [a]
inputMulti = G.inputMulti getFormId

-- | radio buttons, single @\<select\>@ boxes
inputChoice :: forall a m err input lbl view. (FormError input err, FormInput input, Environment m input, Eq a, Monoid view)
  => (a -> Bool) -- ^ is default
  -> NonEmpty (a, lbl) -- ^ value, label
  -> (input -> Either err a)
  -> (FormId -> [G.Choice lbl a] -> view) -- ^ function which generates the view
  -> Form m input err view a
inputChoice = G.inputChoice getFormId

-- | this is necessary in order to basically map over the decoding function
inputList :: forall m input err a view. (Monad m, FormError input err, Environment m input)
  => (input -> Either err [a]) -- ^ decoding function for the list
  -> ([view] -> view) -- ^ how to concatenate views
  -> [a] -- ^ initial values
  -> view -- ^ view to generate in the fail case
  -> (a -> Form m input err view a)
  -> Form m input err view [a]
inputList = G.inputList getFormId

-- | used to create @\<label\>@ elements
label :: Environment m input
  => (FormId -> view)
  -> Form m input err view ()
label = G.label getFormId

-- | used to add a list of err messages to a 'Form'
--
-- This function automatically takes care of extracting only the
-- errors that are relevent to the form element it is attached to via
-- '<*' or '*>'.
errors :: Environment m input
  => ([err] -> view) -- ^ function to convert the err messages into a view
  -> Form m input err view ()
errors = G.errors

-- | similar to 'errors' but includes err messages from children of the form as well.
childErrors :: Environment m input
  => ([err] -> view)
  -> Form m input err view ()
childErrors = G.childErrors

-- | modify the view of a form based on its errors
withErrors :: Environment m input
  => (view -> [err] -> view)
  -> Form m input err view a
  -> Form m input err view a
withErrors = G.withErrors

