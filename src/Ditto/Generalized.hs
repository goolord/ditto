{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.

module Ditto.Generalized where

import Ditto.Backend
import Ditto.Core
import Ditto.Result
import qualified Ditto.Generalized.Internal as G

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input :: (Monad m, FormError err) => (input -> Either err a) -> (FormId -> a -> view) -> a -> Form m input err view a
input = G.input getFormId

-- | used for elements like @\<input type=\"submit\"\>@ which are not always present in the form submission data.
inputMaybe
  :: (Monad m, FormError err)
  => (input -> Either err a)
  -> (FormId -> a -> view)
  -> a
  -> Form m input err view (Maybe a)
inputMaybe = G.inputMaybe getFormId

-- | used for elements like @\<input type=\"reset\"\>@ which take a value, but are never present in the form data set.
inputNoData
  :: (Monad m)
  => (FormId -> a -> view)
  -> a
  -> Form m input err view ()
inputNoData = G.inputNoData getFormId

-- | used for @\<input type=\"file\"\>@
inputFile
  :: forall m input err view. (Monad m, FormInput input, FormError err, ErrorInputType err ~ input)
  => (FormId -> view)
  -> Form m input err view (FileType input)
inputFile = G.inputFile getFormId

-- | used for groups of checkboxes, @\<select multiple=\"multiple\"\>@ boxes
inputMulti
  :: forall m input err view a lbl. (Functor m, FormError err, ErrorInputType err ~ input, FormInput input, Monad m)
  => [(a, lbl)] -- ^ value, label, initially checked
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> (a -> Bool) -- ^ isChecked/isSelected initially
  -> Form m input err view [a]
inputMulti = G.inputMulti getFormId

-- | radio buttons, single @\<select\>@ boxes
inputChoice
  :: forall a m err input lbl view. (Functor m, FormError err, ErrorInputType err ~ input, FormInput input, Monad m)
  => (a -> Bool) -- ^ is default
  -> [(a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> Form m input err view a
inputChoice = G.inputChoice getFormId

-- | radio buttons, single @\<select\>@ boxes
inputChoiceForms
  :: forall a m err input lbl view. (Functor m, Monad m, FormError err, ErrorInputType err ~ input, FormInput input)
  => a
  -> [(Form m input err view a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, FormId, view, lbl, Bool)] -> view) -- ^ function which generates the view
  -> Form m input err view a
inputChoiceForms = G.inputChoiceForms getFormId

-- | used to create @\<label\>@ elements
label
  :: Monad m
  => (FormId -> view)
  -> Form m input err view ()
label = G.label getFormId

-- | used to add a list of err messages to a 'Form'
--
-- This function automatically takes care of extracting only the
-- errors that are relevent to the form element it is attached to via
-- '<++' or '++>'.
errors
  :: Monad m
  => ([err] -> view) -- ^ function to convert the err messages into a view
  -> Form m input err view ()
errors = G.errors

-- | similar to 'errors' but includes err messages from children of the form as well.
childErrors
  :: Monad m
  => ([err] -> view)
  -> Form m input err view ()
childErrors = G.childErrors

-- | modify the view of a form based on its errors
withErrors
  :: Monad m
  => (view -> [err] -> view)
  -> Form m input err view a
  -> Form m input err view a
withErrors = G.withErrors
