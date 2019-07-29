{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.

module Ditto.Generalized.Named where

import Ditto.Backend
import Ditto.Core
import Ditto.Result
import qualified Ditto.Generalized.Internal as G

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input :: (Monad m, FormError err input) => String -> (input -> Either err a) -> (FormId -> a -> view) -> a -> Form m input err view a
input name = G.input (getNamedFormId name)


-- | used to construct elements with optional initial values, which are still required
inputMaybeReq
  :: (Monad m, FormError err input)
  => String
  -> (input -> Either err a)
  -> (FormId -> Maybe a -> view)
  -> Maybe a
  -> Form m input err view a
inputMaybeReq name = G.inputMaybeReq (getNamedFormId name)

-- | used for elements like @\<input type=\"submit\"\>@ which are not always present in the form submission data.
inputMaybe
  :: (Monad m, FormError err input)
  => String
  -> (input -> Either err a)
  -> (FormId -> Maybe a -> view)
  -> Maybe a
  -> Form m input err view (Maybe a)
inputMaybe name = G.inputMaybe (getNamedFormId name)

-- | used for elements like @\<input type=\"reset\"\>@ which take a value, but are never present in the form data set.
inputNoData
  :: (Monad m)
  => String
  -> (FormId -> view)
  -> Form m input err view ()
inputNoData name = G.inputNoData (getNamedFormId name)

-- | used for @\<input type=\"file\"\>@
inputFile
  :: forall m input err view. (Monad m, FormInput input, FormError err input)
  => String
  -> (FormId -> view)
  -> Form m input err view (FileType input)
inputFile name = G.inputFile (getNamedFormId name)

-- | used for groups of checkboxes, @\<select multiple=\"multiple\"\>@ boxes
inputMulti
  :: forall m input err view a lbl. (FormError err input, FormInput input, Monad m)
  => String
  -> [(a, lbl)] -- ^ value, label, initially checked
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> (a -> Bool) -- ^ isChecked/isSelected initially
  -> Form m input err view [a]
inputMulti name = G.inputMulti (getNamedFormId name)

-- | radio buttons, single @\<select\>@ boxes
inputChoice
  :: forall a m err input lbl view. (FormError err input, FormInput input, Monad m)
  => String
  -> (a -> Bool) -- ^ is default
  -> [(a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> Form m input err view a
inputChoice name = G.inputChoice (getNamedFormId name)

-- | radio buttons, single @\<select\>@ boxes
inputChoiceForms
  :: forall a m err input lbl view. (Monad m, FormError err input, FormInput input)
  => String
  -> a
  -> [(Form m input err view a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, FormId, view, lbl, Bool)] -> view) -- ^ function which generates the view
  -> Form m input err view a
inputChoiceForms name = G.inputChoiceForms (getNamedFormId name)

-- | used to create @\<label\>@ elements
label
  :: Monad m
  => String
  -> (FormId -> view)
  -> Form m input err view ()
label name = G.label (getNamedFormId name)

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

