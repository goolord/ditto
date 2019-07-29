{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
This module contains two classes. 'FormInput' is a class which is parameterized over the @input@ type used to represent form data in different web frameworks. There should be one instance for each framework, such as Happstack, Snap, WAI, etc.

The 'FormError' class is used to map error messages into an application specific error type.

-}
module Ditto.Backend where

import Data.Text (Text)
import Ditto.Result (FormId)
import qualified Data.Text as T

-- | an error type used to represent errors that are common to all backends
--
-- These errors should only occur if there is a bug in the ditto-*
-- packages. Perhaps we should make them an 'Exception' so that we can
-- get rid of the 'FormError' class.
data CommonFormError input
  = InputMissing FormId
  | NoStringFound input
  | NoFileFound input
  | MultiFilesFound input
  | MultiStringsFound input
  | MissingDefaultValue
  deriving (Eq, Ord, Show)

-- | some default error messages for 'CommonFormError'
commonFormErrorStr
  :: (input -> String) -- ^ show 'input' in a format suitable for error messages
  -> CommonFormError input -- ^ a 'CommonFormError'
  -> String
commonFormErrorStr showInput cfe = case cfe of
  InputMissing formId -> "Input field missing for " ++ show formId
  NoStringFound input -> "Could not extract a string value from: " ++ showInput input
  NoFileFound input -> "Could not find a file associated with: " ++ showInput input
  MultiFilesFound input -> "Found multiple files associated with: " ++ showInput input
  MultiStringsFound input -> "Found multiple strings associated with: " ++ showInput input
  MissingDefaultValue -> "Missing default value."

-- | some default error messages for 'CommonFormError'
commonFormErrorText
  :: (input -> Text) -- ^ show 'input' in a format suitable for error messages
  -> CommonFormError input -- ^ a 'CommonFormError'
  -> Text
commonFormErrorText showInput cfe = case cfe of
  InputMissing formId -> "Input field missing for " <> (T.pack $ show formId)
  NoStringFound input -> "Could not extract a string value from: " <> showInput input
  NoFileFound input -> "Could not find a file associated with: " <> showInput input
  MultiFilesFound input -> "Found multiple files associated with: " <> showInput input
  MultiStringsFound input -> "Found multiple strings associated with: " <> showInput input
  MissingDefaultValue -> "Missing default value."

-- | A Class to lift a 'CommonFormError' into an application-specific error type
class FormError e where
  type ErrorInputType e
  commonFormError :: (CommonFormError (ErrorInputType e)) -> e

instance FormError Text where
  type ErrorInputType Text = Text
  commonFormError = commonFormErrorText id

-- | Class which all backends should implement.
--
class FormInput input where

  -- |@input@ is here the type that is used to represent a value
  -- uploaded by the client in the request.
  type FileType input

  -- | Parse the input into a string. This is used for simple text fields
  -- among other things
  --
  getInputString :: (FormError error, ErrorInputType error ~ input) => input -> Either error String
  getInputString input =
    case getInputStrings input of
      [] -> Left (commonFormError $ NoStringFound input)
      [s] -> Right s
      _ -> Left (commonFormError $ MultiStringsFound input)

  -- | Should be implemented
  --
  getInputStrings :: input -> [String]

  -- | Parse the input value into 'Text'
  --
  getInputText :: (FormError error, ErrorInputType error ~ input) => input -> Either error Text
  getInputText input =
    case getInputTexts input of
      [] -> Left (commonFormError $ NoStringFound input)
      [s] -> Right s
      _ -> Left (commonFormError $ MultiStringsFound input)

  -- | Can be overriden for efficiency concerns
  --
  getInputTexts :: input -> [Text]
  getInputTexts = map T.pack . getInputStrings

  -- | Get a file descriptor for an uploaded file
  --
  getInputFile :: (FormError error, ErrorInputType error ~ input) => input -> Either error (FileType input)
