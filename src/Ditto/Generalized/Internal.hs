{-# LANGUAGE 
    TypeFamilies 
  , DoAndIfThenElse
  , ScopedTypeVariables
  , OverloadedStrings
  , LambdaCase
#-}

-- This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.

module Ditto.Generalized.Internal where

-- import Control.Monad (foldM)
-- import Control.Monad.Trans (lift)
-- import Data.Bifunctor
import Ditto.Backend
import Ditto.Core
import Ditto.Types

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input
  :: forall m input err a view. (Monad m, FormError input err)
  => FormState m input FormId
  -> (input -> Either err a)
  -> (FormId -> a -> view)
  -> a
  -> Form m input err view a
input formSId fromInput toView initialValue =
  Form (pure . fromInput) initialValue $ do
    i <- formSId
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i initialValue
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = initialValue
                }
              )
          )
      Found x -> case fromInput x of
        Right a -> pure
          ( View $ const $ toView i a
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = a
                }
              )
          )
        Left err -> pure
          ( View $ const $ toView i initialValue
          , pure $ Error [(unitRange i, err)]
          )
      Missing -> pure
        ( View $ const $ toView i initialValue
        , pure $ Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
        )

