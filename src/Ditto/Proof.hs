{-# LANGUAGE 
    DeriveFunctor
  , NamedFieldPuns
  , ScopedTypeVariables
#-}

{- |
This module defines the 'Proof' type, some proofs, and some helper functions.

A 'Proof' does three things:

 - verifies that the input value meets some criteria
 - optionally transforms the input value to another value while preserving that criteria
 - puts the proof name in type-signature where the type-checker can use it
-}

module Ditto.Proof where

import Control.Monad.Trans (lift)
import Ditto.Core (Form(..))
import Ditto.Backend (FormError(..))
import Ditto.Types (Proved(..), Result(..))
import Numeric (readDec, readFloat, readSigned)

-- | A 'Proof' attempts to prove something about a value.
--
-- If successful, it can also transform the value to a new value. The
-- proof should hold for the new value as well.
--
-- Generally, each 'Proof' has a unique data-type associated with it
-- which names the proof, such as:
--

data Proof m err a b = Proof
  { proofFunction :: a -> m (Either err b) -- ^ function which provides the proof
  , proofNewInitialValue :: a -> b -- ^ usually @const b@
  } deriving (Functor)

-- | apply a 'Proof' to a 'Form'
prove
  :: (Monad m, Monoid view, FormError input error)
  => Form m input error view a
  -> Proof m error a b
  -> Form m input error view b
prove Form{formDecodeInput, formInitialValue, formFormlet} (Proof f ivB) = Form 
  (\input -> do 
    a <- formDecodeInput input
    case a of
      Left x -> pure $ Left x
      Right x -> f x
  )
  (fmap ivB formInitialValue)
  ( do
    (html, a) <- formFormlet
    res <- lift $ case a of
      Error xs -> pure $ Error xs
      Ok (Proved pos x) -> do
        eeb <- f x
        case eeb of
          Left err -> pure $ Error [(pos, err)]
          Right res -> pure $ Ok (Proved pos res)
    pure
      ( html
      , res
      )
  )

-- * transformations (proofs minus the proof).

-- | transform the 'Form' result using a monadic 'Either' function.
transformEitherM
  :: (Monad m, Monoid view, FormError input error)
  => Form m input error view a
  -> (a -> m (Either error b))
  -> (a -> b)
  -> Form m input error view b
transformEitherM frm func ivb = frm `prove` Proof func ivb

-- | transform the 'Form' result using an 'Either' function.
transformEither
  :: (Monad m, Monoid view, FormError input error)
  => Form m input error view a
  -> (a -> Either error b)
  -> (a -> b)
  -> Form m input error view b
transformEither frm func ivb = transformEitherM frm (pure . func) ivb

-- * Various Proofs

-- | prove that a list is not empty
notNullProof :: (Monad m) => error -> Proof m error [a] [a]
notNullProof errorMsg = Proof (pure . check) id
  where
    check list =
      if null list
      then Left errorMsg
      else Right list

-- | read an unsigned number in decimal notation
decimal
  :: (Monad m, Eq i, Num i)
  => (String -> error) -- ^ create an error message ('String' is the value that did not parse)
  -> i
  -> Proof m error String i
decimal mkError i = Proof (pure . toDecimal) (const i)
  where
    toDecimal str =
      case readDec str of
        [(d, [])] -> Right d
        _ -> Left $ mkError str

-- | read signed decimal number
signedDecimal :: (Monad m, Eq i, Real i) 
  => (String -> error) 
  -> i
  -> Proof m error String i
signedDecimal mkError i = Proof (pure . toDecimal) (const i)
  where
    toDecimal str =
      case readSigned readDec str of
        [(d, [])] -> Right d
        _ -> Left $ mkError str

-- | read 'RealFrac' number
realFrac :: (Monad m, RealFrac a) 
  => (String -> error) 
  -> a
  -> Proof m error String a
realFrac mkError a = Proof (pure . toRealFrac) (const a)
  where
    toRealFrac str =
      case readFloat str of
        [(f, [])] -> Right f
        _ -> Left $ mkError str

-- | read a signed 'RealFrac' number
realFracSigned :: (Monad m, RealFrac a) 
  => (String -> error) 
  -> a
  -> Proof m error String a
realFracSigned mkError a = Proof (pure . toRealFrac) (const a)
  where
    toRealFrac str =
      case readSigned readFloat str of
        [(f, [])] -> Right f
        _ -> Left $ mkError str

