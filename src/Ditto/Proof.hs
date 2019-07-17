{- |
This module defines the 'Proof' type, some proofs, and some helper functions.

A 'Proof' does three things:

 - verifies that the input value meets some criteria
 - optionally transforms the input value to another value while preserving that criteria
 - puts the proof name in type-signature where the type-checker can use it
-}
module Ditto.Proof where

import Control.Monad.Trans (lift)
import Numeric (readDec, readFloat, readSigned)
import Ditto.Core (Form (..), Proved (..))
import Ditto.Result (Result (..))

-- | A 'Proof' attempts to prove something about a value.
--
-- If successful, it can also transform the value to a new value. The
-- proof should hold for the new value as well.
--
-- Generally, each 'Proof' has a unique data-type associated with it
-- which names the proof, such as:
--
--
data Proof m error a b = Proof
  { proofFunction :: a -> m (Either error b) -- ^ function which provides the proof
  }

-- | apply a 'Proof' to a 'Form'
prove
  :: (Monad m)
  => Form m input error view a
  -> Proof m error a b
  -> Form m input error view b
prove (Form frm) (Proof f) =
  Form $ do
    (xml, mval) <- frm
    val <- lift $ lift $ mval
    case val of
      (Error errs) -> pure (xml, pure $ Error errs)
      (Ok (Proved posi a)) ->
        do
          r <- lift $ lift $ f a
          case r of
            (Left err) -> pure (xml, pure $ Error [(posi, err)])
            (Right b) ->
              pure
                ( xml
                , pure $
                  Ok
                    ( Proved
                      { pos = posi
                      , unProved = b
                      }
                    )
                )

-- * transformations (proofs minus the proof).

-- | transform a 'Form' using a 'Proof', and the replace the proof with @()@.
--
-- This is useful when you want just want classic digestive-functors behaviour.
transform
  :: (Monad m)
  => Form m input error view a
  -> Proof m error a b
  -> Form m input error view b
transform frm proof = frm `prove` proof

-- | transform the 'Form' result using a monadic 'Either' function.
transformEitherM
  :: (Monad m)
  => Form m input error view a
  -> (a -> m (Either error b))
  -> Form m input error view b
transformEitherM frm func = frm `transform` (Proof func)

-- | transform the 'Form' result using an 'Either' function.
transformEither
  :: (Monad m)
  => Form m input error view a
  -> (a -> Either error b)
  -> Form m input error view b
transformEither frm func = transformEitherM frm (pure . func)

-- * Various Proofs

-- | prove that a list is not empty
notNullProof :: (Monad m) => error -> Proof m error [a] [a]
notNullProof errorMsg = Proof (pure . check)
  where
    check list =
      if null list
      then (Left errorMsg)
      else (Right list)

-- | read an unsigned number in decimal notation
decimal
  :: (Monad m, Eq i, Num i)
  => (String -> error) -- ^ create an error message ('String' is the value that did not parse)
  -> Proof m error String i
decimal mkError = Proof (pure . toDecimal)
  where
    toDecimal str =
      case readDec str of
        [(d, [])] -> (Right d)
        _ -> (Left $ mkError str)

-- | read signed decimal number
signedDecimal :: (Monad m, Eq i, Real i) => (String -> error) -> Proof m error String i
signedDecimal mkError = Proof (pure . toDecimal)
  where
    toDecimal str =
      case (readSigned readDec) str of
        [(d, [])] -> (Right d)
        _ -> (Left $ mkError str)

-- | read 'RealFrac' number
realFrac :: (Monad m, RealFrac a) => (String -> error) -> Proof m error String a
realFrac mkError = Proof (pure . toRealFrac)
  where
    toRealFrac str =
      case readFloat str of
        [(f, [])] -> (Right f)
        _ -> (Left $ mkError str)

-- | read a signed 'RealFrac' number
realFracSigned :: (Monad m, RealFrac a) => (String -> error) -> Proof m error String a
realFracSigned mkError = Proof (pure . toRealFrac)
  where
    toRealFrac str =
      case (readSigned readFloat) str of
        [(f, [])] -> (Right f)
        _ -> (Left $ mkError str)
