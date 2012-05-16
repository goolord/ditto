{- |
This module defines the 'Proof' type, some proofs, and some helper functions.

A 'Proof' does three things:

 - verifies that the input value meets some criteria
 - optionally transforms the input value to another value while preserving that criteria
 - puts the proof name in type-signature where the type-checker can use it
-}
module Text.Reform.Proof where

import Control.Applicative.Indexed (IndexedFunctor(imap))
import Control.Monad.Trans   (lift)
import Numeric               (readDec, readFloat, readSigned)
import Text.Reform.Result (FormRange, Result(..))
import Text.Reform.Core   (Form(..), Proved(..))

-- | A 'Proof' attempts to prove something about a value.
--
-- If successful, it can also transform the value to a new value. The
-- proof should hold for the new value as well.
--
-- Generally, each 'Proof' has a unique data-type associated with it
-- which names the proof, such as:
--
-- > data NotNull = NotNull
--
data Proof m error proof a b
    = Proof { proofName     :: proof                   -- ^ name of the thing to prove
            , proofFunction :: a -> m (Either error b) -- ^ function which provides the proof
            }

-- | apply a 'Proof' to a 'Form'
prove :: (Monad m) =>
         Form m input error view q a
      -> Proof m error proof a b
      -> Form m input error view proof b
prove (Form frm) (Proof p f) =
    Form $ do (xml, mval) <- frm
              val <- lift $ lift $ mval
              case val of
                (Error errs)            -> return (xml, return $ Error errs)
                (Ok (Proved _ pos a)) ->
                    do r <- lift $ lift $ f a
                       case r of
                         (Left err) -> return (xml, return $ Error [(pos, err)])
                         (Right b)  ->
                             return (xml, return $ Ok (Proved { proofs   = p
                                                              , pos      = pos
                                                              , unProved = b
                                                              }))

-- * transformations (proofs minus the proof).

-- | transform a 'Form' using a 'Proof', and the replace the proof with @()@.
--
-- This is useful when you want just want classic digestive-functors behaviour.
transform :: (Monad m) =>
             Form m input error view anyProof a
          -> Proof m error proof a b
          -> Form m input error view () b
transform frm proof = imap (const ()) id (frm `prove` proof)

-- | transform the 'Form' result using a monadic 'Either' function.
transformEitherM :: (Monad m) => Form m input error view anyProof a
                 -> (a -> m (Either error b))
                 -> Form m input error view () b
transformEitherM frm func = frm `transform` (Proof () func)

-- | transform the 'Form' result using an 'Either' function.
transformEither :: (Monad m) =>
                   Form m input error view anyProof a
                -> (a -> Either error b)
                -> Form m input error view () b
transformEither frm func = transformEitherM frm (return . func)

-- * Various Proofs

-- | proof that a list is not empty
data NotNull = NotNull

-- | prove that a list is not empty
notNullProof :: (Monad m) => error -> Proof m error NotNull [a] [a]
notNullProof errorMsg = Proof NotNull (return . check)
    where
    check list =
        if null list
          then (Left errorMsg)
          else (Right list)

-- | proof that a 'String' is a decimal number
data Decimal        = Decimal
-- | proof that a 'String' is a Real/Fractional number
data RealFractional = RealFractional
-- | proof that a number is also (allowed to be) signed
data Signed a       = Signed a

-- | read an unsigned number in decimal notation
decimal :: (Monad m, Eq i, Num i) =>
           (String -> error) -- ^ create an error message ('String' is the value that did not parse)
        -> Proof m error Decimal String i
decimal mkError = Proof Decimal (return . toDecimal)
    where
      toDecimal str =
          case readDec str of
            [(d,[])] -> (Right d)
            _        -> (Left $ mkError str)

-- | read signed decimal number
signedDecimal :: (Monad m, Eq i, Real i) => (String -> error) -> Proof m error (Signed Decimal) String i
signedDecimal mkError = Proof (Signed Decimal) (return . toDecimal)
    where
      toDecimal str =
          case (readSigned readDec) str of
            [(d,[])] -> (Right d)
            _        -> (Left $ mkError str)

-- | read 'RealFrac' number
realFrac :: (Monad m, RealFrac a) => (String -> error) -> Proof m error RealFractional String a
realFrac mkError = Proof RealFractional (return . toRealFrac)
    where
      toRealFrac str =
          case readFloat str of
            [(f,[])] -> (Right f)
            _        -> (Left $ mkError str)

-- | read a signed 'RealFrac' number
realFracSigned :: (Monad m, RealFrac a) => (String -> error) -> Proof m error (Signed RealFractional) String a
realFracSigned mkError = Proof (Signed RealFractional) (return . toRealFrac)
    where
      toRealFrac str =
          case (readSigned readFloat) str of
            [(f,[])] -> (Right f)
            _        -> (Left $ mkError str)
