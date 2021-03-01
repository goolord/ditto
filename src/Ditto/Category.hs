{-# LANGUAGE 
    DeriveFunctor
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , RecordWildCards
#-}

module Ditto.Category where

import Prelude hiding ((.), id)

import Ditto.Core
import Control.Category
import Control.Arrow
import Ditto.Types
import Control.Applicative

newtype FormArrow m input err view a b = FormArrow
  { runFormArrow :: m a -> Form m input err view b }

fooForm :: Monad m => FormArrow m String () () Int Bool
fooForm = arr even

instance (Monad m, Monoid view) => Category (FormArrow m input err view) where
  id = FormArrow liftForm
  (FormArrow fa) . (FormArrow fb) = FormArrow $ \a -> do
    let formB = fb a
    Form
      { formDecodeInput = \input -> do
          eb <- formDecodeInput formB input
          case eb of
            Right b -> do
              let formA = fa (pure b)
              formDecodeInput formA input
            Left err -> pure $ Left err
      , formInitialValue = formInitialValue $ fa $ formInitialValue formB
      , formFormlet = do
        (view', res) <- formFormlet formB
        case res of
          Error err -> pure (view', Error err)
          Ok (Proved _ b) -> do
            let formA = fa (pure b)
            formFormlet formA
      }

instance (Monad m, Monoid view) => Arrow (FormArrow m input err view) where
  arr f = FormArrow $ liftForm . fmap f
  (FormArrow fa) *** (FormArrow fb) = FormArrow $ \x -> do
    let formB = fb (fmap snd x)
    let formA = fa (fmap fst x)
    Form
      { formDecodeInput = \input -> do
          eb <- formDecodeInput formB input
          ea <- formDecodeInput formA input
          pure $ liftA2 (,) ea eb
      , formInitialValue = liftA2 (,) (formInitialValue formA) (formInitialValue formB)
      , formFormlet = do 
          let liftProved f (Proved (FormRange l _) a) (Proved (FormRange r _) b) = Proved (FormRange l r) (f a b)
          (viewA,resA) <- formFormlet formA
          (viewB,resB) <- formFormlet formB
          pure (viewA <> viewB, liftA2 (liftProved (,)) resA resB)
      }
