{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
This module defines the 'Form' type, its instances, core manipulation functions, and a bunch of helper utilities.
-}
module Ditto.Core where

import Control.Applicative (Applicative ((<*>), pure), empty, Alternative(..))
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Monoid (mappend, mempty))
import Data.Text.Lazy (Text, unpack)
import Ditto.Result (FormId (..), FormRange (..), Result (..), unitRange, zeroId)
import qualified Data.Semigroup as SG

------------------------------------------------------------------------------
-- * Proved
------------------------------------------------------------------------------

-- | Proved records a value, the location that value came from, and something that was proved about the value.
data Proved a
  = Proved
      { pos :: FormRange
      , unProved :: a
      }
  deriving (Show, Functor)

-- | Utility Function: trivially prove nothing about ()
unitProved :: FormId -> Proved ()
unitProved formId =
  Proved
    { pos = unitRange formId
    , unProved = ()
    }

------------------------------------------------------------------------------
-- * FormState
------------------------------------------------------------------------------

-- | inner state used by 'Form'.
type FormState m input = ReaderT (Environment m input) (StateT FormRange m)

-- | used to represent whether a value was found in the form
-- submission data, missing from the form submission data, or expected
-- that the default value should be used
data Value a
  = Default
  | Missing
  | Found a

-- | Utility function: Get the current input
--
getFormInput :: Monad m => FormState m input (Value input)
getFormInput = getFormId >>= getFormInput'

-- | Utility function: Gets the input of an arbitrary 'FormId'.
--
getFormInput' :: Monad m => FormId -> FormState m input (Value input)
getFormInput' id' = do
  env <- ask
  case env of
    NoEnvironment -> pure Default
    Environment f ->
      lift $ lift $ f id'

-- | Utility function: Get the current range
--
getFormRange :: Monad m => FormState m i FormRange
getFormRange = get

-- | The environment is where you get the actual input per form.
--
-- The 'NoEnvironment' constructor is typically used when generating a
-- view for a GET request, where no data has yet been submitted. This
-- will cause the input elements to use their supplied default values.
--
-- Note that 'NoEnviroment' is different than supplying an empty environment.
data Environment m input
  = Environment (FormId -> m (Value input))
  | NoEnvironment

instance (SG.Semigroup input, Monad m) => SG.Semigroup (Environment m input) where

  NoEnvironment <> x = x
  x <> NoEnvironment = x
  (Environment env1) <> (Environment env2) =
    Environment $ \id' ->
      do
        r1 <- (env1 id')
        r2 <- (env2 id')
        case (r1, r2) of
          (Missing, Missing) -> pure Missing
          (Default, Missing) -> pure Default
          (Missing, Default) -> pure Default
          (Default, Default) -> pure Default
          (Found x, Found y) -> pure $ Found (x SG.<> y)
          (Found x, _) -> pure $ Found x
          (_, Found y) -> pure $ Found y

-- | Not quite sure when this is useful and so hard to say if the rules for combining things with Missing/Default are correct
instance (SG.Semigroup input, Monad m) => Monoid (Environment m input) where

  mempty = NoEnvironment

  mappend = (SG.<>)

-- | Utility function: returns the current 'FormId'. This will only make sense
-- if the form is not composed
--
getFormId :: Monad m => FormState m i FormId
getFormId = do
  FormRange x _ <- get
  pure x

getNamedFormId :: Monad m => String -> FormState m i FormId
getNamedFormId name = do
  FormRange x _ <- get
  pure $ case x of
    FormIdCustom _ i -> FormIdCustom name i
    FormId _ (i :| _) -> FormIdCustom name i

-- | Utility function: increment the current 'FormId'.
incFormId :: Monad m => FormState m i ()
incFormId = do
  FormRange _ endF1 <- get
  put $ unitRange endF1

-- | A view represents a visual representation of a form. It is composed of a
-- function which takes a list of all errors and then produces a new view
--
newtype View error v
  = View
      { unView :: [(FormRange, error)] -> v
      }
  deriving (SG.Semigroup, Monoid, Functor)

------------------------------------------------------------------------------
-- * Form
------------------------------------------------------------------------------

-- | a 'Form' contains a 'View' combined with a validation function
-- which will attempt to extract a value from submitted form data.
--
-- It is highly parameterized, allowing it work in a wide variety of
-- different configurations. You will likely want to make a type alias
-- that is specific to your application to make type signatures more
-- manageable.
--
--   [@m@] A monad which can be used by the validator
--
--   [@input@] A framework specific type for representing the raw key/value pairs from the form data
--
--   [@error@] A application specific type for error messages
--
--   [@view@] The type of data being generated for the view (HSP, Blaze Html, Heist, etc)
--
--   [@proof@] A type which names what has been proved about the pure value. @()@ means nothing has been proved.
--
--   [@a@] Value pure by form when it is successfully decoded, validated, etc.
--
--
-- This type is very similar to the 'Form' type from
-- @digestive-functors <= 0.2@. If @proof@ is @()@, then 'Form' is an
-- applicative functor and can be used almost exactly like
-- @digestive-functors <= 0.2@.
newtype Form m input error view a = Form {unForm :: FormState m input (View error view, m (Result error (Proved a)))}
  deriving Functor

bracketState :: Monad m => FormState m input a -> FormState m input a
bracketState k = do
  FormRange startF1 _ <- get
  res <- k
  FormRange _ endF2 <- get
  put $ FormRange startF1 endF2
  pure res

instance (Functor m, Monoid view, Monad m) => Applicative (Form m input error view) where
  pure a =
    Form $ do
      i <- getFormId
      pure
        ( View $ const $ mempty
        , pure $ Ok $ Proved
          { pos = FormRange i i
          , unProved = a
          }
        )

  -- this coud be defined in terms of <<*>> if we just changed the proof of frmF to (() -> ())
  (Form frmF) <*> (Form frmA) =
    Form $ do
      ((view1, mfok), (view2, maok)) <-
        bracketState $ do
          res1 <- frmF
          incFormId
          res2 <- frmA
          pure (res1, res2)
      fok <- lift $ lift $ mfok
      aok <- lift $ lift $ maok
      case (fok, aok) of
        (Error errs1, Error errs2) -> pure (view1 <> view2, pure $ Error $ errs1 ++ errs2)
        (Error errs1, _) -> pure (view1 <> view2, pure $ Error $ errs1)
        (_, Error errs2) -> pure (view1 <> view2, pure $ Error $ errs2)
        (Ok (Proved (FormRange x _) f), Ok (Proved (FormRange _ y) a)) ->
          pure
            ( view1 <> view2
            , pure $ Ok $ Proved
              { pos = FormRange x y
              , unProved = f a
              }
            )

instance (Monad m, Monoid view) => Alternative (Form m input error view) where
  empty = Form $ pure (mempty, pure $ Error mempty)
  formA <|> formB = Form $ do
    (_, mres0) <- unForm formA
    res0 <- lift $ lift mres0
    case res0 of
      Ok _ -> unForm formA
      Error _ -> unForm formB

-- ** Ways to evaluate a Form

-- | Run a form
--
runForm
  :: (Monad m)
  => Environment m input
  -> Text
  -> Form m input error view a
  -> m (View error view, m (Result error (Proved a)))
runForm env prefix' form =
  evalStateT (runReaderT (unForm form) env) (unitRange (zeroId $ unpack prefix'))

-- | Run a form
--
runForm'
  :: (Monad m)
  => Environment m input
  -> Text
  -> Form m input error view a
  -> m (view, Maybe a)
runForm' env prefix form =
  do
    (view', mresult) <- runForm env prefix form
    result <- mresult
    pure $ case result of
      Error e -> (unView view' e, Nothing)
      Ok x -> (unView view' [], Just (unProved x))

-- | Just evaluate the form to a view. This usually maps to a GET request in the
-- browser.
--
viewForm
  :: (Monad m)
  => Text -- ^ form prefix
  -> Form m input error view a -- ^ form to view
  -> m view
viewForm prefix form =
  do
    (v, _) <- runForm NoEnvironment prefix form
    pure (unView v [])

-- | Evaluate a form
--
-- Returns:
--
-- [@Left view@] on failure. The @view@ will have already been applied to the errors.
--
-- [@Right a@] on success.
--
eitherForm
  :: (Monad m)
  => Environment m input -- ^ Input environment
  -> Text -- ^ Identifier for the form
  -> Form m input error view a -- ^ Form to run
  -> m (Either view a) -- ^ Result
eitherForm env id' form = do
  (view', mresult) <- runForm env id' form
  result <- mresult
  pure $ case result of
    Error e -> Left $ unView view' e
    Ok x -> Right (unProved x)

-- | create a 'Form' from some @view@.
--
-- This is typically used to turn markup like @\<br\>@ into a 'Form'.
view
  :: (Monad m)
  => view -- ^ View to insert
  -> Form m input error view () -- ^ Resulting form
view view' =
  Form $ do
    i <- getFormId
    pure
      ( View (const view')
      , pure
        ( Ok
          ( Proved
            { pos = FormRange i i
            , unProved = ()
            }
          )
        )
      )

-- | Append a unit form to the left. This is useful for adding labels or error
-- fields.
--
-- The 'Forms' on the left and right hand side will share the same
-- 'FormId'. This is useful for elements like @\<label
-- for=\"someid\"\>@, which need to refer to the id of another
-- element.
(++>)
  :: (Monad m, Semigroup view)
  => Form m input error view ()
  -> Form m input error view a
  -> Form m input error view a
f1 ++> f2 =
  Form $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v2, r) <- unForm f2
    (v1, _) <- unForm f1
    pure (v1 <> v2, r)

infixl 6 ++>

-- | Append a unit form to the right. See '++>'.
--
(<++)
  :: (Monad m, Semigroup view)
  => Form m input error view a
  -> Form m input error view ()
  -> Form m input error view a
f1 <++ f2 =
  Form $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v1, r) <- unForm f1
    (v2, _) <- unForm f2
    pure (v1 <> v2, r)

infixr 5 <++

-- | Change the view of a form using a simple function
--
-- This is useful for wrapping a form inside of a \<fieldset\> or other markup element.
mapView
  :: (Monad m)
  => (view -> view') -- ^ Manipulator
  -> Form m input error view a -- ^ Initial form
  -> Form m input error view' a -- ^ Resulting form
mapView f = Form . fmap (first $ fmap f) . unForm

-- | Utility Function: turn a view and pure value into a successful 'FormState'
mkOk
  :: (Monad m)
  => FormId
  -> view
  -> a
  -> FormState m input (View error view, m (Result error (Proved a)))
mkOk i view' val =
  pure
    ( View $ const $ view'
    , pure $
      Ok
        ( Proved
          { pos = unitRange i
          , unProved = val
          }
        )
    )
