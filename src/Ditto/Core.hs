{-# LANGUAGE 
    DeriveFunctor
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , NamedFieldPuns
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeFamilies
  , LiberalTypeSynonyms
  , TypeSynonymInstances
  , UndecidableInstances
  , DataKinds
  , KindSignatures
#-}

-- | The core module for @ditto@. 
--
-- This module provides the @Form@ type and helper functions 
-- for constructing typesafe forms inside arbitrary "views" / web frameworks.
-- @ditto@ is meant to be a generalized formlet library used to write
-- formlet libraries specific to a web / gui framework
module Ditto.Core (
  -- * Form types
  -- | The representation of formlets
    FormState
  , Form(..)
  -- * Environment
  -- | The interface to a given web framework
  , Environment(..)
  , NoEnvironment(..)
  , WithEnvironment(..)
  , noEnvironment
  -- * Utility functions
  , (@$)
  , catchFormError
  , catchFormErrorM
  , eitherForm
  , getFormId
  , getFormInput
  , getFormInput'
  , getFormRange
  , getNamedFormId
  , incrementFormId
  , isInRange
  , mapFormMonad
  , mapResult
  , mapView
  , mkOk
  , retainChildErrors
  , retainErrors
  , runForm
  , runForm_
  , successDecode
  , unitRange
  , view
  , viewForm
  , pureRes
  , liftForm
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Text (Text)
import Ditto.Types
import Ditto.Backend
import Torsor

------------------------------------------------------------------------------
-- Form types
------------------------------------------------------------------------------

-- | The Form's state is just the range of identifiers so far
type FormState m = StateT FormRange m

-- | @ditto@'s representation of a formlet
data Form m input err view a = Form 
  { formDecodeInput :: input -> m (Either err a) -- ^ Decode the value from the input
  , formInitialValue :: m a -- ^ The initial value
  , formFormlet :: FormState m (View err view, Result err (Proved a)) -- ^ A @FormState@ which produces a @View@ and a @Result@
  } deriving (Functor)

instance (Monad m, Monoid view) => Applicative (Form m input err view) where

  pure x = Form (successDecode x) (pure x) $ do
    i <- getFormId
    pure  ( mempty
          , Ok $ Proved
              { pos = FormRange i i
              , unProved = x
              }
          )

  (Form df ivF frmF) <*> (Form da ivA frmA) =
    Form 
      ( \inp -> do 
        f <- df inp
        x <- da inp
        pure (f <*> x) ) 
      (ivF <*> ivA) 
      ( do
        ((view1, fok), (view2, aok)) <-
          bracketState $ do
            res1 <- frmF
            incrementFormRange
            res2 <- frmA
            pure (res1, res2)
        case (fok, aok) of
          (Error errs1, Error errs2) -> pure (view1 <> view2, Error $ errs1 ++ errs2)
          (Error errs1, _) -> pure (view1 <> view2, Error errs1)
          (_, Error errs2) -> pure (view1 <> view2, Error errs2)
          (Ok (Proved (FormRange x _) f), Ok (Proved (FormRange _ y) a)) ->
            pure
              ( view1 <> view2
              , Ok $ Proved
                { pos = FormRange x y
                , unProved = f a
                }
              )
      )

  f1 *> f2 = Form (formDecodeInput f2) (formInitialValue f2) $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v2, r) <- formFormlet f2
    (v1, _) <- formFormlet f1
    pure (v1 <> v2, r)

  f1 <* f2 = Form (formDecodeInput f1) (formInitialValue f1) $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v1, r) <- formFormlet f1
    (v2, _) <- formFormlet f2
    pure (v1 <> v2, r)

instance (Environment m input, Monoid view, FormError input err) => Monad (Form m input err view) where
  form >>= f =
    let mres = fmap snd $ runForm "" form 
    in Form
      (\input -> do
        res <- mres
        case res of
          Error {} -> do
            iv <- formInitialValue form
            formDecodeInput (f iv) input
          Ok (Proved _ x) -> formDecodeInput (f x) input
      ) 
      (do 
        res <- mres
        case res of
          Error {} -> do
            iv <- formInitialValue form
            formInitialValue $ f iv
          Ok (Proved _ x) -> formInitialValue (f x)
      )
      (do
        (View viewF0, res0) <- formFormlet form
        case res0 of
          Error errs0 -> do 
            iv <- lift $ formInitialValue form
            (View viewF, res) <- formFormlet $ f iv
            let errs = case res of
                  Error es -> es
                  Ok {} -> []
            pure (View $ const $ viewF0 errs0 <> viewF errs, Error (errs0 <> errs))
          Ok (Proved _ x) -> fmap (first (\(View v) -> View $ \e -> viewF0 [] <> v e)) $ formFormlet (f x)
      )
  return = pure
  (>>) = (*>) -- way more efficient than the default

instance (Monad m, Monoid view, Semigroup a) => Semigroup (Form m input err view a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid view, Monoid a) => Monoid (Form m input err view a) where
  mempty = pure mempty

instance Functor m => Bifunctor (Form m input err) where
  first = mapView
  second = fmap

errorInitialValue :: String
errorInitialValue = "ditto: Ditto.Core.errorInitialValue was evaluated"

instance (Monad m, Monoid view, FormError input err, Environment m input) => Alternative (Form m input err view) where
  empty = Form 
    failDecodeMDF
    (error errorInitialValue)
    (pure (mempty, Error []))
  formA <|> formB = do
    efA <- formEither formA
    case efA of
      Right{} -> formA
      Left{} -> formB

------------------------------------------------------------------------------
-- Environment
------------------------------------------------------------------------------

-- | The environment typeclass: the interface between ditto and a given framework
class Monad m => Environment m input | m -> input where
  environment :: FormId -> m (Value input)

-- | Run the form, but always return the initial value
newtype NoEnvironment input m a = NoEnvironment { getNoEnvironment ::  m a }
  deriving (Monad, Functor, Applicative)

instance Monad m => Environment (NoEnvironment input m) input where
  environment = noEnvironment

-- | @environment@ which will always return the initial value
noEnvironment :: Applicative m => FormId -> m (Value input)
noEnvironment = const (pure Default)

-- | Run the form, but with a given @environment@ function
newtype WithEnvironment input m a = WithEnvironment { getWithEnvironment :: ReaderT (FormId -> m (Value input)) m a }
  deriving (Monad, Functor, Applicative)

deriving instance Monad m => MonadReader (FormId -> m (Value input)) (WithEnvironment input m)

instance MonadTrans (WithEnvironment input) where
  lift = WithEnvironment . lift

instance Monad m => Environment (WithEnvironment input m) input where
  environment fid = do
    f <- ask
    lift $ f fid

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

failDecodeMDF :: forall m input err a. (Applicative m, FormError input err) => input -> m (Either err a)
failDecodeMDF = const $ pure $ Left err
  where
  mdf :: CommonFormError input
  mdf = MissingDefaultValue
  err :: err
  err = commonFormError mdf

-- | Always succeed decoding
successDecode :: Applicative m => a -> (input -> m (Either err a))
successDecode = const . pure . Right

-- | Common operations on @Form@s

-- | Change the view of a form using a simple function
--
-- This is useful for wrapping a form inside of a \<fieldset\> or other markup element.
mapView :: (Functor m)
  => (view -> view') -- ^ Manipulator
  -> Form m input err view a -- ^ Initial form
  -> Form m input err view' a -- ^ Resulting form
mapView f Form{formDecodeInput, formInitialValue, formFormlet} = 
  Form formDecodeInput formInitialValue (fmap (first (fmap f)) formFormlet)

-- | Increment a form ID
incrementFormId :: FormId -> FormId
incrementFormId fid = add 1 fid

-- | Check if a 'FormId' is contained in a 'FormRange'
isInRange
  :: FormId -- ^ Id to check for
  -> FormRange -- ^ Range
  -> Bool -- ^ If the range contains the id
isInRange a (FormRange b c) = 
     formIdentifier a >= formIdentifier b 
  && formIdentifier a < formIdentifier c

-- | Check if a 'FormRange' is contained in another 'FormRange'
isSubRange
  :: FormRange -- ^ Sub-range
  -> FormRange -- ^ Larger range
  -> Bool -- ^ If the sub-range is contained in the larger range
isSubRange (FormRange a b) (FormRange c d) =
     formIdentifier a >= formIdentifier c 
  && formIdentifier b <= formIdentifier d

-- | Get a @FormId@ from the FormState
getFormId :: Monad m => FormState m FormId
getFormId = do
  FormRange x _ <- get
  pure x

-- | Utility function: Get the current range
getFormRange :: Monad m => FormState m FormRange
getFormRange = get

-- | Get a @FormIdName@ from the FormState
getNamedFormId :: Monad m => Text -> FormState m FormId
getNamedFormId name = do
  FormRange x _ <- get
  pure $ FormIdName name $ formIdentifier x

-- | Turns a @FormId@ into a @FormRange@ by incrementing the base for the end Id
unitRange :: FormId -> FormRange
unitRange i = FormRange i $ add 1 i

bracketState :: Monad m => FormState m a -> FormState m a
bracketState k = do
  FormRange startF1 _ <- get
  res <- k
  FormRange _ endF2 <- get
  put $ FormRange startF1 endF2
  pure res

-- | Utility function: increment the current 'FormId'.
incrementFormRange :: Monad m => FormState m ()
incrementFormRange = do
  FormRange _ endF1 <- get
  put $ unitRange endF1

-- | Run a form
runForm :: Monad m 
  => Text
  -> Form m input err view a
  -> m (View err view, Result err (Proved a))
runForm prefix Form{formFormlet} =
  evalStateT formFormlet (unitRange (FormId prefix (pure 0)))

-- | Run a form, and unwrap the result
runForm_ :: (Monad m)
  => Text
  -> Form m input err view a
  -> m (view , Maybe a)
runForm_ prefix form = do 
  (view', result) <- runForm prefix form
  pure $ case result of
    Error e -> (unView view' e , Nothing)
    Ok x -> (unView view' [], Just (unProved x))

-- | Evaluate a form
--
-- Returns:
--
-- [@Left view@] on failure. The @view@ will have already been applied to the errors.
--
-- [@Right a@] on success.
--
eitherForm :: (Monad m)
  => Text -- ^ Identifier for the form
  -> Form m input err view a -- ^ Form to run
  -> m (Either view a) -- ^ Result
eitherForm id' form = do
  (view', result) <- runForm id' form
  return $ case result of
    Error e -> Left $ unView view' e
    Ok x -> Right (unProved x)

-- | infix mapView: succinctly mix the @view@ dsl and the formlets dsl  @foo \@$ do ..@
infixr 0 @$
(@$) :: Monad m => (view -> view') -> Form m input err view a -> Form m input err view' a
(@$) = mapView

-- | Utility Function: turn a view and pure value into a successful 'FormState'
mkOk
  :: (Monad m)
  => FormId
  -> view
  -> a
  -> FormState m (View err view, Result err (Proved a))
mkOk i view' val = pure
  ( View $ const $ view'
  , Ok ( Proved
      { pos = unitRange i
      , unProved = val
      } )
  )

-- | Lift the errors into the result type. This will cause the form to always 'succeed'
formEither :: Monad m
  => Form m input err view a 
  -> Form m input err view (Either [err] a)
formEither Form{formDecodeInput, formInitialValue, formFormlet} = Form
  (\input -> do 
    res <- formDecodeInput input
    case res of
      Left err -> pure $ Right $ Left [err]
      Right x -> pure $ Right $ Right x
  ) 
  (fmap Right formInitialValue) 
  ( do
    range <- get
    (view', res') <- formFormlet
    let res = case res' of
          Error err -> Left (map snd err)
          Ok (Proved _ x) -> Right x
    pure  
      ( view'
      , Ok $ Proved 
          { pos = range
          , unProved = res
          }
      )
  )

-- | Utility function: Get the current input
getFormInput :: Environment m input => FormState m (Value input)
getFormInput = getFormId >>= getFormInput'

-- | Utility function: Gets the input of an arbitrary 'FormId'.
getFormInput' :: Environment m input => FormId -> FormState m (Value input)
getFormInput' fid = lift $ environment fid

-- | Select the errors for a certain range
retainErrors :: FormRange -> [(FormRange, e)] -> [e]
retainErrors range = map snd . filter ((== range) . fst)

-- | Select the errors originating from this form or from any of the children of
-- this form
retainChildErrors :: FormRange -> [(FormRange, e)] -> [e]
retainChildErrors range = map snd . filter ((`isSubRange` range) . fst)

-- | Turn a @view@ into a @Form@
view :: Monad m => view -> Form m input err view ()
view html = Form (successDecode ()) (pure ()) $ do
  i <- getFormId
  pure  ( View (const html)
        , Ok $ Proved
            { pos = FormRange i i
            , unProved = ()
            }
        )

-- | Change the underlying Monad of the form, usually a @lift@ or newtype
mapFormMonad :: (Monad f)
  => (forall x. m x -> f x)
  -> Form m input err view a
  -> Form f input err view a
mapFormMonad f Form{formDecodeInput, formInitialValue, formFormlet} = Form 
  { formDecodeInput = f . formDecodeInput
  , formInitialValue = f formInitialValue
  , formFormlet = do
      (view', res) <- fstate formFormlet
      pure $ (view', res)
  }
  where
  fstate st = StateT $ f . runStateT st

-- | Catch errors purely
catchFormError :: (Monad m)
  => ([err] -> a)
  -> Form m input err view a
  -> Form m input err view a
catchFormError ferr Form{formDecodeInput, formInitialValue, formFormlet} = Form formDecodeInput formInitialValue $ do
  i <- getFormId
  (View viewf, res) <- formFormlet
  case res of
    Ok _ -> formFormlet
    Error err -> mkOk i (viewf []) (ferr $ fmap snd err)

-- | Catch errors inside @Form@ / @m@
catchFormErrorM :: (Monad m)
  => Form m input err view a
  -> ([err] -> Form m input err view a)
  -> Form m input err view a
catchFormErrorM form@(Form{formDecodeInput, formInitialValue}) e = Form formDecodeInput formInitialValue $ do
  (_, res0) <- formFormlet form
  case res0 of
    Ok _ -> formFormlet form
    Error err -> formFormlet $ e $ map snd err

-- | Map over the @Result@ and @View@ of a form
mapResult :: (Monad m)
  => (Result err (Proved a) -> Result err (Proved a))
  -> (View err view -> View err view)
  -> Form m input err view a
  -> Form m input err view a
mapResult fres fview Form{formDecodeInput, formInitialValue, formFormlet} = Form formDecodeInput formInitialValue $ do
  (view', res) <- formFormlet
  pure (fview view', fres res)

-- | Run the form with no environment, return only the html.
-- This means that the values will always be their defaults
viewForm :: (Monad m)
  => Text -- ^ form prefix
  -> Form m input err view a -- ^ form to view
  -> m view
viewForm prefix form = do
  (v, _) <- getNoEnvironment $ runForm prefix $ mapFormMonad NoEnvironment form
  pure (unView v [])

-- | lift the result of a decoding to a @Form@
pureRes :: (Monad m, Monoid view, FormError input err)
  => a
  -> Either err a
  -> Form m input err view a
pureRes def x' = case x' of
  Right x -> Form (successDecode x) (pure x) $ do
    i <- getFormId
    pure  ( mempty
          , Ok $ Proved
              { pos = FormRange i i
              , unProved = x
              }
          )
  Left e -> Form (successDecode def) (pure def) $ do
    i <- getFormId
    pure ( mempty
         , Error [(FormRange i i, e)]
         )

-- | @Form@ is a @MonadTrans@, but we can't have an instance of it because of the order and kind of its type variables
liftForm :: (Monad m, Monoid view) => m a -> Form m input err view a
liftForm x = Form (const (fmap Right x)) x $ do
  res <- lift x
  i <- getFormId
  pure (mempty, Ok $ Proved (FormRange i i) res)

