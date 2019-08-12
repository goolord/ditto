{-# LANGUAGE 
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses 
  , NamedFieldPuns
  , ScopedTypeVariables
  , StandaloneDeriving
  , FunctionalDependencies
  , OverloadedStrings
  , RankNTypes
#-}

module Ditto.Core where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Text (Text)
import Ditto.Types
import Ditto.Backend
import Torsor

class Monad m => Environment m input | m -> input where
  environment :: FormId -> m (Value input)

noEnvironment :: Applicative m => FormId -> m (Value input)
noEnvironment = const (pure Default)

type FormState m = StateT FormRange m

data Form m input err view a = Form 
  { formDecodeInput :: input -> m (Either err a)
  , formInitialValue :: m a
  , formFormlet :: FormState m (View err view, m (Result err (Proved a)))
  } deriving (Functor)

instance (Monad m, Monoid view) => Applicative (Form m input err view) where
  pure x = Form (successDecode x) (pure x) $ do
    i <- getFormId
    pure  ( mempty
          , pure $ Ok $ Proved
              { pos = FormRange i i
              , unProved = x
              }
          )
  (Form df ivF frmF) <*> (Form da ivA frmA) =
    Form (\inp -> do 
        f <- df inp
        x <- da inp
        pure (f <*> x)
      ) (ivF <*> ivA) $ do
      ((view1, mfok), (view2, maok)) <-
        bracketState $ do
          res1 <- frmF
          incrementFormRange
          res2 <- frmA
          pure (res1, res2)
      fok <- lift $ mfok
      aok <- lift $ maok
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

newtype Phantom a b = Phantom { getPhantom :: b }

missingDefaultValue :: forall input err. FormError input err => Phantom input err
missingDefaultValue = Phantom $ commonFormError (MissingDefaultValue :: CommonFormError input)

instance (Environment m input, Monoid view, FormError input err) => Monad (Form m input err view) where
  form >>= f = form *> 
    let mres = do 
        (~_, mr) <- runForm "" form 
        mr
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
        res <- lift mres
        case res of
          Error {} -> do 
            iv <- lift $ formInitialValue form
            formFormlet $ f iv
          Ok (Proved _ x) -> formFormlet $ f x
      )

instance (Monad m, Monoid view, Semigroup a) => Semigroup (Form m input err view a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid view, Monoid a) => Monoid (Form m input err view a) where
  mempty = pure mempty

instance Functor m => Bifunctor (Form m input err) where
  first = mapView
  second = fmap

errorInitialValue :: String
errorInitialValue = "ditto: Ditto.Core.errorInitalValue was evaluated"

instance (Monad m, Monoid view, FormError input err, Environment m input) => Alternative (Form m input err view) where
  empty = Form 
    failDecode
    (error errorInitialValue)
    (pure (mempty, pure $ Error mempty))
  formA <|> formB = do
    efA <- formEither formA
    case efA of
      Right{} -> formA
      Left{} -> formB

failDecode :: forall m input err a. (Applicative m, FormError input err) => input -> m (Either err a)
failDecode = const (pure $ Left (commonFormError (MissingDefaultValue :: CommonFormError input) :: err))

successDecode :: Applicative m => a -> (input -> m (Either err a))
successDecode = const . pure . Right

instance (Environment m input, Monoid view, FormError input err) => MonadError [err] (Form m input err view) where
  throwError es = Form failDecode (error errorInitialValue) $ do
    range <- get
    pure (mempty, pure $ Error $ fmap ((,) range) es)
  catchError form@(Form{formDecodeInput, formInitialValue}) e = Form formDecodeInput formInitialValue $ do
    (_, mres0) <- formFormlet form
    res0 <- lift mres0
    case res0 of
      Ok _ -> formFormlet form
      Error err -> formFormlet $ e $ map snd err

-- | Change the view of a form using a simple function
--
-- This is useful for wrapping a form inside of a \<fieldset\> or other markup element.
mapView
  :: (Functor m)
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

getFormId :: Monad m => FormState m FormId
getFormId = do
  FormRange x _ <- get
  pure x

getNamedFormId :: Monad m => Text -> FormState m FormId
getNamedFormId name = do
  FormRange x _ <- get
  pure $ FormIdCustom name $ formIdentifier x

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

runForm :: Monad m 
  => Text
  -> Form m input err view a
  -> m (View err view, m (Result err (Proved a)))
runForm prefix Form{formFormlet} =
  evalStateT formFormlet (unitRange (FormId prefix (pure 0)))

-- | infix mapView: succinctly mix the @view@ dsl and the formlets dsl  @foo @$ do ..@
infixr 0 @$
(@$) :: Monad m => (view -> view) -> Form m input err view a -> Form m input err view a
(@$) = mapView

-- | Utility Function: turn a view and pure value into a successful 'FormState'
mkOk
  :: (Monad m)
  => FormId
  -> view
  -> a
  -> FormState m (View err view, m (Result err (Proved a)))
mkOk i view' val = pure
  ( View $ const $ view'
  , pure $ Ok ( Proved
      { pos = unitRange i
      , unProved = val
      } )
  )

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
    (view', mres) <- formFormlet
    res' <- lift mres
    let res = case res' of
          Error err -> Left (map snd err)
          Ok (Proved _ x) -> Right x
    pure  
      ( view'
      , pure $ Ok $ Proved 
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

view :: Monad m => view -> Form m input err view ()
view html = Form (successDecode ()) (pure ()) $ do
  i <- getFormId
  pure  ( View (const html)
        , pure $ Ok $ Proved
            { pos = FormRange i i
            , unProved = ()
            }
        )

mapFormMonad :: Monad f => (forall x. m x -> f x) -> Form m input err view a -> Form f input err view a
mapFormMonad f Form{formDecodeInput, formInitialValue, formFormlet} = Form 
  { formDecodeInput = f . formDecodeInput
  , formInitialValue = f formInitialValue
  , formFormlet = do
      (view', mres) <- fstate formFormlet
      pure $ (view', f mres)
  }
  where
  fstate st = StateT $ f . runStateT st

catchFormError :: Monad m
  => ([err] -> a)
  -> Form m input err view a
  -> Form m input err view a
catchFormError ferr Form{formDecodeInput, formInitialValue, formFormlet} = Form formDecodeInput formInitialValue $ do
  i <- getFormId
  (View viewf, mres) <- formFormlet
  res <- lift mres
  case res of
    Ok _ -> formFormlet
    Error err -> mkOk i (viewf []) (ferr $ fmap snd err)

viewForm :: (Monad m)
  => Text -- ^ form prefix
  -> Form m input err view a -- ^ form to view
  -> m view
viewForm prefix form = do
  (v, _) <- runForm prefix form
  pure (unView v [])

