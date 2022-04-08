{-# LANGUAGE 
    NamedFieldPuns
  , ScopedTypeVariables
  , LambdaCase
  , TypeFamilies
#-}

-- | This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.

module Ditto.Generalized.Internal where

import Control.Monad.State.Class (get)
import Control.Monad.Trans (lift)
import Data.Either
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable (for)
import Ditto.Backend
import Ditto.Core
import Ditto.Types

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input :: forall m input err a view. (Environment m input, FormError input err)
  => FormState m FormId
  -> (input -> Either err a)
  -> (FormId -> a -> view)
  -> a
  -> Form m input err view a
input formSId fromInput toView initialValue =
  Form (pure . fromInput) (pure initialValue) $ do
    i <- formSId
    v <- getFormInput' i
    case v of
      Default -> pure
        ( View $ const $ toView i initialValue
        , Ok $ Proved
            { pos = unitRange i
            , unProved = initialValue
            }
        )
      Found inp -> case fromInput inp of
        Right a -> pure
          ( View $ const $ toView i a
          , Ok $ Proved
              { pos = unitRange i
              , unProved = a
              }
          )
        Left err -> pure
          ( View $ const $ toView i initialValue
          , Error [(unitRange i, err)]
          )
      Missing -> pure
        ( View $ const $ toView i initialValue
        , Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
        )

-- | this is necessary in order to basically map over the decoding function
inputList :: forall m input err a view view'. (Monad m, FormError input err, Environment m input)
  => FormState m FormId
  -> (input -> m (Either err [a])) -- ^ decoding function for the list
  -> ([view] -> view') -- ^ how to concatenate views
  -> [a] -- ^ initial values
  -> view' -- ^ view to generate in the fail case
  -> (a -> Form m input err view a)
  -> Form m input err view' [a]
inputList formSId fromInput viewCat initialValue defView createForm =
  Form fromInput (pure initialValue) $ do
    i <- formSId
    v <- getFormInput' i
    case v of
      Default -> do
        views <- for initialValue $ \x -> do
          (View viewF, _) <- formFormlet $ createForm x 
          pure $ viewF []
        pure
          ( View $ const $ viewCat views
          , Ok $ Proved
              { pos = unitRange i
              , unProved = initialValue
              }
          )
      Found inp -> lift (fromInput inp) >>= \case
        Right xs -> do
          views <- for xs $ \x -> do
            (View viewF, _) <- formFormlet $ createForm x 
            pure $ viewF []
          pure
            ( View $ const $ viewCat views
            , Ok $ Proved
                { pos = unitRange i
                , unProved = xs
                }
            )
        Left err -> do
          let err' = [(unitRange i, err)]
          views <- for initialValue $ \x -> do
            (View viewF, _) <- formFormlet $ createForm x 
            pure $ viewF err'
          pure
            ( View $ const $ viewCat views
            , Error err'
            )
      Missing -> do
        pure
          ( View $ const defView
          , Ok $ Proved
              { pos = unitRange i
              , unProved = []
              }
          )

-- | used for elements like @\<input type=\"submit\"\>@ which are not always present in the form submission data.
inputMaybe :: (Monad m, FormError input err, Environment m input)
  => FormState m FormId
  -> (input -> Either err a)
  -> (FormId -> Maybe a -> view)
  -> Maybe a
  -> Form m input err view (Maybe a)
inputMaybe i' fromInput toView initialValue =
  Form (pure . fmap Just . fromInput) (pure initialValue) $ do
    i <- i'
    v <- getFormInput' i
    case v of
      Default -> pure
          ( View $ const $ toView i initialValue
          , Ok ( Proved
              { pos = unitRange i
              , unProved = initialValue
              })
          )
      Found x -> case fromInput x of
        Right a -> pure
          ( View $ const $ toView i (Just a)
          , Ok $ Proved
              { pos = unitRange i
              , unProved = Just a
              }
          )
        Left err -> pure
          ( View $ const $ toView i initialValue
          , Error [(unitRange i, err)]
          )
      Missing -> pure
        ( View $ const $ toView i initialValue
        , Ok $ Proved
            { pos = unitRange i
            , unProved = Nothing
            }
        )

-- | used for elements like @\<input type=\"reset\"\>@ which take a value, but are never present in the form data set.
inputNoData :: (Monad m)
  => FormState m FormId
  -> (FormId -> view)
  -> Form m input err view ()
inputNoData i' toView =
  Form (successDecode ()) (pure ()) $ do
    i <- i'
    pure
      ( View $ const $ toView i
      , Ok ( Proved
          { pos = unitRange i
          , unProved = ()
          })
      )

-- | used for @\<input type=\"file\"\>@
inputFile :: forall m ft input err view. (Monad m, FormInput input, FormError input err, Environment m input, ft ~ FileType input, Monoid ft)
  => FormState m FormId
  -> (FormId -> view)
  -> Form m input err view (FileType input)
inputFile i' toView =
  Form (pure . getInputFile) (pure mempty) $ do -- FIXME
    i <- i'
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i
          , Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
          )
      Found x -> case getInputFile x of
        Right a -> pure
          ( View $ const $ toView i
          , Ok ( Proved
              { pos = unitRange i
              , unProved = a
              })
          )
        Left err -> pure
          ( View $ const $ toView i
          , Error [(unitRange i, err)]
          )
      Missing ->
        pure
          ( View $ const $ toView i
          , Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) ::err)]
          )

-- | used for groups of checkboxes, @\<select multiple=\"multiple\"\>@ boxes
inputMulti :: forall m input err view a lbl. (FormError input err, FormInput input, Environment m input, Eq a)
  => FormState m FormId
  -> [(a, lbl)] -- ^ value, label, initially checked
  -> (input -> Either err [a])
  -> (FormId -> [Choice lbl a] -> view) -- ^ function which generates the view
  -> (a -> Bool) -- ^ isChecked/isSelected initially
  -> Form m input err view [a]
inputMulti i' choices fromInput mkView isSelected =
  Form (pure . fromInput) (pure $ map fst choices) $ do
    i <- i'
    inp <- getFormInput' i
    case inp of
      Default -> do
        let (choices', vals) =
              foldr
                ( \(a, lbl) (cs, vs) ->
                  if isSelected a
                  then ((a, lbl, True) : cs, a : vs)
                  else ((a, lbl, False) : cs, vs)
                )
                ([], [])
                choices
        view' <- mkView i <$> augmentChoices i choices'
        mkOk i view' vals
      Missing -> do
        -- just means that no checkboxes were checked
        view' <- mkView i <$> augmentChoices i (map (\(x, y) -> (x, y, False)) choices)
        mkOk i view' []
      Found v -> do
        let keys = fromRight [] $ fromInput v
            (choices', vals) =
              foldr
                ( \(a, lbl) (c, v0) ->
                  if a `elem` keys
                  then ((a, lbl, True) : c, a : v0)
                  else ((a, lbl, False) : c, v0)
                )
                ([], [])
                choices
        view' <- mkView i <$> augmentChoices i choices'
        mkOk i view' vals

augmentChoices :: (Monad m) => FormId ->  [(a, lbl, Bool)] -> FormState m [Choice lbl a]
augmentChoices i choices = mapM (augmentChoice i) choices

augmentChoice :: (Monad m) => FormId -> (a, lbl, Bool) -> FormState m (Choice lbl a)
augmentChoice i (a, lbl, selected) = do
  pure $ Choice i lbl selected a

-- | a choice for inputChoice
data Choice lbl a = Choice
  { choiceFormId :: FormId -- ^ the formId
  , choiceLabel :: lbl -- ^ <label>
  , choiceIsSelected :: Bool -- ^ is the choice selected
  , choiceVal :: a -- ^ the haskell value of the choice
  }

-- | radio buttons, single @\<select\>@ boxes
inputChoice :: forall a m err input lbl view. (FormError input err, FormInput input, Monad m, Eq a, Monoid view, Environment m input)
  => FormState m FormId
  -> (a -> Bool) -- ^ is default
  -> NonEmpty (a, lbl) -- ^ value, label
  -> (input -> Either err a)
  -> (FormId -> [Choice lbl a] -> view) -- ^ function which generates the view
  -> Form m input err view a
inputChoice i' isDefault choices@(headChoice :| _) fromInput mkView = do
  let f = case find isDefault (fmap fst choices) of
        Nothing -> Form (pure . fromInput) (pure $ fst headChoice)
        Just defChoice -> Form (pure . fromInput) (pure defChoice)
  f $ do
    i <- i'
    inp <- getFormInput' i
    case inp of
      Default -> do
        let (choices', def) = markSelected choices
        view' <- mkView i <$> augmentChoices i choices'
        mkOk' i view' def
      Missing -> do
        -- can happen if no choices where checked
        let (choices', def) = markSelected choices
        view' <- mkView i <$> augmentChoices i choices'
        mkOk' i view' def
      Found v -> do
        case fromInput v of
          Left err -> do
            let choices' =
                  foldr
                    ( \(a, lbl) c -> (a, lbl, False) : c )
                    []
                    choices
            view' <- mkView i <$> augmentChoices i choices'
            pure
              ( View $ const view'
              , Error [(unitRange i, err)]
              )
          Right key -> do
            let (choices', mval) =
                  foldr
                    ( \(a, lbl) (c, v0) ->
                      if key == a
                      then ((a, lbl, True) : c, Just a)
                      else ((a, lbl, False) : c, v0)
                    )
                    ([], Nothing)
                    choices
            view' <- mkView i <$> augmentChoices i choices'
            case mval of
              Nothing -> pure
                ( View $ const view'
                , Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
                )
              Just val -> mkOk i view' val
  where
    mkOk' i view' (Just val) = mkOk i view' val
    mkOk' i view' Nothing =
      pure
        ( View $ const view'
        , Error [(unitRange i, commonFormError (MissingDefaultValue :: CommonFormError input) :: err)]
        )
    markSelected :: Foldable f => f (a, lbl) -> ([(a, lbl, Bool)], Maybe a)
    markSelected cs =
      foldr
        ( \(a, lbl) (vs, ma) ->
          if isDefault a
          then ((a, lbl, True) : vs, Just a)
          else ((a, lbl, False) : vs, ma)
        )
        ([], Nothing)
        cs

-- | used to create @\<label\>@ elements
label :: Monad m
  => FormState m FormId
  -> (FormId -> view)
  -> Form m input err view ()
label i' f = Form (successDecode ()) (pure ()) $ do
  id' <- i'
  pure
    ( View (const $ f id')
    , Ok $ Proved
        { pos = unitRange id'
        , unProved = ()
        }
    )

-- | used to add a list of err messages to a 'Form'
--
-- This function automatically takes care of extracting only the
-- errors that are relevent to the form element it is attached to via
-- '<*' or '*>'.
errors :: Monad m
  => ([err] -> view) -- ^ function to convert the err messages into a view
  -> Form m input err view ()
errors f = Form (successDecode ()) (pure ()) $ do
  range <- get
  pure
    ( View (f . retainErrors range)
    , Ok $ Proved
        { pos = range
        , unProved = ()
        }
    )

-- | similar to 'errors' but includes err messages from children of the form as well.
childErrors :: Monad m
  => ([err] -> view)
  -> Form m input err view ()
childErrors f = Form (successDecode ()) (pure ()) $ do
  range <- get
  pure
    ( View (f . retainChildErrors range)
    , Ok $ Proved
        { pos = range
        , unProved = ()
        }
    )

-- | modify the view of a form based on its child errors
withChildErrors :: Monad m
  => (view -> [err] -> view)
  -> Form m input err view a
  -> Form m input err view a
withChildErrors f Form{formDecodeInput, formInitialValue, formFormlet} = Form formDecodeInput formInitialValue $ do
  (View v, r) <- formFormlet
  range <- get
  pure
    ( View $ \x ->
        let errs = retainChildErrors range x
        in f (v x) errs
    , r
    )

-- | modify the view of a form based on its errors
withErrors :: Monad m
  => (view -> [err] -> view)
  -> Form m input err view a
  -> Form m input err view a
withErrors f Form{formDecodeInput, formInitialValue, formFormlet} = Form formDecodeInput formInitialValue $ do
  (View v, r) <- formFormlet
  range <- get
  pure
    ( View $ \x ->
        let errs = retainErrors range x
        in f (v x) errs
    , r
    )
