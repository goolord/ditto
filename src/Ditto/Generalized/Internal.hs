{-# LANGUAGE 
    NamedFieldPuns
  , OverloadedStrings
  , ScopedTypeVariables
#-}

-- This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.

module Ditto.Generalized.Internal where

import Control.Monad.State.Class (get)
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
        , pure $ Ok
            ( Proved
              { pos = unitRange i
              , unProved = initialValue
              }
            )
        )
      Found inp -> case fromInput inp of
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

-- | this is necessary in order to basically map over the decoding function
inputList :: forall m input err a view. (Monad m, FormError input err, Environment m input)
  => FormState m FormId
  -> (input -> Either err [a])
  -> ([view] -> view)
  -> a
  -> [a]
  -> (a -> Form m input err view a)
  -> Form m input err view [a]
inputList formSId fromInput viewCat failVal initialValue createForm =
  Form (pure . fromInput) (pure initialValue) $ do
    i <- formSId
    v <- getFormInput' i
    case v of
      Default -> do
        let ivs' = case initialValue of
              [] -> [failVal]
              _ -> initialValue
        viewFs <- for ivs' $ \x -> do
          (View viewF, _) <- formFormlet $ createForm x 
          pure viewF
        pure
          ( View $ const $ viewCat $ fmap ($ []) viewFs
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = ivs'
                }
              )
          )
      Found inp -> case fromInput inp of
        Right xs -> do
          viewFs <- for xs $ \x -> do
            (View viewF, _) <- formFormlet $ createForm x 
            pure viewF
          pure
            ( View $ const $ viewCat $ fmap ($ []) viewFs
            , pure $
              Ok
                ( Proved
                  { pos = unitRange i
                  , unProved = xs
                  }
                )
            )
        Left err -> do
          (View viewF, _) <- formFormlet $ createForm failVal
          pure
            ( View $ const $ viewF []
            , pure $ Error [(unitRange i, err)]
            )
      Missing -> do 
        (View viewF, _) <- formFormlet $ createForm failVal
        pure
          ( View $ const $ viewF []
          , pure $ Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
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
          ( View $ const $ toView i (Just a)
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = (Just a)
                }
              )
          )
        Left err -> pure
          ( View $ const $ toView i initialValue
          , pure $ Error [(unitRange i, err)]
          )
      Missing -> pure
        ( View $ const $ toView i initialValue
        , pure $
          Ok
            ( Proved
              { pos = unitRange i
              , unProved = Nothing
              }
            )
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
      , pure $
        Ok
          ( Proved
            { pos = unitRange i
            , unProved = ()
            }
          )
      )

-- | used for @\<input type=\"file\"\>@
inputFile :: forall m input err view. (Monad m, FormInput input, FormError input err, Environment m input)
  => FormState m FormId
  -> (FormId -> view)
  -> Form m input err view (FileType input)
inputFile i' toView =
  Form (pure . getInputFile') undefined $ do
    i <- i'
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
          )
      Found x -> case getInputFile' x of
        Right a -> pure
          ( View $ const $ toView i
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = a
                }
              )
          )
        Left err -> pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, err)]
          )
      Missing ->
        pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) ::err)]
          )
  where
    -- just here for the type-signature to make the type-checker happy
    getInputFile' :: (FormError input err) => input -> Either err (FileType input)
    getInputFile' = getInputFile

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
      Default ->
        do
          let (choices', vals) =
                foldr
                  ( \(a, lbl) (cs, vs) ->
                    if isSelected a
                    then ((a, lbl, True) : cs, a : vs)
                    else ((a, lbl, False) : cs, vs)
                  )
                  ([], [])
                  choices
          view' <- mkView i <$> augmentChoices choices'
          mkOk i view' vals
      Missing -> do
        -- just means that no checkboxes were checked
        view' <- mkView i <$> augmentChoices (map (\(x, y) -> (x, y, False)) choices)
        mkOk i view' []
      Found v -> do
        let keys = either (const []) id $ fromInput v
            (choices', vals) =
              foldr
                ( \(a, lbl) (c, v0) ->
                  if a `elem` keys
                  then ((a, lbl, True) : c, a : v0)
                  else ((a, lbl, False) : c, v0)
                )
                ([], []) $
                choices
        view' <- mkView i <$> augmentChoices choices'
        mkOk i view' vals
  where
    augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m [Choice lbl a]
    augmentChoices choices' = mapM augmentChoice choices'
    augmentChoice :: (Monad m) => (a, lbl, Bool) -> FormState m (Choice lbl a)
    augmentChoice (a, lbl, selected) = do
      i <- i'
      pure $ Choice i lbl selected a

data Choice lbl a = Choice
  { choiceFormId :: FormId
  , choiceLabel :: lbl
  , choiceIsSelected :: Bool
  , choiceVal :: a
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
        view' <- mkView i <$> augmentChoices choices'
        mkOk' i view' def
      Missing -> do
        -- can happen if no choices where checked
        let (choices', def) = markSelected choices
        view' <- mkView i <$> augmentChoices choices'
        mkOk' i view' def
      Found v -> do
        case fromInput v of
          Left err -> do
            let choices' =
                  foldr
                    ( \(a, lbl) c -> (a, lbl, False) : c )
                    []
                    choices
            view' <- mkView i <$> augmentChoices choices'
            pure
              ( View $ const view'
              , pure $ Error [(unitRange i, err)]
              )
          Right key -> do
            let (choices', mval) =
                  foldr
                    ( \(a, lbl) (c, v0) ->
                      if key == a
                      then ((a, lbl, True) : c, Just a)
                      else ((a, lbl, False) : c, v0)
                    )
                    ([], Nothing) $
                    choices
            view' <- mkView i <$> augmentChoices choices'
            case mval of
              Nothing ->
                pure
                  ( View $ const view'
                  , pure $ Error [(unitRange i, commonFormError (InputMissing i :: CommonFormError input) :: err)]
                  )
              Just val -> mkOk i view' val
  where
    mkOk' i view' (Just val) = mkOk i view' val
    mkOk' i view' Nothing =
      pure
        ( View $ const $ view'
        , pure $ Error [(unitRange i, commonFormError (MissingDefaultValue :: CommonFormError input) :: err)]
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
    augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m [Choice lbl a]
    augmentChoices choices' = mapM augmentChoice choices'
    augmentChoice :: (Monad m) => (a, lbl, Bool) -> FormState m (Choice lbl a)
    augmentChoice (a, lbl, selected) = do
      i <- i'
      pure $ Choice i lbl selected a

-- | used to create @\<label\>@ elements
label :: Monad m
  => FormState m FormId
  -> (FormId -> view)
  -> Form m input err view ()
label i' f = Form (successDecode ()) (pure ()) $ do
  id' <- i'
  pure
    ( View (const $ f id')
    , pure
      ( Ok $ Proved
        { pos = unitRange id'
        , unProved = ()
        }
      )
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
    , pure
      ( Ok $ Proved
        { pos = range
        , unProved = ()
        }
      )
    )

-- | similar to 'errors' but includes err messages from children of the form as well.
childErrors :: Monad m
  => ([err] -> view)
  -> Form m input err view ()
childErrors f = Form (successDecode ()) (pure ()) $ do
  range <- get
  pure
    ( View (f . retainChildErrors range)
    , pure
      ( Ok $ Proved
        { pos = range
        , unProved = ()
        }
      )
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
        let errs = retainChildErrors range x
        in f (v x) errs
    , r
    )
