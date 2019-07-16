{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}

{- |
This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.
-}
module Text.Reform.Generalized where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Trans (lift)
import qualified Data.IntSet as IS
import Data.List (find)
import Data.Maybe (mapMaybe)
import Numeric (readDec)
import Text.Reform.Backend
import Text.Reform.Core
import Text.Reform.Result

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input
  :: (Monad m, FormError error)
  => (input -> Either error a)
  -> (FormId -> a -> view)
  -> a
  -> Form m input error view () a
input fromInput toView initialValue =
  Form $ do
    i <- getFormId
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i initialValue
          , pure $
            Ok
              ( Proved
                { proofs = ()
                , pos = unitRange i
                , unProved = initialValue
                }
              )
          )
      (Found (fromInput -> (Right a))) ->
        pure
          ( View $ const $ toView i a
          , pure $
            Ok
              ( Proved
                { proofs = ()
                , pos = unitRange i
                , unProved = a
                }
              )
          )
      (Found (fromInput -> (Left error))) ->
        pure
          ( View $ const $ toView i initialValue
          , pure $ Error [(unitRange i, error)]
          )
      Missing ->
        pure
          ( View $ const $ toView i initialValue
          , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
          )

-- | used for elements like @\<input type=\"submit\"\>@ which are not always present in the form submission data.
inputMaybe
  :: (Monad m, FormError error)
  => (input -> Either error a)
  -> (FormId -> a -> view)
  -> a
  -> Form m input error view () (Maybe a)
inputMaybe fromInput toView initialValue =
  Form $ do
    i <- getFormId
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i initialValue
          , pure $
            Ok
              ( Proved
                { proofs = ()
                , pos = unitRange i
                , unProved = Just initialValue
                }
              )
          )
      (Found (fromInput -> (Right a))) ->
        pure
          ( View $ const $ toView i a
          , pure $
            Ok
              ( Proved
                { proofs = ()
                , pos = unitRange i
                , unProved = (Just a)
                }
              )
          )
      (Found (fromInput -> (Left error))) ->
        pure
          ( View $ const $ toView i initialValue
          , pure $ Error [(unitRange i, error)]
          )
      Missing ->
        pure
          ( View $ const $ toView i initialValue
          , pure $
            Ok
              ( Proved
                { proofs = ()
                , pos = unitRange i
                , unProved = Nothing
                }
              )
          )

-- | used for elements like @\<input type=\"reset\"\>@ which take a value, but are never present in the form data set.
inputNoData
  :: (Monad m)
  => (FormId -> a -> view)
  -> a
  -> Form m input error view () ()
inputNoData toView a =
  Form $ do
    i <- getFormId
    pure
      ( View $ const $ toView i a
      , pure $
        Ok
          ( Proved
            { proofs = ()
            , pos = unitRange i
            , unProved = ()
            }
          )
      )

-- | used for @\<input type=\"file\"\>@
inputFile
  :: forall m input error view. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input)
  => (FormId -> view)
  -> Form m input error view () (FileType input)
inputFile toView =
  Form $ do
    i <- getFormId
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
          )
      (Found (getInputFile' -> (Right a))) ->
        pure
          ( View $ const $ toView i
          , pure $
            Ok
              ( Proved
                { proofs = ()
                , pos = unitRange i
                , unProved = a
                }
              )
          )
      (Found (getInputFile' -> (Left error))) ->
        pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, error)]
          )
      Missing ->
        pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
          )
  where
    -- just here for the type-signature to make the type-checker happy
    getInputFile' :: (FormError error, ErrorInputType error ~ input) => input -> Either error (FileType input)
    getInputFile' = getInputFile

-- | used for groups of checkboxes, @\<select multiple=\"multiple\"\>@ boxes
inputMulti
  :: forall m input error view a lbl. (Functor m, FormError error, ErrorInputType error ~ input, FormInput input, Monad m)
  => [(a, lbl)] -- ^ value, label, initially checked
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> (a -> Bool) -- ^ isChecked/isSelected initially
  -> Form m input error view () [a]
inputMulti choices mkView isSelected =
  Form $ do
    i <- getFormId
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
          view <- mkView i <$> augmentChoices choices'
          mkOk i view vals
      Missing ->
        -- just means that no checkboxes were checked
        do
          view <- mkView i <$> augmentChoices (map (\(x, y) -> (x, y, False)) choices)
          mkOk i view []
      (Found v) ->
        do
          let readDec' str = case readDec str of
                [(n, [])] -> n
                _ -> (-1) -- FIXME: should probably pure an internal error?
              keys = IS.fromList $ map readDec' $ getInputStrings v
              (choices', vals) =
                foldr
                  ( \(i, (a, lbl)) (c, v) ->
                    if IS.member i keys
                    then ((a, lbl, True) : c, a : v)
                    else ((a, lbl, False) : c, v)
                  )
                  ([], []) $
                  zip [0..] choices
          view <- mkView i <$> augmentChoices choices'
          mkOk i view vals
  where
    augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m input [(FormId, Int, lbl, Bool)]
    augmentChoices choices = mapM augmentChoice (zip [0..] choices)
    augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
    augmentChoice (vl, (a, lbl, checked)) =
      do
        incFormId
        i <- getFormId
        pure (i, vl, lbl, checked)

-- | radio buttons, single @\<select\>@ boxes
inputChoice
  :: forall a m error input lbl view. (Functor m, FormError error, ErrorInputType error ~ input, FormInput input, Monad m)
  => (a -> Bool) -- ^ is default
  -> [(a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> Form m input error view () a
inputChoice isDefault choices mkView =
  Form $ do
    i <- getFormId
    inp <- getFormInput' i
    case inp of
      Default ->
        do
          let (choices', def) = markSelected choices
          view <- mkView i <$> augmentChoices choices'
          mkOk' i view def
      Missing ->
        -- can happen if no choices where checked
        do
          let (choices', def) = markSelected choices
          view <- mkView i <$> augmentChoices choices'
          mkOk' i view def
      (Found v) ->
        do
          let readDec' :: String -> Int
              readDec' str = case readDec str of
                [(n, [])] -> n
                _ -> (-1) -- FIXME: should probably pure an internal error?
              (Right str) = getInputString v :: Either error String -- FIXME
              key = readDec' str
              (choices', mval) =
                foldr
                  ( \(i, (a, lbl)) (c, v) ->
                    if i == key
                    then ((a, lbl, True) : c, Just a)
                    else ((a, lbl, False) : c, v)
                  )
                  ([], Nothing) $
                  zip [0..] choices
          view <- mkView i <$> augmentChoices choices'
          case mval of
            Nothing ->
              pure
                ( View $ const $ view
                , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
                )
            (Just val) -> mkOk i view val
  where
    mkOk' i view (Just val) = mkOk i view val
    mkOk' i view Nothing =
      pure
        ( View $ const $ view
        , pure $ Error [(unitRange i, commonFormError MissingDefaultValue)]
        )
    markSelected :: [(a, lbl)] -> ([(a, lbl, Bool)], Maybe a)
    markSelected cs =
      foldr
        ( \(a, lbl) (vs, ma) ->
          if isDefault a
          then ((a, lbl, True) : vs, Just a)
          else ((a, lbl, False) : vs, ma)
        )
        ([], Nothing)
        cs
    augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m input [(FormId, Int, lbl, Bool)]
    augmentChoices choices = mapM augmentChoice (zip [0..] choices)
    augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
    augmentChoice (vl, (_a, lbl, selected)) =
      do
        incFormId
        i <- getFormId
        pure (i, vl, lbl, selected)

-- | radio buttons, single @\<select\>@ boxes
inputChoiceForms
  :: forall a m error input lbl view proof. (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input)
  => a
  -> [(Form m input error view proof a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, FormId, view, lbl, Bool)] -> view) -- ^ function which generates the view
  -> Form m input error view proof a
inputChoiceForms def choices mkView =
  Form $ do
    i <- getFormId -- id used for the 'name' attribute of the radio buttons
    inp <- getFormInput' i
    case inp of
      Default ->
        -- produce view for GET request
        do
          choices' <- mapM viewSubForm =<< augmentChoices (selectFirst choices)
          let view = mkView i choices'
          mkOk' i view def
      Missing ->
        -- shouldn't ever happen...
        do
          choices' <- mapM viewSubForm =<< augmentChoices (selectFirst choices)
          let view = mkView i choices'
          mkOk' i view def
      (Found v) ->
        do
          let readDec' str = case readDec str of
                [(n, [])] -> n
                _ -> (-1) -- FIXME: should probably pure an internal error?
              (Right str) = getInputString v :: Either error String -- FIXME
              key = readDec' str
          choices' <- augmentChoices $ markSelected key (zip [0..] choices)
          (choices'', mres) <-
            foldM
              ( \(views, res) (fid, val, iview, frm, lbl, selected) -> do
                incFormId
                if selected
                then do
                    (v, mres) <- unForm frm
                    res' <- lift $ lift mres
                    case res' of
                      (Ok ok) -> do
                        pure (((fid, val, iview, unView v [], lbl, selected) : views), pure res')
                      (Error errs) -> do
                        pure (((fid, val, iview, unView v errs, lbl, selected) : views), pure res')
                else do
                    (v, _) <- unForm frm
                    pure ((fid, val, iview, unView v [], lbl, selected) : views, res)
              )
              ([], pure $ Error [(unitRange i, commonFormError (InputMissing i))])
              (choices')
          let view = mkView i (reverse choices'')
          pure (View (const view), mres)
  where
    -- | Utility Function: turn a view and pure value into a successful 'FormState'
    mkOk'
      :: (Monad m)
      => FormId
      -> view
      -> a
      -> FormState m input (View error view, m (Result error (Proved proof a)))
    mkOk' i view val =
      pure
        ( View $ const $ view
        , pure $ Error []
        )
    selectFirst :: [(Form m input error view proof a, lbl)] -> [(Form m input error view proof a, lbl, Bool)]
    selectFirst ((frm, lbl) : fs) = (frm, lbl, True) : map (\(frm', lbl') -> (frm', lbl', False)) fs
    markSelected :: Int -> [(Int, (Form m input error view proof a, lbl))] -> [(Form m input error view proof a, lbl, Bool)]
    markSelected n choices =
      map (\(i, (f, lbl)) -> (f, lbl, i == n)) choices
    viewSubForm :: (FormId, Int, FormId, Form m input error view proof a, lbl, Bool) -> FormState m input (FormId, Int, FormId, view, lbl, Bool)
    viewSubForm (fid, vl, iview, frm, lbl, selected) =
      do
        incFormId
        (v, _) <- unForm frm
        pure (fid, vl, iview, unView v [], lbl, selected)
    augmentChoices :: (Monad m) => [(Form m input error view proof a, lbl, Bool)] -> FormState m input [(FormId, Int, FormId, Form m input error view proof a, lbl, Bool)]
    augmentChoices choices = mapM augmentChoice (zip [0..] choices)
    augmentChoice :: (Monad m) => (Int, (Form m input error view proof a, lbl, Bool)) -> FormState m input (FormId, Int, FormId, Form m input error view proof a, lbl, Bool)
    augmentChoice (vl, (frm, lbl, selected)) =
      do
        incFormId
        i <- getFormId
        incFormId
        iview <- getFormId
        pure (i, vl, iview, frm, lbl, selected)

{-
              case inp of
                (Found v) ->
                    do let readDec' str = case readDec str of
                                            [(n,[])] -> n
                                            _ -> (-1) -- FIXME: should probably pure an internal error?
                           (Right str) = getInputString v :: Either error String -- FIXME
                           key = readDec' str
                           (choices', mval) =
                               foldr (\(i, (a, lbl)) (c, v) ->
                                          if i == key
                                          then ((a,lbl,True) : c, Just a)
                                          else ((a,lbl,False): c,     v))
                                     ([], Nothing) $
                                     zip [0..] choices


-}
-- | used to create @\<label\>@ elements
label
  :: Monad m
  => (FormId -> view)
  -> Form m input error view () ()
label f =
  Form $ do
    id' <- getFormId
    pure
      ( View (const $ f id')
      , pure
        ( Ok $ Proved
          { proofs = ()
          , pos = unitRange id'
          , unProved = ()
          }
        )
      )

-- | used to add a list of error messages to a 'Form'
--
-- This function automatically takes care of extracting only the
-- errors that are relevent to the form element it is attached to via
-- '<++' or '++>'.
errors
  :: Monad m
  => ([error] -> view) -- ^ function to convert the error messages into a view
  -> Form m input error view () ()
errors f =
  Form $ do
    range <- getFormRange
    pure
      ( View (f . retainErrors range)
      , pure
        ( Ok $ Proved
          { proofs = ()
          , pos = range
          , unProved = ()
          }
        )
      )

-- | similar to 'errors' but includes error messages from children of the form as well.
childErrors
  :: Monad m
  => ([error] -> view)
  -> Form m input error view () ()
childErrors f =
  Form $ do
    range <- getFormRange
    pure
      ( View (f . retainChildErrors range)
      , pure
        ( Ok $ Proved
          { proofs = ()
          , pos = range
          , unProved = ()
          }
        )
      )
