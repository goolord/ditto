{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- This module provides helper functions for HTML input elements. These helper functions are not specific to any particular web framework or html library.

module Ditto.Generalized.Named where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Trans (lift)
import Data.Bifunctor
import Numeric (readDec)
import Ditto.Backend
import Ditto.Core
import Ditto.Result
import qualified Data.IntSet as IS

-- | used for constructing elements like @\<input type=\"text\"\>@, which pure a single input value.
input
  :: (Monad m, FormError err)
  => (input -> Either err a)
  -> (FormId -> a -> view)
  -> a
  -> String
  -> Form m input err view a
input fromInput toView initialValue name =
  Form $ do
    i <- getNamedFormId name
    v <- getFormInput' i
    case v of
      Default ->
        pure
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
        , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
        )

-- | used for elements like @\<input type=\"submit\"\>@ which are not always present in the form submission data.
inputMaybe
  :: (Monad m, FormError err)
  => (input -> Either err a)
  -> (FormId -> a -> view)
  -> a
  -> String
  -> Form m input err view (Maybe a)
inputMaybe fromInput toView initialValue name =
  Form $ do
    i <- getNamedFormId name
    v <- getFormInput' i
    case v of
      Default -> pure
          ( View $ const $ toView i initialValue
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = Just initialValue
                }
              )
          )
      Found x -> case fromInput x of
        Right a -> pure
          ( View $ const $ toView i a
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
inputNoData
  :: (Monad m)
  => (FormId -> a -> view)
  -> a
  -> String
  -> Form m input err view ()
inputNoData toView a name =
  Form $ do
    i <- getNamedFormId name
    pure
      ( View $ const $ toView i a
      , pure $
        Ok
          ( Proved
            { pos = unitRange i
            , unProved = ()
            }
          )
      )

-- | used for @\<input type=\"file\"\>@
inputFile
  :: forall m input err view. (Monad m, FormInput input, FormError err, ErrorInputType err ~ input)
  => (FormId -> view)
  -> String
  -> Form m input err view (FileType input)
inputFile toView name =
  Form $ do
    i <- getNamedFormId name
    v <- getFormInput' i
    case v of
      Default ->
        pure
          ( View $ const $ toView i
          , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
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
          , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
          )
  where
    -- just here for the type-signature to make the type-checker happy
    getInputFile' :: (FormError err, ErrorInputType err ~ input) => input -> Either err (FileType input)
    getInputFile' = getInputFile

-- | used for groups of checkboxes, @\<select multiple=\"multiple\"\>@ boxes
inputMulti
  :: forall m input err view a lbl. (Functor m, FormError err, ErrorInputType err ~ input, FormInput input, Monad m)
  => [(a, lbl)] -- ^ value, label, initially checked
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> (a -> Bool) -- ^ isChecked/isSelected initially
  -> String
  -> Form m input err view [a]
inputMulti choices mkView isSelected name =
  Form $ do
    i <- getNamedFormId name
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
      Missing ->
        -- just means that no checkboxes were checked
        do
          view' <- mkView i <$> augmentChoices (map (\(x, y) -> (x, y, False)) choices)
          mkOk i view' []
      Found v -> do
        let readDec' str = case readDec str of
              [(n, [])] -> n
              _ -> (-1) -- FIXME: should probably pure an internal err?
            keys = IS.fromList $ map readDec' $ getInputStrings v
            (choices', vals) =
              foldr
                ( \(i0, (a, lbl)) (c, v0) ->
                  if IS.member i0 keys
                  then ((a, lbl, True) : c, a : v0)
                  else ((a, lbl, False) : c, v0)
                )
                ([], []) $
                zip [0..] choices
        view' <- mkView i <$> augmentChoices choices'
        mkOk i view' vals
  where
    augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m input [(FormId, Int, lbl, Bool)]
    augmentChoices choices' = mapM augmentChoice (zip [0..] choices')
    augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
    augmentChoice (vl, (_, lbl, checked)) =
      do
        incFormId
        i <- getNamedFormId name
        pure (i, vl, lbl, checked)

-- | radio buttons, single @\<select\>@ boxes
inputChoice
  :: forall a m err input lbl view. (Functor m, FormError err, ErrorInputType err ~ input, FormInput input, Monad m)
  => (a -> Bool) -- ^ is default
  -> [(a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, lbl, Bool)] -> view) -- ^ function which generates the view
  -> String
  -> Form m input err view a
inputChoice isDefault choices mkView name =
  Form $ do
    i <- getNamedFormId name
    inp <- getFormInput' i
    case inp of
      Default ->
        do
          let (choices', def) = markSelected choices
          view' <- mkView i <$> augmentChoices choices'
          mkOk' i view' def
      Missing ->
        -- can happen if no choices where checked
        do
          let (choices', def) = markSelected choices
          view' <- mkView i <$> augmentChoices choices'
          mkOk' i view' def
      Found v ->
        do
          let readDec' :: String -> Int
              readDec' str' = case readDec str' of
                [(n, [])] -> n
                _ -> (-1) -- FIXME: should probably pure an internal err?
              estr = getInputString v :: Either err String
              key = second readDec' estr
              (choices', mval) =
                foldr
                  ( \(i0, (a, lbl)) (c, v0) ->
                    if either (const False) (==i0) key
                    then ((a, lbl, True) : c, Just a)
                    else ((a, lbl, False) : c, v0)
                  )
                  ([], Nothing) $
                  zip [0..] choices
          view' <- mkView i <$> augmentChoices choices'
          case mval of
            Nothing ->
              pure
                ( View $ const view'
                , pure $ Error [(unitRange i, commonFormError (InputMissing i))]
                )
            (Just val) -> mkOk i view' val
  where
    mkOk' i view' (Just val) = mkOk i view' val
    mkOk' i view' Nothing =
      pure
        ( View $ const $ view'
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
    augmentChoices choices' = mapM augmentChoice (zip [0..] choices')
    augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
    augmentChoice (vl, (_a, lbl, selected)) =
      do
        incFormId
        i <- getNamedFormId name
        pure (i, vl, lbl, selected)

-- | radio buttons, single @\<select\>@ boxes
inputChoiceForms
  :: forall a m err input lbl view. (Functor m, Monad m, FormError err, ErrorInputType err ~ input, FormInput input)
  => a
  -> [(Form m input err view a, lbl)] -- ^ value, label
  -> (FormId -> [(FormId, Int, FormId, view, lbl, Bool)] -> view) -- ^ function which generates the view
  -> String
  -> Form m input err view a
inputChoiceForms def choices mkView name =
  Form $ do
    i <- getNamedFormId name -- id used for the 'name' attribute of the radio buttons
    inp <- getFormInput' i
    case inp of
      Default ->
        -- produce view for GET request
        do
          choices' <- mapM viewSubForm =<< augmentChoices (selectFirst choices)
          let view' = mkView i choices'
          mkOk' i view' def
      Missing ->
        -- shouldn't ever happen...
        do
          choices' <- mapM viewSubForm =<< augmentChoices (selectFirst choices)
          let view' = mkView i choices'
          mkOk' i view' def
      (Found v) ->
        do
          let readDec' str' = case readDec str' of
                [(n, [])] -> n
                _ -> (-1) -- FIXME: should probably pure an internal err?
              estr = getInputString v :: Either err String
              key = second readDec' estr
          choices' <- augmentChoices $ markSelected key (zip [0..] choices)
          (choices'', mres) <-
            foldM
              ( \(views, res) (fid, val, iview, frm, lbl, selected) -> do
                incFormId
                if selected
                then do
                    (v0, mres) <- unForm frm
                    res' <- lift $ lift mres
                    case res' of
                      Ok{} -> do
                        pure (((fid, val, iview, unView v0 [], lbl, selected) : views), pure res')
                      Error errs -> do
                        pure (((fid, val, iview, unView v0 errs, lbl, selected) : views), pure res')
                else do
                    (v0, _) <- unForm frm
                    pure ((fid, val, iview, unView v0 [], lbl, selected) : views, res)
              )
              ([], pure $ Error [(unitRange i, commonFormError (InputMissing i))])
              (choices')
          let view' = mkView i (reverse choices'')
          pure (View (const view'), mres)
  where
    -- | Utility Function: turn a view and pure value into a successful 'FormState'
    mkOk'
      :: (Monad m)
      => FormId
      -> view
      -> a
      -> FormState m input (View err view, m (Result err (Proved a)))
    mkOk' _ view' _ =
      pure
        ( View $ const view'
        , pure $ Error []
        )
    selectFirst :: [(Form m input err view a, lbl)] -> [(Form m input err view a, lbl, Bool)]
    selectFirst ((frm, lbl) : fs) = (frm, lbl, True) : map (\(frm', lbl') -> (frm', lbl', False)) fs
    selectFirst [] = []
    markSelected :: Either e Int -> [(Int, (Form m input err view a, lbl))] -> [(Form m input err view a, lbl, Bool)]
    markSelected en choices' =
      map (\(i, (f, lbl)) -> (f, lbl, either (const False) (==i) en)) choices'
    viewSubForm :: (FormId, Int, FormId, Form m input err view a, lbl, Bool) -> FormState m input (FormId, Int, FormId, view, lbl, Bool)
    viewSubForm (fid, vl, iview, frm, lbl, selected) =
      do
        incFormId
        (v, _) <- unForm frm
        pure (fid, vl, iview, unView v [], lbl, selected)
    augmentChoices :: (Monad m) => [(Form m input err view a, lbl, Bool)] -> FormState m input [(FormId, Int, FormId, Form m input err view a, lbl, Bool)]
    augmentChoices choices' = mapM augmentChoice (zip [0..] choices')
    augmentChoice :: (Monad m) => (Int, (Form m input err view a, lbl, Bool)) -> FormState m input (FormId, Int, FormId, Form m input err view a, lbl, Bool)
    augmentChoice (vl, (frm, lbl, selected)) =
      do
        incFormId
        i <- getNamedFormId name
        incFormId
        iview <- getFormId
        pure (i, vl, iview, frm, lbl, selected)

{-
              case inp of
                (Found v) ->
                    do let readDec' str = case readDec str of
                                            [(n,[])] -> n
                                            _ -> (-1) -- FIXME: should probably pure an internal err?
                           (Right str) = getInputString v :: Either err String -- FIXME
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
  -> String
  -> Form m input err view ()
label f name =
  Form $ do
    id' <- getNamedFormId name
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
-- '<++' or '++>'.
errors
  :: Monad m
  => ([err] -> view) -- ^ function to convert the err messages into a view
  -> Form m input err view ()
errors f =
  Form $ do
    range <- getFormRange
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
childErrors
  :: Monad m
  => ([err] -> view)
  -> Form m input err view ()
childErrors f =
  Form $ do
    range <- getFormRange
    pure
      ( View (f . retainChildErrors range)
      , pure
        ( Ok $ Proved
          { pos = range
          , unProved = ()
          }
        )
      )
