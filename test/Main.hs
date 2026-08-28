module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Ditto
import Ditto.Generalized.Named qualified as Named
import Ditto.Generalized.Unnamed qualified as Unnamed
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Data.Text qualified as T

-- | Minimal backend input type for file-upload tests.
newtype TestInput = TestInput Text
  deriving stock (Show)
  deriving newtype (Eq)

instance FormInput TestInput where
  type FileType TestInput = Text
  getInputTexts (TestInput t) = [t]
  getInputFile (TestInput t) = Right t

instance FormError TestInput Text where
  commonFormError = commonFormErrorText (T.pack . show)

main :: IO ()
main = do
  testEncodeFormId
  testResultApplicative
  testValueInstances
  testViewForm
  testNamedInput
  testUnnamedInput
  testIreqIopt
  testProofs
  testProofDecode
  testHoistForm
  testAlternative
  testCatchFormError
  testInputMulti
  testInputChoice
  testInputFile
  putStrLn "OK"

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
  unless (expected == actual) $ do
    hPutStrLn stderr $ "FAIL: " <> label
    hPutStrLn stderr $ "  expected: " <> show expected
    hPutStrLn stderr $ "  got:      " <> show actual
    exitFailure

testEncodeFormId :: IO ()
testEncodeFormId = do
  assertEq "encodeFormId numbered"
    "user-val-0.1.2"
    (encodeFormId (FormId "user" (0 :| [1, 2])))
  assertEq "encodeFormId named"
    "email"
    (encodeFormId (FormIdName "email" 3))
  assertEq "formIdentifier numbered" 0 (formIdentifier (FormId "user" (0 :| [1])))
  assertEq "formIdentifier named" 3 (formIdentifier (FormIdName "email" 3))

range0 :: FormRange
range0 = FormRange (FormIdName "a" 0) (FormIdName "a" 1)

testResultApplicative :: IO ()
testResultApplicative = do
  assertEq "Result accumulates errors"
    (Error [(range0, "a" :: Text), (range0, "b")] :: Result Text Int)
    (Error [(range0, "a")] <*> Error [(range0, "b")])
  assertEq "Result Ok <*> Ok"
    (Ok (3 :: Int) :: Result Text Int)
    (Ok (+ 1) <*> Ok (2 :: Int))

testValueInstances :: IO ()
testValueInstances = do
  assertEq "Found <*> Found" (Found (3 :: Int)) (Found (+ 1) <*> Found (2 :: Int))
  assertEq "Missing <|> Found" (Found (1 :: Int)) (Missing <|> Found 1)
  assertEq "Found <> Found" (Found ("ab" :: Text)) (Found "a" <> Found "b")

testViewForm :: IO ()
testViewForm = do
  let html = runIdentity $ viewForm "f" (view ("hello" :: Text))
  assertEq "viewForm renders defaults" ("hello" :: Text) html

evalForm
  :: [(Text, Text)]
  -> Form (WithEnvironment Text Identity) Text Text view a
  -> Either view a
evalForm pairs form =
  runIdentity $ flip runReaderT lookupFn $ getWithEnvironment $ eitherForm "f" form
  where
    lookupFn fid = pure $ maybe Missing Found (lookup (encodeFormId fid) pairs)

evalFormTest
  :: [(Text, TestInput)]
  -> Form (WithEnvironment TestInput Identity) TestInput Text view a
  -> Either view a
evalFormTest pairs form =
  runIdentity $ flip runReaderT lookupFn $ getWithEnvironment $ eitherForm "f" form
  where
    lookupFn fid = pure $ maybe Missing Found (lookup (encodeFormId fid) pairs)

evalFormDefault
  :: Form (WithEnvironment Text Identity) Text Text view a
  -> Either view a
evalFormDefault form =
  runIdentity $ flip runReaderT (const (pure Default)) $ getWithEnvironment $ eitherForm "f" form

evalFormTestDefault
  :: Form (WithEnvironment TestInput Identity) TestInput Text view a
  -> Either view a
evalFormTestDefault form =
  runIdentity $ flip runReaderT (const (pure Default)) $ getWithEnvironment $ eitherForm "f" form

decodeSubmitted
  :: Text
  -> Form (WithEnvironment Text Identity) Text Text view a
  -> Either Text a
decodeSubmitted raw form =
  runIdentity $
    runReaderT
      (getWithEnvironment (formDecodeInput form raw))
      (const (pure (Found raw)))

textField
  :: Text
  -> Text
  -> Form (WithEnvironment Text Identity) Text Text Text Text
textField name initial =
  Named.input name Right (\_fid val -> val) initial

testNamedInput :: IO ()
testNamedInput = do
  let form = (,) <$> textField "first" "a" <*> textField "last" "b"
  assertEq "named input reads environment"
    (Right ("Ada", "Lovelace") :: Either Text (Text, Text))
    (evalForm [("first", "Ada"), ("last", "Lovelace")] form)
  assertEq "named input uses defaults when missing"
    (Left "ab" :: Either Text (Text, Text))
    (evalForm [] form)

unnamedText
  :: Text
  -> Form (WithEnvironment Text Identity) Text Text Text Text
unnamedText initial =
  Unnamed.input Right (\_fid val -> val) initial

testUnnamedInput :: IO ()
testUnnamedInput = do
  let form = (,) <$> unnamedText "a" <*> unnamedText "b"
  assertEq "unnamed input enumerates ids"
    (Right ("Ada", "Lovelace") :: Either Text (Text, Text))
    (evalForm [("f-val-0", "Ada"), ("f-val-1", "Lovelace")] form)

readInt :: Text -> Either Text Int
readInt t = maybe (Left "not an int") Right (readMaybe (T.unpack t))

testIreqIopt :: IO ()
testIreqIopt = do
  assertEq "ireq success"
    (Right (42 :: Int) :: Either Text Int)
    (evalForm [("age", "42")] (Named.ireq "age" readInt 0))
  assertEq "ireq missing"
    (Left (mempty :: Text) :: Either Text Int)
    (evalForm [] (Named.ireq "age" readInt 0))
  assertEq "iopt missing is Nothing"
    (Right Nothing :: Either Text (Maybe Int))
    (evalForm [] (Named.iopt "age" readInt Nothing))
  assertEq "iopt found"
    (Right (Just (7 :: Int)) :: Either Text (Maybe Int))
    (evalForm [("age", "7")] (Named.iopt "age" readInt Nothing))

testProofs :: IO ()
testProofs = do
  let parsed =
        transformEither
          (pure "12" :: Form Identity Text Text Text String)
          (\s -> if s == "12" then Right (12 :: Int) else Left ("bad" :: Text))
          (const 0)
  assertEq "transformEither success"
    (Right (12 :: Int))
    (runIdentity $ eitherForm "f" parsed)
  let dec = prove (pure "42" :: Form Identity Text Text Text String) (decimal T.pack 0)
  assertEq "decimal proof"
    (Right (42 :: Int))
    (runIdentity $ eitherForm "f" dec)
  let nonempty = prove (pure [1, 2 :: Int] :: Form Identity Text Text Text [Int]) (notNullProof ("empty" :: Text))
  assertEq "notNullProof"
    (Right [1, 2 :: Int])
    (runIdentity $ eitherForm "f" nonempty)

testProofDecode :: IO ()
testProofDecode = do
  let decoded = transformEither (textField "amount" "0") readInt (const 0)
  assertEq "prove decode valid"
    (Right (7 :: Int))
    (decodeSubmitted "7" decoded)
  assertEq "prove decode invalid"
    (Left ("not an int" :: Text))
    (decodeSubmitted "nope" decoded)
  assertEq "prove submit valid"
    (Right (9 :: Int))
    (evalForm [("amount", "9")] decoded)

pickLabels :: FormId -> [Named.Choice Text Text] -> Text
pickLabels _ = T.intercalate "," . map Named.choiceLabel

testHoistForm :: IO ()
testHoistForm = do
  let inner = view ("ok" :: Text) :: Form Identity Text Text Text ()
      outer = hoistForm id inner
  assertEq "hoistForm Identity"
    ("ok" :: Text)
    (runIdentity $ viewForm "f" outer)

testAlternative :: IO ()
testAlternative = do
  let failing = Named.ireq "x" readInt 0 :: Form (WithEnvironment Text Identity) Text Text Text Int
      fallback = pure (99 :: Int) :: Form (WithEnvironment Text Identity) Text Text Text Int
      combined = failing <|> fallback
  assertEq "Alternative fallback"
    (Right (99 :: Int))
    (evalForm [] combined)

testCatchFormError :: IO ()
testCatchFormError = do
  let form =
        catchFormError
          (const (0 :: Int))
          (Named.ireq "x" readInt 999 :: Form (WithEnvironment Text Identity) Text Text Text Int)
  assertEq "catchFormError recovery"
    (Right (0 :: Int))
    (evalForm [] form)

parseTags :: Text -> Either Text [Text]
parseTags inp = Right (T.words inp)

testInputMulti :: IO ()
testInputMulti = do
  let form =
        Named.inputMulti
          "tags"
          [("a", "alpha"), ("b", "beta")]
          parseTags
          pickLabels
          (== "a")
  assertEq "inputMulti default selection"
    (Right ["a"] :: Either Text [Text])
    (evalFormDefault form)
  assertEq "inputMulti submitted values"
    (Right ["b"] :: Either Text [Text])
    (evalForm [("tags", "b")] form)
  assertEq "inputMulti multiple submitted values"
    (Right ["a", "b"] :: Either Text [Text])
    (evalForm [("tags", "a b")] form)

testInputChoice :: IO ()
testInputChoice = do
  let form =
        Named.inputChoice
          "mode"
          (== "b")
          (("a", "A") :| [("b", "B")])
          (\inp -> Right inp)
          pickLabels
  assertEq "inputChoice default"
    (Right ("b" :: Text))
    (evalForm [] form)
  assertEq "inputChoice submitted"
    (Right ("a" :: Text))
    (evalForm [("mode", "a")] form)

testInputFile :: IO ()
testInputFile = do
  let form = Named.inputFile "upload" (\_ -> "widget" :: Text)
  assertEq "inputFile initial render"
    (Right (mempty :: Text))
    (evalFormTestDefault form)
  assertEq "inputFile missing on submit"
    (Left ("widget" :: Text))
    (evalFormTest [] form)
  assertEq "inputFile submitted"
    (Right ("payload" :: Text))
    (evalFormTest [("upload", TestInput "payload")] form)
  assertEq "inputFile decode"
    (Right ("payload" :: Text))
    (runIdentity $ runReaderT (getWithEnvironment (formDecodeInput form (TestInput "payload"))) (const (pure Default)))
