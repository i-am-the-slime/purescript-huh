-- | Forms for the terminal, with answers in your own types.
-- |
-- |     data Plan = Free | Pro
-- |
-- |     signup :: Form { name :: String, plan :: Plan, newsletter :: Boolean }
-- |     signup = ado
-- |       name <- Huh.input _ { title = "What's your name?" }
-- |       plan <- Huh.select _ { title = "Pick a plan" } [ "Free" /\ Free, "Pro" /\ Pro ]
-- |       newsletter <- Huh.confirm _ { title = "Newsletter?" }
-- |       in { name, plan, newsletter }
-- |
-- | Run it on its own with `run`, or inside a component with `useForm`.
module Huh
  ( Form
  , run
  , runWith
  , Options
  , Theme(..)
  , useForm
  , useFormWith
  , FormHandle
  , Status(..)
  , page
  , input
  , InputOptions
  , text
  , TextOptions
  , select
  , SelectOptions
  , multiSelect
  , MultiSelectOptions
  , confirm
  , ConfirmOptions
  , note
  ) where

import Prelude

import Bubbletea as Tea
import Bubbletea.Hooks (Action, Hooks, command, liftIO, useCommand, useEvent)
import Data.Array (catMaybes, index)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Exception (error)
import Go.IO (throwIO)
import Go.IO.Internal (IO(..))

-- | Fields that, filled in, give an `a`. Combine them with `ado` or `<*>`;
-- | they appear in that order.
newtype Form a = Form (IO { entries :: Array Entry, answer :: IO a })

instance Functor Form where
  map f (Form build) = Form do
    built <- build
    pure { entries: built.entries, answer: f <$> built.answer }

instance Apply Form where
  apply (Form buildF) (Form buildA) = Form do
    left <- buildF
    right <- buildA
    pure { entries: left.entries <> right.entries, answer: left.answer <*> right.answer }

instance Applicative Form where
  pure a = Form (pure { entries: [], answer: pure a })

-- | Show these fields on a page of their own.
page :: forall a. Form a -> Form a
page (Form build) = Form do
  built <- build
  pure built { entries = [ pageBreak ] <> built.entries <> [ pageBreak ] }

foreign import pageBreak :: Entry

--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------

data Theme = Charm | Dracula | Base16 | Catppuccin | Base

type Options = { theme :: Theme, showHelp :: Boolean }

defaults :: Options
defaults = { theme: Charm, showHelp: true }

-- | Ask in the terminal, and wait for the answer. `Nothing` if the user gave
-- | up with ctrl+c.
-- |
-- |     main = launchIO_ (Huh.run order >>= …)
run :: forall a. Form a -> IO (Maybe a)
run = runWith identity

runWith :: forall a. (Options -> Options) -> Form a -> IO (Maybe a)
runWith configure (Form build) = do
  built <- build
  form <- buildForm configure built.entries
  IO (runImpl Completed GaveUp Failed form) >>= case _ of
    Completed -> built.answer <#> Just
    GaveUp -> pure Nothing
    Failed reason -> throwIO (error reason)

data Outcome = Completed | GaveUp | Failed String

foreign import runImpl :: forall r. r -> r -> (String -> r) -> NativeForm -> Effect r

-- | Where a form inside a component has got to.
data Status a = Filling | Done a | Aborted

derive instance Eq a => Eq (Status a)

type FormHandle a =
  { view :: Action String
  , status :: Action (Status a)
  }

-- | A form inside a component: it gets every event, you show its `view`
-- | and watch its `status`.
useForm :: forall a. Form a -> Hooks (FormHandle a)
useForm = useFormWith identity

useFormWith :: forall a. (Options -> Options) -> Form a -> Hooks (FormHandle a)
useFormWith configure (Form build) = do
  built <- build # liftIO
  form <- buildForm configure built.entries # liftIO
  start <- IO (initImpl form) # liftIO
  useCommand (Tea.fromNative start)
  useEvent \event -> do
    cmd <- IO (updateImpl form (Tea.toNative event)) # liftIO
    command (Tea.fromNative cmd)
  pure
    { view: IO (viewImpl form) # liftIO
    , status: statusOf form built.answer # liftIO
    }
  where
  statusOf form answer = IO (stateImpl form) >>= case _ of
    1 -> Done <$> answer
    2 -> pure Aborted
    _ -> pure Filling

foreign import initImpl :: NativeForm -> Effect Tea.NativeCmd
foreign import updateImpl :: NativeForm -> Tea.NativeMsg -> Effect Tea.NativeCmd
foreign import viewImpl :: NativeForm -> Effect String
foreign import stateImpl :: NativeForm -> Effect Int

buildForm :: (Options -> Options) -> Array Entry -> IO NativeForm
buildForm configure entries = do
  let options = configure defaults
  IO (formImpl entries (themeCode options.theme) options.showHelp)
  where
  themeCode = case _ of
    Charm -> 0
    Dracula -> 1
    Base16 -> 2
    Catppuccin -> 3
    Base -> 4

foreign import formImpl :: Array Entry -> Int -> Boolean -> Effect NativeForm

--------------------------------------------------------------------------------
-- Fields
--------------------------------------------------------------------------------

type InputOptions =
  { title :: String
  , description :: String
  , placeholder :: String
  -- | `0` for no limit.
  , charLimit :: Int
  , password :: Boolean
  -- | `Just` what's wrong with an answer, `Nothing` if it's fine.
  , validate :: String -> Maybe String
  }

-- | One line of text.
input :: (InputOptions -> InputOptions) -> Form String
input configure = Form do
  let o = configure { title: "", description: "", placeholder: "", charLimit: 0, password: false, validate: const Nothing }
  field <- IO (inputImpl o.title o.description o.placeholder o.charLimit o.password (problemOf o.validate))
  pure { entries: [ stringEntry field ], answer: IO (readStringImpl field) }

foreign import inputImpl :: String -> String -> String -> Int -> Boolean -> (String -> String) -> Effect StringField
foreign import stringEntry :: StringField -> Entry
foreign import readStringImpl :: StringField -> Effect String

type TextOptions =
  { title :: String
  , description :: String
  , placeholder :: String
  , lines :: Int
  -- | `0` for no limit.
  , charLimit :: Int
  , validate :: String -> Maybe String
  }

-- | Several lines of text.
text :: (TextOptions -> TextOptions) -> Form String
text configure = Form do
  let o = configure { title: "", description: "", placeholder: "", lines: 5, charLimit: 0, validate: const Nothing }
  field <- IO (textImpl o.title o.description o.placeholder o.lines o.charLimit (problemOf o.validate))
  pure { entries: [ stringEntry field ], answer: IO (readStringImpl field) }

foreign import textImpl :: String -> String -> String -> Int -> Int -> (String -> String) -> Effect StringField

type SelectOptions =
  { title :: String
  , description :: String
  -- | How many options show at once; `0` for all of them.
  , height :: Int
  }

-- | One of the options, each a label and the value you get. Give it at least one.
select :: forall a. (SelectOptions -> SelectOptions) -> Array (String /\ a) -> Form a
select configure choices = Form do
  let o = configure { title: "", description: "", height: 0 }
  field <- IO (selectImpl o.title o.description (map fst choices) o.height)
  pure { entries: [ choiceEntry field ], answer: IO (readChoiceImpl field) >>= chosen }
  where
  chosen i = case index choices i of
    Just choice -> pure (snd choice)
    Nothing -> throwIO (error "Huh.select needs at least one option")

foreign import selectImpl :: String -> String -> Array String -> Int -> Effect ChoiceField
foreign import choiceEntry :: ChoiceField -> Entry
foreign import readChoiceImpl :: ChoiceField -> Effect Int

type MultiSelectOptions =
  { title :: String
  , description :: String
  -- | How many may be picked; `0` for any number.
  , limit :: Int
  }

-- | Any of the options.
multiSelect :: forall a. (MultiSelectOptions -> MultiSelectOptions) -> Array (String /\ a) -> Form (Array a)
multiSelect configure choices = Form do
  let o = configure { title: "", description: "", limit: 0 }
  field <- IO (multiSelectImpl o.title o.description (map fst choices) o.limit)
  pure { entries: [ choicesEntry field ], answer: IO (readChoicesImpl field) <#> map (index choices >>> map snd) >>> catMaybes }

foreign import multiSelectImpl :: String -> String -> Array String -> Int -> Effect ChoicesField
foreign import choicesEntry :: ChoicesField -> Entry
foreign import readChoicesImpl :: ChoicesField -> Effect (Array Int)

type ConfirmOptions =
  { title :: String
  , description :: String
  , affirmative :: String
  , negative :: String
  , default :: Boolean
  }

-- | Yes or no.
confirm :: (ConfirmOptions -> ConfirmOptions) -> Form Boolean
confirm configure = Form do
  let o = configure { title: "", description: "", affirmative: "Yes", negative: "No", default: false }
  field <- IO (confirmImpl o.title o.description o.affirmative o.negative o.default)
  pure { entries: [ boolEntry field ], answer: IO (readBoolImpl field) }

foreign import confirmImpl :: String -> String -> String -> String -> Boolean -> Effect BoolField
foreign import boolEntry :: BoolField -> Entry
foreign import readBoolImpl :: BoolField -> Effect Boolean

-- | Something to read, no answer.
note :: { title :: String, description :: String } -> Form Unit
note { title, description } = Form do
  entry <- IO (noteImpl title description)
  pure { entries: [ entry ], answer: pure unit }

foreign import noteImpl :: String -> String -> Effect Entry

problemOf :: (String -> Maybe String) -> String -> String
problemOf validate answer = fromMaybe "" (validate answer)

--------------------------------------------------------------------------------
-- Go
--------------------------------------------------------------------------------

foreign import data Entry :: Type
foreign import data StringField :: Type
foreign import data ChoiceField :: Type
foreign import data ChoicesField :: Type
foreign import data BoolField :: Type
foreign import data NativeForm :: Type
