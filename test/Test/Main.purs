module Test.Main where

import Prelude

import Bubbletea as Tea
import Bubbletea.Hooks (Component, component, get, render)
import Bubbletea.Key (KeyCode(..), ctrl, key)
import Bubbletea.Testing (Step(..))
import Bubbletea.Testing as Testing
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String (Pattern(..), contains, length)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Go.IO (toAff)
import Huh (Form, Status(..))
import Huh as Huh
import Test.Exit (exit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, evalSpecT)
import Test.Spec.Summary (successful)

main :: Effect Unit
main = launchAff_ do
  results <- un Identity (evalSpecT (defaultConfig { exit = false }) [ consoleReporter ] specs)
  unless (successful results) (exit 1 # liftEffect)

data Plan = Free | Pro

derive instance Eq Plan

instance Show Plan where
  show = case _ of
    Free -> "Free"
    Pro -> "Pro"

signup :: Form { name :: String, plan :: Plan, extras :: Array String }
signup = ado
  name <- Huh.input _ { title = "Name", validate = \n -> if length n < 2 then Just "too short" else Nothing }
  plan <- Huh.select _ { title = "Plan" } [ "Free" /\ Free, "Pro" /\ Pro ]
  extras <- Huh.multiSelect _ { title = "Extras" } [ "Stickers" /\ "stickers", "Mug" /\ "mug" ]
  in { name, plan, extras }

app :: Component Unit Void
app = component \_ -> do
  form <- Huh.useForm signup
  render do
    status <- form.status
    view <- form.view
    pure $ Tea.view case status of
      Done answer -> "Welcome " <> answer.name
      Aborted -> "Maybe next time"
      Filling -> view

detailed :: Component Unit Void
detailed = component \_ -> do
  form <- Huh.useForm signup
  render do
    status <- form.status
    pure $ Tea.view case status of
      Done answer -> answer.name <> " on " <> show answer.plan <> " with " <> show answer.extras
      _ -> "not yet"

specs :: Spec Unit
specs = describe "Huh" do
  it "shows the first field" do
    result <- Testing.runComponent [] app # toAff
    result.frame `shouldSatisfy` contains (Pattern "Name")

  it "gives typed answers once the form is done" do
    result <- Testing.runComponent
      [ Type "Ada", Press (key Enter), Press (key Down), Press (key Enter), Press (key Space), Press (key Enter) ]
      detailed # toAff
    result.frame `shouldSatisfy` contains (Pattern "Ada on Pro with [\"stickers\"]")

  it "welcomes you when you're done" do
    result <- Testing.runComponent [ Type "Ada", Press (key Enter), Press (key Enter), Press (key Enter) ] app # toAff
    result.frame `shouldSatisfy` contains (Pattern "Welcome Ada")

  it "doesn't let an invalid answer through" do
    result <- Testing.runComponent [ Type "A", Press (key Enter) ] app # toAff
    result.frame `shouldSatisfy` contains (Pattern "too short")

  it "reports giving up" do
    result <- Testing.runComponent [ Press (ctrl 'c') ] app # toAff
    result.frame `shouldSatisfy` contains (Pattern "Maybe next time")
