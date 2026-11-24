module Burger (main, order, Order) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith, null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Go.IO (launchIO_)
import Go.IO.Console (log)
import Huh (Form)
import Huh as Huh

data Burger = Classic | Chickwich | Fishburger

instance Show Burger where
  show = case _ of
    Classic -> "Charmburger Classic"
    Chickwich -> "Chickwich"
    Fishburger -> "Fishburger"

type Order = { burger :: Burger, toppings :: Array String, name :: String, discount :: Boolean }

order :: Form Order
order = ado
  burger <- Huh.page $ Huh.select _ { title = "Choose your burger" }
    [ "Charmburger Classic" /\ Classic, "Chickwich" /\ Chickwich, "Fishburger" /\ Fishburger ]
  toppings <- Huh.page $ Huh.multiSelect _ { title = "Toppings", limit = 3 }
    [ "Lettuce" /\ "lettuce", "Tomatoes" /\ "tomatoes", "Cheese" /\ "cheese", "Jalapeños" /\ "jalapeños" ]
  name <- Huh.page $ Huh.input _ { title = "What's your name?", validate = \n -> if null n then Just "We need a name for the order" else Nothing }
  discount <- Huh.confirm _ { title = "Would you like 15% off?", default = true }
  in { burger, toppings, name, discount }

main :: Effect Unit
main = launchIO_ do
  answer <- Huh.run order
  case answer of
    Nothing -> log "No burger today."
    Just { burger, toppings, name, discount } -> log $
      "One " <> show burger <> " with " <> withToppings toppings <> " for " <> name
        <> (if discount then ", 15% off." else ".")
  where
  withToppings = case _ of
    [] -> "nothing on it"
    toppings -> joinWith ", " toppings
