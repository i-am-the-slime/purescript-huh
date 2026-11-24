# purescript-huh

Forms for the terminal in PureScript, with [Huh](https://github.com/charmbracelet/huh). The answers
come back in your own types. For PureScript compiled to Go with
[Iris](https://github.com/i-am-the-slime/purescript-analyzer/tree/iris-go).

```purescript
data Burger = Classic | Chickwich | Fishburger

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
```

```
One Chickwich with lettuce, tomatoes for Ada, 15% off.
```

`run` is a [go-io](https://github.com/i-am-the-slime/purescript-go/tree/main/packages/go-io) `IO`
action that waits for the answer, and `log` is `Go.IO.Console`'s. It gives you `Nothing` if the user
gives up with ctrl+c. `Huh.page` puts fields on a page of their
own; without it, fields share a page.

## Fields

| Field | Answer |
| --- | --- |
| `input _ { title, placeholder, charLimit, password, validate }` | `String` |
| `text _ { title, lines, charLimit, validate }` | `String`, several lines |
| `select _ { title, height } [ "Label" /\ value, … ]` | one of your values |
| `multiSelect _ { title, limit } [ "Label" /\ value, … ]` | `Array` of your values |
| `confirm _ { title, affirmative, negative, default }` | `Boolean` |
| `note { title, description }` | `Unit`: just something to read |

Every option has a default, so you only set what you need. `validate` gets the answer and returns
`Just` what's wrong with it, or `Nothing`. Give `select` at least one option.

Pick a look with `Huh.runWith _ { theme = Huh.Dracula } order` (`Charm`, `Dracula`, `Base16`,
`Catppuccin`, `Base`).

## Inside a Bubble Tea program

`useForm` puts a form in a [hooks](../purescript-bubbletea#hooks) component. Show its `view`, and
watch its `status`:

```purescript
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
```

## Example

```bash
cd examples && ./run.sh Burger
```

[purescript-bubbletea](../purescript-bubbletea)'s `Signup` example has a form inside a full-screen app.

## Changes from the old bindings

- `runInput`, `runConfirm` and `runSelect` became fields you combine into one `Form`, run with
  `run`. A select gives you back your own values instead of strings.
- New: `text`, `multiSelect`, `note`, `page`, validation, themes, and `useForm` for forms inside a
  program.
- Huh v2 from `charm.land`.

## Go only

There are no `.js` files: this package only works with Iris's Go backend. Run `./test.sh` for the
specs.
