# purescript-huh

Forms for the terminal in PureScript, with [Huh](https://github.com/charmbracelet/huh). The answers
come back in your own types. For PureScript compiled to Go with
[googoo](https://github.com/i-am-the-slime/iris-go-backend).

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

## Getting it

Take the [Go package set](https://github.com/rowtype-yoga/purescript-go-package-sets) and add Huh,
Bubble Tea and Lip Gloss from GitHub:

```yaml
package:
  name: burgers
  dependencies:
    - effect
    - go-io
    - huh
    - maybe
    - prelude
    - strings
    - tuples
workspace:
  packageSet:
    url: https://raw.githubusercontent.com/rowtype-yoga/purescript-go-package-sets/go-0.1.18-20261126/packages.json
  extraPackages:
    huh:
      git: https://github.com/i-am-the-slime/purescript-huh.git
      ref: v0.1.0
    bubbletea:
      git: https://github.com/i-am-the-slime/purescript-bubbletea.git
      ref: v0.2.0
    lipgloss:
      git: https://github.com/i-am-the-slime/purescript-lipgloss.git
      ref: v0.1.0
```

The burger order is [examples/src/Burger.purs](examples/src/Burger.purs), imports and all.
`googoo run --go --main Burger` takes your order. You need googoo 0.1.18-go.3 or newer and Go 1.27.

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

`useForm` puts a form in a [hooks](https://github.com/i-am-the-slime/purescript-bubbletea#hooks) component. Show its `view`, and
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
cd examples && googoo run --go -p huh-examples --main Burger
```

[purescript-bubbletea](https://github.com/i-am-the-slime/purescript-bubbletea)'s `Signup` example has a form inside a full-screen app.

## Changes from the old bindings

- `runInput`, `runConfirm` and `runSelect` became fields you combine into one `Form`, run with
  `run`. A select gives you back your own values instead of strings.
- New: `text`, `multiSelect`, `note`, `page`, validation, themes, and `useForm` for forms inside a
  program.
- Huh v2 from `charm.land`.

## Go only

There are no `.js` files: this package only works with googoo's Go backend. The specs:

```bash
googoo run --go -p huh --main Test.Main
```
