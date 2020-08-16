# russelldavies/elm-ui-searchbox

An [Elm UI](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
searchbox (searchable autocomplete dropdown).

This is essentially an
[Element.Input.text](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Input#text)
with extra parts to handle showing and filtering a list of options.

Checkout this [Ellie Demo](TODO).

## Usage

This searchbox stores just enough state to manage itself but does not store
your options (you store them in whatever format is best for your app), the
selected option, nor the text of the underlying `Element.Input.text`. It
adheres to one of the core rules of the Elm Architecture: never put functions
in your `Model` or `Msg` types. So `SearchBox.state` belongs in your model but
its configuration does not.

As such, a minimal working model looks like:

```elm
type alias Model a =
    { selectedOption = Maybe a
    , searchBoxText = String
    , searchBox = SearchBox.State
    }
```
