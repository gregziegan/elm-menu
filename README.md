# Elm Menu

[![Build Status](https://travis-ci.org/thebritican/elm-menu.svg?branch=master)](https://travis-ci.org/thebritican/elm-menu)

> Note: This will be the replacement for `elm-autocomplete`. Elm Autocomplete should
> be a drop-in autocomplete for people to use in the 90% case. This menu, that
> the autocomplete will be built upon, exists for crazier use-cases.

## Demo

Checkout the [landing page](http://thebritican.github.io/elm-menu) inspired by [React Autosuggest](http://react-autosuggest.js.org/)'s page design

Menus have _just enough_ functionality to be tedious to implement again and again.
This is a flexible library for handling the needs of many different menus.

Your data is stored separately; keep it in whatever shape makes the most sense for your application.

Make an issue if this library cannot handle your scenario and we'll investigate together if it makes sense in the larger
context!

I recommend looking at the [examples](https://github.com/thebritican/elm-menu/tree/master/examples) before diving into the API or source code!

## Usage Rules

  - Always put `Menu.State` in your model.
  - Never put _any_ `Config` in your model.

Design inspired by [elm-sortable-table](https://github.com/evancz/elm-sortable-table/).

Read about why these usage rules are good rules [here](https://github.com/evancz/elm-sortable-table/tree/1.0.0#usage-rules).

The [API Design Session video](https://www.youtube.com/watch?v=KSuCYUqY058) w/ Evan Czaplicki (@evancz) that brought us to this API.


## Installation

```
elm-package install thebritican/elm-menu
```

## Setup
```elm
import Menu


type alias Model =
  { autoState = Menu.State -- Own the State of the menu in your model
  , query = String -- Perhaps you want to filter by a string?
  , people = List Person -- The data you want to list and filter
  }

-- Let's filter the data however we want
acceptablePeople : String -> List Person -> List Person
acceptablePeople query people =
  let
      lowerQuery =
          String.toLower query
  in
      List.filter (String.contains lowerQuery << String.toLower << .name) people

-- Set up what will happen with your menu updates
updateConfig : Menu.UpdateConfig Msg Person
updateConfig =
    Menu.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 13 then
                    Maybe.map SelectPerson maybeId
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectPerson id
        , separateSelections = False
        }

type Msg
  = SetMenuState Menu.Msg

update : Msg -> Model -> Model
update msg { autoState, query, people, howManyToShow } =
  case msg of
    SetMenuState autoMsg ->
      let
        (newState, maybeMsg) =
          Menu.update updateConfig autoMsg howManyToShow autoState (acceptablePeople query people)
      in
        { model | autoState = newState }

-- setup for your menu view
viewConfig : Menu.ViewConfig Msg Person
viewConfig =
  let
    customizedLi keySelected mouseSelected person =
      { attributes = [ classList [ ("menu-item", True), ("is-selected", keySelected || mouseSelected) ] ]
      , children = [ Html.text person.name ]
      }
  in
    Menu.viewConfig
      { toId = .name
      , ul = [ class "menu-list" ] -- set classes for your list
      , li = customizedLi -- given selection states and a person, create some Html!
      }

-- and let's show it! (See an example for the full code snippet)
view : Model -> Html Msg
view { autoState, query, people } =
  div []
      [ input [ onInput SetQuery ]
      , Html.App.map SetMenuState (Menu.view viewConfig 5 autoState (acceptablePeople query people))
      ]

```
