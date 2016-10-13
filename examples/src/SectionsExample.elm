module SectionsExample exposing (..)

import Menu
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import String
import Json.Decode as Json


main : Program Never
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Menu.subscription


type alias Model =
    { people : List Person
    , peopleByCentury : List Century
    , autoState : Menu.State
    , howManyToShow : Int
    , query : String
    , showMenu : Bool
    }


init : Model
init =
    { people = presidents
    , peopleByCentury = presidentsByCentury
    , autoState = Menu.empty
    , howManyToShow = List.length presidents
    , query = ""
    , showMenu = False
    }


type Msg
    = SetQuery String
    | SetAutoState Menu.Msg
    | Reset
    | SelectPerson String
    | OnFocus
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            { model | query = newQuery, showMenu = True } ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Menu.update updateConfig autoMsg model.howManyToShow model.autoState (acceptablePeople model)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        Reset ->
            { model | autoState = Menu.resetToFirstItem updateConfig (acceptablePeople model) model.howManyToShow model.autoState } ! []

        SelectPerson id ->
            { model
                | query =
                    List.filter (\person -> person.name == id) model.people
                        |> List.head
                        |> Maybe.withDefault (Person "" 0 "" "")
                        |> .name
                , autoState = Menu.empty
                , showMenu = False
            }
                ! []

        OnFocus ->
            model ! []

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.customDecoder keyCode
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else
                        Err "not handling that key"
                )
            )
    in
        div []
            [ input
                [ onInput SetQuery
                , onFocus OnFocus
                , onWithOptions "keydown" options dec
                , class "menu-input"
                , value model.query
                ]
                []
            , if model.showMenu then
                viewMenu model
              else
                div [] []
            ]


acceptablePeopleByCentury : Model -> List Century
acceptablePeopleByCentury { query, peopleByCentury } =
    let
        lowerQuery =
            String.toLower query

        filteredCentury century people =
            { century | people = people }

        filterPeople century =
            filteredCentury century <| List.filter (String.contains lowerQuery << String.toLower << .name) century.people
    in
        List.map filterPeople peopleByCentury


acceptablePeople : Model -> List Person
acceptablePeople { query, people } =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .name) people


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "menu-menu" ]
        [ Html.map SetAutoState (Menu.viewWithSections viewConfig model.howManyToShow model.autoState (acceptablePeopleByCentury model)) ]


updateConfig : Menu.UpdateConfig Msg Person
updateConfig =
    Menu.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Nothing
                else if code == 13 then
                    Maybe.map SelectPerson maybeId
                else
                    Just Reset
        , onTooLow = Just Reset
        , onTooHigh = Just Reset
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectPerson id
        , separateSelections = True
        }


viewConfig : Menu.ViewWithSectionsConfig Person Century
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ classList [ ( "menu-item", True ), ( "key-selected", keySelected ), ( "mouse-selected", mouseSelected ) ]
                , id person.name
                ]
            , children = [ Html.text person.name ]
            }
    in
        Menu.viewWithSectionsConfig
            { toId = .name
            , ul = [ class "menu-list-with-sections" ]
            , li = customizedLi
            , section = sectionConfig
            }


sectionConfig : Menu.SectionConfig Person Century
sectionConfig =
    Menu.sectionConfig
        { toId = .title
        , getData = .people
        , ul = [ class "menu-section-list" ]
        , li =
            \section ->
                { nodeType = "div"
                , attributes = [ class "menu-section-item" ]
                , children =
                    [ div [ class "menu-section-box" ]
                        [ strong [ class "menu-section-text" ] [ text section.title ]
                        ]
                    ]
                }
        }



-- PEOPLE


type alias Century =
    { title : String
    , people : List Person
    }


presidentsByCentury : List Century
presidentsByCentury =
    [ { title = "1700s"
      , people =
            [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
            , Person "John Adams" 1735 "Braintree" "Massachusetts"
            , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
            , Person "James Madison" 1751 "Port Conway" "Virginia"
            , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
            , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
            , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
            , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
            , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
            , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
            , Person "John Tyler" 1790 "Charles City County" "Virginia"
            , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
            , Person "James K. Polk" 1795 "Pineville" "North Carolina"
            ]
      }
    , { title = "1800s"
      , people =
            [ Person "Millard Fillmore" 1800 "Summerhill" "New York"
            , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
            , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
            , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
            , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
            , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
            , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
            , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
            , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
            , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
            , Person "William McKinley" 1843 "Niles" "Ohio"
            , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
            , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
            , Person "Theodore Roosevelt" 1858 "New York City" "New York"
            , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
            , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
            , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
            , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
            , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
            , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
            ]
      }
    , { title = "1900s"
      , people =
            [ Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
            , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
            , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
            , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
            , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
            , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
            , Person "Jimmy Carter" 1924 "Plains" "Georgia"
            , Person "George W. Bush" 1946 "New Haven" "Connecticut"
            , Person "Bill Clinton" 1946 "Hope" "Arkansas"
            , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
            ]
      }
    ]


type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    }


presidents : List Person
presidents =
    [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
    , Person "John Adams" 1735 "Braintree" "Massachusetts"
    , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
    , Person "James Madison" 1751 "Port Conway" "Virginia"
    , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
    , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
    , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
    , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
    , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
    , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
    , Person "John Tyler" 1790 "Charles City County" "Virginia"
    , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
    , Person "James K. Polk" 1795 "Pineville" "North Carolina"
    , Person "Millard Fillmore" 1800 "Summerhill" "New York"
    , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
    , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
    , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
    , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
    , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
    , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
    , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
    , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
    , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
    , Person "William McKinley" 1843 "Niles" "Ohio"
    , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
    , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
    , Person "Theodore Roosevelt" 1858 "New York City" "New York"
    , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
    , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
    , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
    , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
    , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
    , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
    , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
    , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
    , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
    , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
    , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
    , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
    , Person "Jimmy Carter" 1924 "Plains" "Georgia"
    , Person "George W. Bush" 1946 "New Haven" "Connecticut"
    , Person "Bill Clinton" 1946 "Hope" "Arkansas"
    , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
    ]
