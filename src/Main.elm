module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)
import String exposing (fromInt)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Increment
    | Decrement


type alias Model =
    { character : Character
    }


tabula_rasa : Character
tabula_rasa =
    { name = "Thuldir"
    , bioform = "Dwarf"
    , class = "Knight"
    , story = "Lord Commander of the Red Knight"
    , hitpoints = 10
    , equipped = []
    , carried = []
    , stats = Stats 0 0 0 0 0 0 0 0 0 0 0
    }


init : Model
init =
    { character = tabula_rasa
    }


type alias Stats =
    { str : Int
    , dex : Int
    , con : Int
    , wis : Int
    , int : Int
    , cha : Int
    , armor : Int
    , basic : Int
    , weapon : Int
    , magic : Int
    , ultimate : Int
    }


type Item
    = Item String Stats


type alias Character =
    { name : String
    , bioform : String
    , class : String
    , story : String
    , hitpoints : Int
    , equipped : List Item
    , carried : List Item
    , stats : Stats
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            (model.character.stats.str + 1)
                |> asStrIn model.character.stats
                |> asStatsIn model.character
                |> asCharIn model

        Decrement ->
            (model.character.stats.str - 1)
                |> asStrIn model.character.stats
                |> asStatsIn model.character
                |> asCharIn model


asStrIn : Stats -> Int -> Stats
asStrIn stats change =
    { stats | str = change }


asStatsIn : Character -> Stats -> Character
asStatsIn char stats =
    { char | stats = stats }


asCharIn : Model -> Character -> Model
asCharIn model char =
    { model | character = char }


view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 100 50
        , Background.color <| Element.rgb255 199 199 199
        ]
    <|
        Element.column
            [ Element.paddingXY 24 10
            , Element.spacingXY 0 8
            , Element.width Element.fill
            , Element.height Element.fill
            , Background.color <| Element.rgb255 255 255 255
            ]
            [ infoRow model.character
            , storyRow model.character
            ]


infoRow : Character -> Element Msg
infoRow char =
    let
        labelStyle =
            [ Font.size 16 ]

        valueStyle =
            [ Element.width Element.fill, Element.paddingXY 10 0 ]
    in
    Element.row
        [ Element.width Element.fill
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        ]
        [ Element.el labelStyle (Element.text "Name:")
        , Element.el valueStyle (Element.text <| char.name)
        , Element.el labelStyle (Element.text "Class:")
        , Element.el valueStyle (Element.text <| char.class)
        , Element.el labelStyle (Element.text "Bioform:")
        , Element.el valueStyle (Element.text <| char.bioform)
        ]


storyRow : Character -> Element Msg
storyRow char =
    let
        labelStyle =
            [ Font.size 16 ]

        valueStyle =
            [ Element.width Element.fill, Element.paddingXY 10 0, Font.italic ]
    in
    Element.row
        [ Element.width Element.fill
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        ]
        [ Element.el labelStyle (Element.text "Story:")
        , Element.el valueStyle (Element.text <| char.story)
        ]


statRow : Character -> Element Msg
statRow char =
    let
        labelStyle =
            [ Font.size 16 ]

        valueStyle =
            [ Element.width Element.fill, Element.paddingXY 10 0, Font.italic ]
    in
    Element.row
        [ Element.width Element.fill
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        ]
        [ Element.el labelStyle (Element.text "Story:")
        , Element.el valueStyle (Element.text <| char.story)
        ]
