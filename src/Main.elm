module Main exposing (..)

import Browser
import Dict exposing (Dict)
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


type Stat
    = Str
    | Dex
    | Con
    | Wis
    | Int
    | Cha
    | Armor
    | Basic
    | Weapon
    | Magic
    | Ultimate


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



-- emptyStats : Stats
-- emptyStats = [(Str, 0), (Dex, 0), (Con, 0), (Wis, 0), (Int, 0), (Cha, 0), (Armor, 0), (Basic, 0), (Weapon, 0), (Magic, 0), (Ultimate, 0)]
-- addStats : Stats -> Stats -> Stat
-- addStats s1 s2 =


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
        , Element.paddingXY 100 0
        , Background.color <| Element.rgb255 199 199 199
        , Font.family
            [ Font.external
                { name = "Permanent Marker"
                , url = "https://fonts.googleapis.com/css?family=Permanent+Marker"
                }
            ]
        , Font.size 30
        ]
    <|
        Element.column
            [ Element.paddingXY 24 40
            , Element.spacingXY 0 25
            , Element.width Element.fill
            , Element.height Element.fill
            , Background.color <| Element.rgb255 255 255 255
            ]
            [ infoRow model.character
            , storyRow model.character
            -- , heartRow model.character
            , Element.wrappedRow [ Element.spacingXY 50 10, Element.width Element.fill ]
                [ statCol model.character
                , effortCol model.character
                , equippedCol model.character
                , unEquippedCol model.character
                ]
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
            [ Font.size 18 ]

        valueStyle =
            [ Element.paddingXY 10 0, Font.italic ]
    in
    Element.row
        [ Element.width Element.fill
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Element.paddingEach { bottom = 10, left = 0, right = 0, top = 0 }
        ]
        [ Element.el labelStyle (Element.text "Story:")
        , Element.el valueStyle (Element.text <| char.story)
        , heartRow char
        ]


heartRow : Character -> Element Msg
heartRow char =
    let
        heartCount : Int
        heartCount =
            char.hitpoints // 10

        remainingHearts : Int
        remainingHearts =
            10 - heartCount

        labelStyle =
            [ Font.size 18 ]
    in
    Element.row
        [ Element.spacingXY 5 10
        , Element.paddingXY 20 0
        ]

    <|  [Element.el labelStyle (Element.text "Hit Points:")] ++
        List.repeat heartCount filledHearts
            ++ List.repeat remainingHearts emptyHearts


filledHearts : Element Msg
filledHearts =
    Element.el [ Element.width Element.fill ] <| Element.text "❤️"


emptyHearts : Element Msg
emptyHearts =
    Element.el [ Element.width Element.fill ] <| Element.text "\u{1F90D}"


statCol : Character -> Element Msg
statCol char =
    Element.column
        [ Element.height Element.fill

        -- , Element.width Element.fill
        , Element.spacingXY 32 32
        , Element.alignLeft
        ]
        [ statBlock "Str" char.stats.str 0
        , statBlock "Dex" char.stats.dex 0
        , statBlock "Con" char.stats.con 0
        , statBlock "Int" char.stats.int 0
        , statBlock "Wis" char.stats.wis 0
        , statBlock "Cha" char.stats.cha 0
        ]


effortCol : Character -> Element Msg
effortCol char =
    Element.column
        [ Element.height Element.fill

        -- , Element.width Element.fill
        , Element.spacingXY 32 32
        , Element.alignLeft
        ]
        [ armorBlock "Armor" char.stats.armor 0
        , effortBlock "Basic Effort (D4)" char.stats.basic 0
        , effortBlock "Weapon Effort (D6)" char.stats.weapon 0
        , effortBlock "Magic Effort (D8)" char.stats.magic 0
        , effortBlock "Ultimate Effort (D12)" char.stats.ultimate 0
        ]


equippedCol : Character -> Element Msg
equippedCol char =
    Element.column
        [ Element.height Element.fill
        , Element.paddingXY 130 0
        ]
    <|
        [ Element.row [Element.paddingEach { bottom = 25, left = 0, right = 0, top = 0}]
            [ Element.el [] <| Element.text "📜"
            , Element.el [ Font.size 40, Font.underline ] <| Element.text "Equipped Gear"
            ]
        ]
            ++ List.repeat 10
                (Element.el
                    [ Element.paddingEach { bottom = 8, left = 0, right = 0, top = 8 }
                    , Element.width <| Element.px 140
                    , Font.size 24
                    , Font.underline
                    ]
                 <|
                    Element.text "                                        "
                )

unEquippedCol : Character -> Element Msg
unEquippedCol char =
    Element.column
        [ Element.height Element.fill
        , Element.paddingXY 130 0
        ]
    <|
        [ Element.row [Element.paddingEach { bottom = 25, left = 0, right = 0, top = 0}]
            [ Element.el [] <| Element.text "🧰"
            , Element.el [ Font.size 40, Font.underline ] <| Element.text "Carried Gear"
            ]
        ]
            ++ List.repeat 10
                (Element.el
                    [ Element.paddingEach { bottom = 8, left = 0, right = 0, top = 8 }
                    , Element.width <| Element.px 140
                    , Font.size 24
                    , Font.underline
                    ]
                 <|
                    Element.text "                                        "
                )

statBlock : String -> Int -> Int -> Element Msg
statBlock label basestat lootstat =
    Element.row [ Element.spacing 11, Element.width Element.fill ] <|
        [ Element.el [] <| Element.text label
        , Element.el
            [ Border.solid
            , Border.color <| Element.rgb255 0 0 0
            , Border.widthEach { bottom = 1, left = 1, right = 1, top = 1 }
            , Border.rounded 5
            , Element.paddingXY 15 10
            , Font.center
            , Element.alignRight
            ]
          <|
            Element.text (String.fromInt basestat)
        , Element.column [ Font.size 14, Element.alignRight ]
            [ Element.text "Base"
            , Element.text <| String.fromInt basestat
            , Element.text "Loot"
            , Element.text <| String.fromInt lootstat
            ]
        ]


effortBlock : String -> Int -> Int -> Element Msg
effortBlock label basestat lootstat =
    Element.row [ Element.spacing 11, Element.width Element.fill ] <|
        [ Element.column [] <| List.map (\l -> Element.el [ Element.centerY, Font.size 24 ] <| Element.text l) <| String.split " " label
        , Element.el
            [ Border.solid
            , Border.color <| Element.rgb255 0 0 0
            , Border.widthEach { bottom = 1, left = 1, right = 1, top = 1 }
            , Border.rounded 5
            , Element.paddingXY 15 10
            , Font.center
            , Element.alignRight
            ]
          <|
            Element.text (String.fromInt basestat)
        , Element.column [ Font.size 14, Element.alignRight ]
            [ Element.text "Base"
            , Element.text <| String.fromInt basestat
            , Element.text "Loot"
            , Element.text <| String.fromInt lootstat
            ]
        ]


armorBlock : String -> Int -> Int -> Element Msg
armorBlock label basestat lootstat =
    Element.row [ Element.spacing 11, Element.width Element.fill ] <|
        [ Element.column [] <| List.map (\l -> Element.el [] <| Element.text l) <| String.split " " label
        , Element.el
            [ Border.solid
            , Border.color <| Element.rgb255 0 0 0
            , Border.widthEach { bottom = 1, left = 1, right = 1, top = 1 }
            , Border.roundEach { bottomLeft = 25, topLeft = 0, bottomRight = 25, topRight = 0 }
            , Element.paddingXY 15 10
            , Font.center
            , Element.alignRight
            ]
          <|
            Element.text (String.fromInt basestat)
        , Element.column [ Element.centerX, Font.size 10, Element.alignRight ]
            [ Element.text "Base"
            , Element.text <| String.fromInt basestat
            , Element.text "Loot"
            , Element.text <| String.fromInt lootstat
            ]
        ]
