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
        , Element.paddingXY (scaled 16) 0
        , Background.color <| Element.rgb255 205 205 205
        , Font.family
            [ Font.external
                { name = "Permanent Marker"
                , url = "https://fonts.googleapis.com/css?family=Permanent+Marker"
                }
            ]
        , Font.size (scaled 1)
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
            , Element.wrappedRow [ Element.spacingXY (scaled 3) 0, Element.width Element.fill ]
                [ Element.row [ Element.spacingXY (scaled 1) 0, Element.height Element.fill ]
                    [ statCol model.character
                    , effortCol model.character
                    ]
                , Element.row [ Element.spacingXY (scaled 1) 0, Element.width Element.fill, Element.height Element.fill ]
                    [ equippedCol model.character
                    , unEquippedCol model.character
                    ]
                ]
            ]


infoRow : Character -> Element Msg
infoRow char =
    let
        labelStyle =
            [ Font.size (scaled -1) ]

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
            [ Font.size (scaled -1) ]

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
    <|
        [ Element.el labelStyle (Element.text "Hit Points:") ]
            ++ List.repeat heartCount filledHearts
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
        , Element.spacingXY 32 32
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
        , Element.spacingXY 32 32
        ]
        [ armorBlock "Armor" char.stats.armor 0
        , effortBlock "Basic (D4)" char.stats.basic 0
        , effortBlock "Weapon (D6)" char.stats.weapon 0
        , effortBlock "Magic (D8)" char.stats.magic 0
        , effortBlock "Ultimate (D12)" char.stats.ultimate 0
        ]


equippedCol : Character -> Element Msg
equippedCol char =
    Element.column
        [ Element.width Element.fill
        , Element.alignTop
        ]
    <|
        [ Element.el [ Font.size (scaled 2), Element.alignTop ] <| Element.text "Equipped Gear:"
        ]
            ++ List.repeat 1
                (Element.el
                    [ Element.paddingEach { bottom = 8, left = scaled 1, right = 0, top = 8 }
                    , Font.size (scaled 1)
                    , Element.alignTop
                    ]
                 <|
                    Element.text "None yet!"
                )


unEquippedCol : Character -> Element Msg
unEquippedCol char =
    Element.column
        [ Element.width Element.fill
        , Element.alignTop
        ]
    <|
        [ Element.el [ Font.size (scaled 2), Element.alignRight, Element.alignTop ] <| Element.text "Carried Gear:"
        ]
            ++ List.repeat 1
                (Element.el
                    [ Element.paddingEach { bottom = 8, left = scaled 1, right = 0, top = 8 }
                    , Font.size (scaled 1)
                    , Element.alignTop
                    ]
                 <|
                    Element.text "None yet!"
                )


scaled f =
    Basics.round <| Element.modular 14 1.2 f


blockStyle : List (Element.Attribute Msg)
blockStyle =
    [ Border.solid
    , Border.color <| Element.rgb255 0 0 0
    , Border.widthEach { bottom = 3, left = 1, right = 1, top = 1 }
    , Border.rounded 5
    , Element.paddingXY 10 5
    , Font.center
    , Element.alignRight
    , Font.size (scaled 1)
    ]


statBlock : String -> Int -> Int -> Element Msg
statBlock label basestat lootstat =
    Element.row [ Element.spacing 5 ] <|
        [ Element.el [ Font.size (scaled 1) ] <| Element.text label
        , Element.el blockStyle <|
            Element.text (String.fromInt basestat)
        , Element.column [ Font.size (scaled -3), Element.alignRight ]
            [ Element.text "Base"
            , Element.text <| String.fromInt basestat
            , Element.text "Loot"
            , Element.text <| String.fromInt lootstat
            ]
        ]


effortBlock : String -> Int -> Int -> Element Msg
effortBlock label basestat lootstat =
    Element.row [ Element.spacing 5, Element.alignRight, Element.height Element.fill ] <|
        [ Element.column [] <| List.map (\l -> Element.el [ Font.size (scaled 1), Element.alignRight ] <| Element.text l) <| String.split " " label
        , Element.el
            blockStyle
          <|
            Element.text (String.fromInt basestat)
        , Element.column [ Font.size (scaled -3), Element.alignRight ]
            [ Element.text "Base"
            , Element.text <| String.fromInt basestat
            , Element.text "Loot"
            , Element.text <| String.fromInt lootstat
            ]
        ]


armorBlock : String -> Int -> Int -> Element Msg
armorBlock label basestat lootstat =
    Element.column [ Element.spacing 5, Element.centerX ] <|
        [ Element.el
            (blockStyle
                ++ [ Font.size (scaled 3), Element.paddingXY 15 10, Element.centerX, Border.roundEach { bottomLeft = 25, topLeft = 0, bottomRight = 25, topRight = 0 } ]
            )
          <|
            Element.text (String.fromInt (10 + basestat))
        , Element.column [] <| List.map (\l -> Element.el [ Font.size (scaled 2), Element.centerX ] <| Element.text l) <| String.split " " label
        , Element.row [ Element.centerX, Font.size (scaled -3), Element.alignRight ]
            [ Element.text "Base "
            , Element.text <| String.fromInt basestat
            , Element.text " "
            , Element.text "Loot "
            , Element.text <| String.fromInt lootstat
            ]
        ]
