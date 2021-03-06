module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html, button, div, span, text, th)
import Html.Events exposing (onClick)
import Maybe
import String exposing (fromInt)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { character : Character
    , settings : AppSettings
    }


tabula_rasa : Character
tabula_rasa =
    { name =
        { value = "Thuldir"
        , id = "name"
        }
    , bioform =
        { value = "Dwarf"
        , id = "bioform"
        }
    , class =
        { value = "Knight"
        , id = "class"
        }
    , story =
        { value = "Lord Commander of the Red Knight"
        , id = "story"
        }
    , hitpoints = 10
    , equipped = []
    , carried = []
    , stats = Stats 0 0 0 0 0 0 0 0 0 0 0
    , deadCount = Nothing
    , coin = 0
    }


type alias AppSettings =
    { editable : Maybe String
    }


init : Model
init =
    { character = tabula_rasa
    , settings =
        { editable = Nothing
        }
    }



-- type Attribute
--     = Name
--     | Story
--     |


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


type alias CharacterProp =
    { value : String
    , id : String
    }


type alias Character =
    { name : CharacterProp
    , bioform : CharacterProp
    , class : CharacterProp
    , story : CharacterProp
    , hitpoints : Int
    , equipped : List Item
    , carried : List Item
    , stats : Stats
    , deadCount : Maybe Int
    , coin : Int
    }


type Msg
    = Increment
    | Decrement
    | MakeEditable String
    | UpdateName String
    | DisableEditing String



-- ANCHOR Update


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

        MakeEditable id ->
            -- let
            --     oldSettings =
            --         model.settings
            --     newSettings =
            --         { oldSettings | editable = id }
            -- in
            -- { model | settings = newSettings }
            Just id
                |> asEditableIn model.settings
                |> asSettingsIn model

        DisableEditing id ->
            let
                allowEmpty =
                    Nothing
                        |> asEditableIn model.settings
                        |> asSettingsIn model

                checkEmpty value =
                    if model.character.name.value == "" then
                        model

                    else
                        allowEmpty
            in
            case id of
                "name" ->
                    checkEmpty model.character.name.value

                "class" ->
                    checkEmpty model.character.class.value

                "bioform" ->
                    checkEmpty model.character.bioform.value

                "story" ->
                    allowEmpty

                _ ->
                    model

        UpdateName name ->
            name
                |> asValueIn model.character.name
                |> asNameIn model.character
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


asNameIn : Character -> CharacterProp -> Character
asNameIn char newname =
    { char | name = newname }


asValueIn : CharacterProp -> String -> CharacterProp
asValueIn charp newvalue =
    { charp | value = newvalue }


asEditableIn : AppSettings -> Maybe String -> AppSettings
asEditableIn setting editable =
    { setting | editable = editable }


asSettingsIn : Model -> AppSettings -> Model
asSettingsIn model settings =
    { model | settings = settings }


view : Model -> Html Msg
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
            [ infoRow model
            , storyRow model.character
            , heartRow model.character
            , Element.wrappedRow [ Element.spacingXY (scaled 8) 0, Element.width Element.fill ]
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


infoRow : Model -> Element Msg
infoRow model =
    let
        char =
            model.character

        settings =
            model.settings

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
        , editableStringField settings.editable char.name
        , Element.el labelStyle (Element.text "Class:")
        , Element.el valueStyle (Element.text <| char.class.value)
        , Element.el labelStyle (Element.text "Bioform:")
        , Element.el valueStyle (Element.text <| char.bioform.value)
        ]



--  ANCHOR editableStringField


editableStringField : Maybe String -> CharacterProp -> Element Msg
editableStringField editable prop =
    let
        style =
            [ Element.width Element.fill
            , Element.paddingXY 10 0
            ]

        write_style =
            [ Events.onLoseFocus <| DisableEditing prop.id
            ]

        readField =
            Input.button style <| { label = Element.text prop.value, onPress = Just <| MakeEditable prop.id }

        writeField =
            Input.text (style ++ write_style)
                { onChange = UpdateName
                , text = prop.value
                , placeholder = Just <| Input.placeholder [] <| Element.text prop.id
                , label = Input.labelHidden prop.id
                }
    in
    case editable of
        Just n ->
            if n == prop.id then
                writeField

            else
                readField

        Nothing ->
            readField


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
        , Element.el valueStyle (Element.text <| char.story.value)
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
            [ Font.size (scaled 2) ]
    in
    Element.column
        [ Element.spacingXY 5 10
        , Element.width Element.fill
        ]
    <|
        [ Element.row [ Font.size (scaled 2), Element.spaceEvenly, Element.width Element.fill ]
            [ Element.column []
                [ Element.text <| "Hit Points: " ++ String.fromInt char.hitpoints
                , Element.row [] <|
                    List.repeat heartCount filledHearts
                        ++ List.repeat remainingHearts emptyHearts
                ]
            , Element.column [] [ Element.text <| "Dying?: ", Element.el [ Font.size (scaled -1) ] <| Element.text <| Maybe.withDefault "Roll a D6" (Maybe.map String.fromInt char.deadCount) ]
            , Element.column [] [ Element.text <| "Coin: ", Element.text <| String.fromInt char.coin ]
            ]
        ]


filledHearts : Element Msg
filledHearts =
    Element.el [ Element.width Element.fill ] <| Element.text "♥"


emptyHearts : Element Msg
emptyHearts =
    Element.el [ Element.width Element.fill ] <| Element.text "♡"


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
        [ Element.el [ Font.size (scaled 2), Element.alignTop ] <| Element.text "Carried Gear:"
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
