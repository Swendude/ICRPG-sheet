module Main exposing (..)

import Browser
import Element exposing (Attr, Attribute, Element, centerX, centerY, column, el, fill, fillPortion, height, inFront, minimum, padding, paddingXY, px, rgb, row, scrollbarX, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe
import String exposing (fromInt, toInt)
import Svg
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- ANCHOR Model


type alias Model =
    { character : Character
    , settings : AppSettings
    }


type alias Character =
    { name : CharacterTextProp
    , bioform : CharacterTextProp
    , class : CharacterTextProp
    , story : CharacterTextProp
    , hitpoints : CharacterNumberProp
    , equipped : List Item
    , carried : List Item
    , stats : Stats
    , coin : CharacterNumberProp
    , deathtimer : CharacterNumberProp
    }


type alias AppSettings =
    { editableText : Maybe TextAttribute
    , editableNumber : Maybe NumberAttribute
    , editingStats : Bool
    }


type alias CharacterTextProp =
    { value : String
    , id : TextAttribute
    }


type alias CharacterNumberProp =
    { value : Int
    , id : NumberAttribute
    , editvalue : Int
    }


type TextAttribute
    = Name
    | Story
    | Class
    | Bioform


type NumberAttribute
    = Coin
    | Hitpoints
    | Deathtimer


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
    | Hearts


type alias Stats =
    { str : Int
    , dex : Int
    , con : Int
    , wis : Int
    , int : Int
    , cha : Int
    , basic : Int
    , weapon : Int
    , magic : Int
    , ultimate : Int
    , armor : Int
    , hearts : Int
    }


type alias Item =
    { name : String
    , description : String
    , stats : Stats
    }



-- ANCHOR init


init : Model
init =
    { character = tabula_rasa
    , settings =
        { editableText = Nothing
        , editableNumber = Nothing
        , editingStats = False
        }
    }


tabula_rasa : Character
tabula_rasa =
    { name =
        { value = "Thuldir"
        , id = Name
        }
    , bioform =
        { value = "Dwarf"
        , id = Bioform
        }
    , class =
        { value = "Knight"
        , id = Class
        }
    , story =
        { value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
        , id = Story
        }
    , hitpoints =
        { value = 10
        , id = Hitpoints
        , editvalue = 0
        }
    , equipped = [ Item "Heartstone" "Adds 1 heart" (Stats 0 0 0 0 0 0 0 0 0 0 0 1), Item "Sword" "Makes you strong!" (Stats 1 0 0 0 0 0 0 0 0 0 0 1) ]
    , carried = []
    , stats = Stats 0 0 0 0 0 0 0 0 0 0 0 1
    , coin =
        { value = 0
        , id = Coin
        , editvalue = 0
        }
    , deathtimer =
        { value = 0
        , id = Deathtimer
        , editvalue = 0
        }
    }


type Msg
    = Increment
    | Decrement
    | MakeTextEditable TextAttribute
    | UpdateTextAttr TextAttribute String
    | DisableTextEditing TextAttribute
    | MakeNumberEditable NumberAttribute
    | DisableNumberEditing
    | IncreaseNumberAttribute NumberAttribute
    | DecreaseNumberAttribute NumberAttribute
    | UpdateEditField NumberAttribute String
    | EditBaseStats
    | ChangeStat Stat String



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

        MakeTextEditable id ->
            Just id
                |> asEditableTextIn model.settings
                |> asSettingsIn model

        DisableTextEditing id ->
            let
                allowEmpty =
                    Nothing
                        |> asEditableTextIn model.settings
                        |> asSettingsIn model

                checkEmpty _ =
                    if model.character.name.value == "" then
                        model

                    else
                        allowEmpty
            in
            case id of
                Name ->
                    checkEmpty model.character.name.value

                Class ->
                    checkEmpty model.character.class.value

                Bioform ->
                    checkEmpty model.character.bioform.value

                Story ->
                    allowEmpty

        UpdateTextAttr attr value ->
            asCharIn model <|
                case attr of
                    Name ->
                        value
                            |> asTextValueIn model.character.name
                            |> asNameIn model.character

                    Class ->
                        value
                            |> asTextValueIn model.character.class
                            |> asClassIn model.character

                    Bioform ->
                        value
                            |> asTextValueIn model.character.bioform
                            |> asBioformIn model.character

                    Story ->
                        value
                            |> asTextValueIn model.character.story
                            |> asStoryIn model.character

        MakeNumberEditable id ->
            Just id
                |> asEditableNumberIn model.settings
                |> asSettingsIn model

        DisableNumberEditing ->
            Nothing
                |> asEditableNumberIn model.settings
                |> asSettingsIn model

        IncreaseNumberAttribute id ->
            case id of
                Coin ->
                    (model.character.coin.value + model.character.coin.editvalue)
                        |> asNumberValueIn model.character.coin
                        |> asCoinIn model.character
                        |> asCharIn model

                Hitpoints ->
                    let
                        result =
                            model.character.hitpoints.value + model.character.hitpoints.editvalue

                        maxResult =
                            if result > (model.character.stats.hearts + .hearts (totalStats model.character.equipped) * 10) then
                                (model.character.stats.hearts + .hearts (totalStats model.character.equipped)) * 10

                            else
                                result
                    in
                    maxResult
                        |> asNumberValueIn model.character.hitpoints
                        |> asHitpointsIn model.character
                        |> asCharIn model

                Deathtimer ->
                    let
                        result =
                            model.character.deathtimer.value + model.character.deathtimer.editvalue

                        maxResult =
                            if result > 6 then
                                6

                            else
                                result
                    in
                    maxResult
                        |> asNumberValueIn model.character.deathtimer
                        |> asDeathtimerIn model.character
                        |> asCharIn model

        DecreaseNumberAttribute id ->
            case id of
                Coin ->
                    (model.character.coin.value - model.character.coin.editvalue)
                        |> asNumberValueIn model.character.coin
                        |> asCoinIn model.character
                        |> asCharIn model

                Hitpoints ->
                    if model.character.hitpoints.value <= 0 then
                        model

                    else
                        (model.character.hitpoints.value - model.character.hitpoints.editvalue)
                            |> asNumberValueIn model.character.hitpoints
                            |> asHitpointsIn model.character
                            |> asCharIn model

                Deathtimer ->
                    if model.character.deathtimer.value <= 0 then
                        model

                    else
                        (model.character.deathtimer.value - model.character.deathtimer.editvalue)
                            |> asNumberValueIn model.character.deathtimer
                            |> asDeathtimerIn model.character
                            |> asCharIn model

        UpdateEditField id value ->
            case id of
                Coin ->
                    Maybe.withDefault 0 (toInt value)
                        |> asEditValueIn model.character.coin
                        |> asCoinIn model.character
                        |> asCharIn model

                Hitpoints ->
                    Maybe.withDefault 0 (toInt value)
                        |> asEditValueIn model.character.hitpoints
                        |> asHitpointsIn model.character
                        |> asCharIn model

                Deathtimer ->
                    Maybe.withDefault 0 (toInt value)
                        |> asEditValueIn model.character.deathtimer
                        |> asDeathtimerIn model.character
                        |> asCharIn model

        EditBaseStats ->
            not model.settings.editingStats
                |> asEditingStatsIn model.settings
                |> asSettingsIn model

        ChangeStat stat value ->
            case toInt value of
                Just intVal ->
                    updateStat model stat intVal

                Nothing ->
                    updateStat model stat 0


updateStat : Model -> Stat -> Int -> Model
updateStat model stat value =
    let
        stats =
            model.character.stats

        statsToModel newStat =
            newStat |> asStatsIn model.character |> asCharIn model
    in
    case stat of
        Str ->
            { stats | str = value } |> statsToModel

        Dex ->
            { stats | dex = value } |> statsToModel

        Con ->
            { stats | con = value } |> statsToModel

        Wis ->
            { stats | wis = value } |> statsToModel

        Int ->
            { stats | int = value } |> statsToModel

        Cha ->
            { stats | cha = value } |> statsToModel

        Armor ->
            { stats | armor = value } |> statsToModel

        Basic ->
            { stats | basic = value } |> statsToModel

        Weapon ->
            { stats | weapon = value } |> statsToModel

        Magic ->
            { stats | magic = value } |> statsToModel

        Ultimate ->
            { stats | ultimate = value } |> statsToModel

        Hearts ->
            { stats | hearts = value } |> statsToModel


printNumberAttribute : NumberAttribute -> String
printNumberAttribute attr =
    case attr of
        Coin ->
            "Coin"

        Hitpoints ->
            "Hitpoints"

        Deathtimer ->
            "Deathtimer"


printTextAttribute : TextAttribute -> String
printTextAttribute attr =
    case attr of
        Name ->
            "Name"

        Class ->
            "Class"

        Story ->
            "Story"

        Bioform ->
            "Bioform"



-- ANCHOR View


view : Model -> Html Msg
view model =
    let
        editStatsModalOverlay =
            inFront <|
                el
                    [ paddingXY 70 0
                    , width fill
                    , centerY
                    , height fill
                    , Background.color (Element.rgba 0 0 0 0.5)
                    ]
                <|
                    column
                        [ paddingXY 70 30
                        , width fill
                        , spacingXY 0 10
                        , centerY
                        , Background.color (rgb 0 0 0)
                        , Font.color (rgb 255 255 255)
                        ]
                        [ row [ spacingXY 10 0 ]
                            [ statEditor Basic model.character.stats.basic "Basic"
                            , statEditor Weapon model.character.stats.weapon "Weapon"
                            , statEditor Magic model.character.stats.magic "Magic"
                            , statEditor Armor model.character.stats.armor "Armor"
                            ]
                        , row [ spacingXY 10 0 ]
                            [ statEditor Str model.character.stats.str "Str"
                            , statEditor Dex model.character.stats.dex "Dex"
                            , statEditor Con model.character.stats.con "Con"
                            , statEditor Int model.character.stats.int "Int"
                            , statEditor Wis model.character.stats.wis "Wis"
                            , statEditor Cha model.character.stats.cha "Cha"
                            ]
                        , Input.button [ centerX ]
                            { onPress = Just EditBaseStats
                            , label = el [ padding 5, Border.width 1, Border.color (rgb 255 255 255) ] <| text "Save"
                            }
                        ]

        activeOverlay =
            if model.settings.editingStats then
                [ editStatsModalOverlay ]

            else
                []
    in
    Element.layout
        ([ width fill
         , height fill
         , Font.family
            [ Font.typeface "Patrick Hand"
            ]
         , Font.size (scaled 1)
         , Background.color <| Element.rgb255 0 0 0
         ]
            ++ activeOverlay
        )
    <|
        Element.column
            [ width <| px 1200
            , centerX
            , Element.height Element.fill
            , Background.color <| Element.rgb255 255 255 255
            , paddingXY 50 23
            , spacingXY 0 23
            ]
            [ infoRow model
            , storyRow model
            , heartRow model
            , effortRow model.character
            , row [ width fill, spacingXY 10 0 ]
                [ el
                    [ width (fillPortion 1)
                    , Background.color <| Element.rgb255 244 244 244
                    , paddingXY 10 10
                    ]
                  <|
                    armorBlock "Armor" model.character.stats.armor <|
                        .armor (totalStats model.character.equipped)
                , column [ width (fillPortion 7), spacingXY 0 10 ]
                    [ statRow1 model.character
                    , statRow2 model.character
                    ]
                ]

            -- , Element.row [ Element.spacingXY (scaled 1) 0, width fill, Element.height Element.fill ]
            --     [ equippedCol model.character
            --     , unEquippedCol model.character
            --     ]
            ]


statEditor : Stat -> Int -> String -> Element Msg
statEditor stat value label =
    el [ width fill ] <|
        Input.text [ Font.color (rgb 0 0 0) ]
            { onChange = ChangeStat stat
            , text = fromInt value
            , placeholder = Just <| Input.placeholder [ Font.color (rgb 244 244 244) ] <| text <| "0"
            , label = Input.labelAbove [ centerX ] <| text label
            }


debugbg : Attr decorative msg
debugbg =
    Background.color <| Element.rgb255 0 255 255


infoRow : Model -> Element Msg
infoRow model =
    let
        labelStyle =
            []

        groupStyle =
            [ spacingXY 5 0, centerX ]

        fieldStyle =
            [ height (px <| 36), width <| px 150 ]
    in
    row [ width fill, spacingXY 10 0, Background.color <| Element.rgb255 244 244 244, padding 10 ]
        [ row groupStyle
            [ el labelStyle (text "Name :")
            , editableTextField fieldStyle model.settings.editableText model.character.name
            ]
        , row groupStyle
            [ el labelStyle (text "Class :")
            , editableTextField fieldStyle model.settings.editableText model.character.class
            ]
        , row groupStyle
            [ el labelStyle (text "Bioform :")
            , editableTextField fieldStyle model.settings.editableText model.character.bioform
            ]
        ]


storyRow : Model -> Element Msg
storyRow model =
    let
        labelStyle =
            []

        fieldStyle =
            [ Font.size (scaled -3), height (px <| 36), width (px 600) ]
    in
    row [ width fill, Background.color <| Element.rgb255 244 244 244, padding 10 ]
        [ row [ spacingXY 10 0, centerX ]
            [ el labelStyle (text "Story :")
            , editableTextField fieldStyle model.settings.editableText model.character.story
            ]
        ]


editableTextField : List (Attribute Msg) -> Maybe TextAttribute -> CharacterTextProp -> Element Msg
editableTextField style editable prop =
    let
        labelEl =
            el
                [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
                , Border.dotted
                , paddingXY 5 5
                ]
            <|
                case prop.value of
                    "" ->
                        text "Start typing"

                    _ ->
                        text prop.value

        readField =
            row style
                [ Input.button [ scrollbarX, width fill ]
                    { label = labelEl
                    , onPress = Just <| MakeTextEditable prop.id
                    }
                ]

        writeField =
            row style
                [ Input.text
                    [ Events.onLoseFocus <| DisableTextEditing prop.id
                    , paddingXY 5 5
                    , width fill
                    ]
                    { onChange = UpdateTextAttr prop.id
                    , text = prop.value
                    , placeholder = Just <| Input.placeholder [] <| text "Start typing"
                    , label = Input.labelHidden (printTextAttribute prop.id)
                    }
                ]
    in
    case editable of
        Just n ->
            if n == prop.id then
                writeField

            else
                readField

        Nothing ->
            readField


editableNumberField : List (Attribute Msg) -> Maybe NumberAttribute -> CharacterNumberProp -> Element Msg
editableNumberField style editable prop =
    let
        labelEl =
            el
                [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
                , Border.dotted
                ]
            <|
                text <|
                    fromInt prop.value

        readField =
            row
                style
                [ Input.button [] <|
                    { label = labelEl
                    , onPress = Just <| MakeNumberEditable prop.id
                    }
                ]

        writeField =
            row
                style
                [ Input.button [] <|
                    { label =
                        labelEl
                    , onPress =
                        Just <|
                            DisableNumberEditing
                    }
                , Input.text
                    [ paddingXY 5 0
                    , width (px 40)
                    ]
                  <|
                    { label = Input.labelHidden (printNumberAttribute prop.id)
                    , text = String.fromInt prop.editvalue
                    , onChange = UpdateEditField prop.id
                    , placeholder = Nothing
                    }
                , Input.button [ Font.size (scaled -1), paddingXY 0 0 ] <|
                    { label = text <| "+", onPress = Just <| IncreaseNumberAttribute prop.id }
                , Input.button [ Font.size (scaled -1), paddingXY 0 0 ] <|
                    { label = text <| "-", onPress = Just <| DecreaseNumberAttribute prop.id }
                ]
    in
    case editable of
        Just n ->
            if n == prop.id then
                writeField

            else
                readField

        Nothing ->
            readField


heartRow : Model -> Element Msg
heartRow model =
    let
        heartCount : Int
        heartCount =
            model.character.hitpoints.value // 10

        remainingHearts : Int
        remainingHearts =
            let
                totalHearts =
                    model.character.stats.hearts + .hearts (totalStats model.character.equipped)
            in
            if heartCount >= totalHearts then
                0

            else
                totalHearts - heartCount

        fieldStyle =
            [ spacingXY 10 0
            , height <| px 40
            , centerX
            ]
    in
    column [ width fill ]
        [ row [ width fill, spacingXY 10 0 ]
            [ column [ width <| fill, height <| px 65, centerX, Background.color <| Element.rgb255 244 244 244 ]
                [ row [ centerX ]
                    [ text <| "Hit Points: "
                    , editableNumberField fieldStyle model.settings.editableNumber model.character.hitpoints
                    ]
                , row [ centerX, scrollbarX, height <| px 65 ] <|
                    List.repeat heartCount filledHearts
                        ++ List.repeat remainingHearts emptyHearts
                ]
            , row [ width <| fill, height fill, centerX, Background.color <| Element.rgb255 244 244 244 ]
                [ el [ centerX ] <| text <| "Coin: "
                , editableNumberField fieldStyle model.settings.editableNumber model.character.coin
                ]
            , row [ width <| fill, height fill, centerX, Background.color <| Element.rgb255 244 244 244 ]
                [ el [ centerX ] <| text <| "† Dying?: "
                , editableNumberField fieldStyle model.settings.editableNumber model.character.deathtimer
                ]
            ]
        ]


filledHearts : Element Msg
filledHearts =
    el []
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 100 100"
                , Svg.Attributes.width "20px"
                ]
                [ Svg.g
                    [ Svg.Attributes.strokeWidth "5", Svg.Attributes.fill "red", Svg.Attributes.stroke "black" ]
                    [ Svg.path [ Svg.Attributes.d "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 z" ] []
                    ]
                ]
        )


emptyHearts : Element Msg
emptyHearts =
    el []
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 100 100"
                , Svg.Attributes.width "18px"
                ]
                [ Svg.g
                    [ Svg.Attributes.strokeWidth "5", Svg.Attributes.fill "white", Svg.Attributes.stroke "black" ]
                    [ Svg.path [ Svg.Attributes.d "M 10,30 A 20,20 0,0,1 50,30 A 20,20 0,0,1 90,30 Q 90,60 50,90 Q 10,60 10,30 z" ] []
                    ]
                ]
        )


blockRowLabelStyle : List (Attribute msg)
blockRowLabelStyle =
    [ centerX ]


blockRowBlockStyle : List (Attribute msg)
blockRowBlockStyle =
    [ centerX ]


blockRowStyle : List (Attribute msg)
blockRowStyle =
    [ spacingXY 10 0, Background.color <| Element.rgb255 244 244 244, padding 5, width (fill |> minimum 150) ]


statRow1 : Character -> Element Msg
statRow1 char =
    row
        [ width fill
        , spacingXY 10 0
        ]
        [ row blockRowStyle
            [ el blockRowLabelStyle <| text "Str"
            , el blockRowBlockStyle <| statBlock char.stats.str <| .str (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Dex"
            , el blockRowBlockStyle <| statBlock char.stats.dex <| .dex (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Con"
            , el blockRowBlockStyle <| statBlock char.stats.con <| .con (totalStats char.equipped)
            ]
        ]


statRow2 : Character -> Element Msg
statRow2 char =
    row
        [ width fill
        , spacingXY 10 0
        ]
        [ row blockRowStyle
            [ el blockRowLabelStyle <| text "Int"
            , el blockRowBlockStyle <| statBlock char.stats.int <| .int (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Wis"
            , el blockRowBlockStyle <| statBlock char.stats.wis <| .wis (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Cha"
            , el blockRowBlockStyle <| statBlock char.stats.cha <| .cha (totalStats char.equipped)
            ]
        ]


effortRow : Character -> Element Msg
effortRow char =
    row
        [ width fill
        , spacingXY 10 0
        ]
        [ row blockRowStyle
            [ el blockRowLabelStyle <| text "Basic (D4)"
            , el blockRowBlockStyle <| statBlock char.stats.basic <| .basic (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Weapon (D6)"
            , el blockRowBlockStyle <| statBlock char.stats.weapon <| .weapon (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Magic (D8)"
            , el blockRowBlockStyle <| statBlock char.stats.magic <| .magic (totalStats char.equipped)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Ultimate (D12)"
            , el blockRowBlockStyle <| statBlock char.stats.ultimate <| .ultimate (totalStats char.equipped)
            ]
        , row [ Background.color <| Element.rgb255 244 244 244, padding 5, width (px 52) ]
            [ el [ centerX, centerY ] <|
                Input.button []
                    { label = gear
                    , onPress = Just EditBaseStats
                    }
            ]
        ]


gear : Element Msg
gear =
    el []
        (Element.html <|
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 40 40"
                , Svg.Attributes.width "40px"
                , Svg.Attributes.height "40px"
                ]
                [ Svg.g
                    [ Svg.Attributes.strokeWidth "1", Svg.Attributes.fill "black", Svg.Attributes.stroke "black" ]
                    [ Svg.path [ Svg.Attributes.d "m3.83 16.135c-1.26.13-2.23 1.19-2.24 2.46l-.03 2.53c-.02 1.27.92 2.35 2.18 2.51l1.86.24c.31 1.13.75 2.21 1.32 3.23l-1.2 1.48c-.8.99-.74 2.41.15 3.32l1.77 1.81c.89.91 2.31 1.01 3.32.23l1.49-1.15c1.05.62 2.16 1.11 3.32 1.45l.2 1.92c.13 1.26 1.19 2.23 2.46 2.24l2.53.03c1.27.02 2.35-.92 2.51-2.18l.23-1.82c1.26-.31 2.46-.78 3.6-1.4l1.4 1.13c.99.8 2.41.74 3.32-.15l1.81-1.77c.91-.89 1.01-2.31.23-3.32l-1.07-1.39c.66-1.1 1.17-2.27 1.52-3.5l1.66-.17c1.26-.13 2.23-1.19 2.24-2.46l.03-2.53c.02-1.27-.92-2.35-2.18-2.51l-1.62-.21c-.31-1.22-.77-2.4-1.37-3.5l1.01-1.24c.8-.99.74-2.41-.15-3.32l-1.77-1.81c-.89-.91-2.31-1.01-3.32-.23l-1.21.93c-1.14-.69-2.36-1.22-3.64-1.58l-.16-1.57c-.13-1.26-1.19-2.23-2.46-2.24l-2.53-.03c-1.27-.02-2.35.92-2.51 2.18l-.2 1.56c-1.32.34-2.59.86-3.77 1.54l-1.25-1.02c-.99-.8-2.41-.74-3.32.15l-1.82 1.78c-.91.89-1.01 2.31-.23 3.32l1.07 1.38c-.62 1.1-1.11 2.27-1.43 3.5l-1.75.18zm16.33-2.86c3.63.04 6.54 3.03 6.5 6.66-.04 3.63-3.03 6.54-6.66 6.5-3.63-.04-6.54-3.03-6.5-6.66.04-3.63 3.03-6.54 6.66-6.5z" ] []
                    ]
                ]
        )


equippedCol : Character -> Element Msg
equippedCol _ =
    Element.column
        [ width fill
        , Element.alignTop
        ]
    <|
        (el [ Font.size (scaled 2), Element.alignTop ] <|
            text "Equipped Gear :"
        )
            :: List.repeat 1
                (el
                    [ Element.paddingEach { bottom = 8, left = scaled 1, right = 0, top = 8 }
                    , Font.size (scaled 1)
                    , Element.alignTop
                    ]
                 <|
                    text "None yet!"
                )


unEquippedCol : Character -> Element Msg
unEquippedCol char =
    Element.column
        [ width fill
        , Element.alignTop
        ]
    <|
        (el [ Font.size (scaled 2), Element.alignTop ] <|
            text "Carried Gear :"
        )
            :: List.repeat
                1
                (el
                    [ Element.paddingEach { bottom = 8, left = scaled 1, right = 0, top = 8 }
                    , Font.size (scaled 1)
                    , Element.alignTop
                    ]
                 <|
                    text char.name.value
                )


scaled : Int -> Int
scaled f =
    Basics.round <| Element.modular 24 1.2 f


blockStyle : List (Element.Attribute Msg)
blockStyle =
    [ Border.solid
    , Border.color <| Element.rgb255 0 0 0
    , Border.widthEach { bottom = 3, left = 1, right = 1, top = 1 }
    , Border.rounded 5
    , Background.color <| Element.rgb255 255 255 255
    , Element.paddingXY 10 5
    , Font.center
    , Element.alignRight
    ]


statBlock : Int -> Int -> Element Msg
statBlock basestat lootstat =
    Element.row [ Element.spacing 5 ] <|
        [ el blockStyle <|
            text (String.fromInt basestat)
        , Element.column [ Font.size (scaled -3), Element.alignRight ]
            [ text ("Base\t" ++ String.fromInt basestat)
            , text ("Loot\t" ++ String.fromInt lootstat)
            ]
        ]


armorBlock : String -> Int -> Int -> Element Msg
armorBlock label basestat lootstat =
    Element.column [ Element.spacing 5, Element.centerX ] <|
        [ el
            [ Element.paddingXY 5 5
            , Element.centerX
            , Border.widthEach { bottom = 3, left = 1, right = 1, top = 1 }
            , Border.roundEach
                { bottomLeft = 25
                , topLeft = 3
                , bottomRight = 25
                , topRight = 3
                }
            , Background.color <| Element.rgb255 255 255 255
            ]
          <|
            text (String.fromInt (lootstat + basestat + 10))
        , el [ Element.centerX ] <| text label
        , Element.row [ Element.centerX, Font.size (scaled -3) ]
            [ text "Base "
            , text <| String.fromInt basestat
            , text " "
            , text "Loot "
            , text <| String.fromInt lootstat
            ]
        ]


totalStats : List Item -> Stats
totalStats items =
    List.foldr sumStats (Stats 0 0 0 0 0 0 0 0 0 0 0 0) <|
        List.map .stats items


sumStats : Stats -> Stats -> Stats
sumStats s1 s2 =
    { str = s1.str + s2.str
    , dex = s1.dex + s2.dex
    , con = s1.con + s2.con
    , wis = s1.wis + s2.wis
    , int = s1.int + s2.int
    , cha = s1.cha + s2.cha
    , basic = s1.basic + s2.basic
    , weapon = s1.weapon + s2.weapon
    , magic = s1.magic + s2.magic
    , ultimate = s1.ultimate + s2.ultimate
    , armor = s1.armor + s2.armor
    , hearts = s1.hearts + s2.hearts
    }



-- ANCHOR: Modifiers


asEditingStatsIn : AppSettings -> Bool -> AppSettings
asEditingStatsIn settings newvalue =
    { settings | editingStats = newvalue }


asDeathtimerIn : Character -> CharacterNumberProp -> Character
asDeathtimerIn char newvalue =
    { char | deathtimer = newvalue }


asEditValueIn : CharacterNumberProp -> Int -> CharacterNumberProp
asEditValueIn charp newvalue =
    { charp | editvalue = newvalue }


asStrIn : Stats -> Int -> Stats
asStrIn stats change =
    { stats | str = change }


asStatsIn : Character -> Stats -> Character
asStatsIn char stats =
    { char | stats = stats }


asCharIn : Model -> Character -> Model
asCharIn model char =
    { model | character = char }


asHitpointsIn : Character -> CharacterNumberProp -> Character
asHitpointsIn char newhitpoints =
    { char | hitpoints = newhitpoints }


asCoinIn : Character -> CharacterNumberProp -> Character
asCoinIn char newcoin =
    { char | coin = newcoin }


asNameIn : Character -> CharacterTextProp -> Character
asNameIn char newname =
    { char | name = newname }


asBioformIn : Character -> CharacterTextProp -> Character
asBioformIn char newbioform =
    { char | bioform = newbioform }


asStoryIn : Character -> CharacterTextProp -> Character
asStoryIn char newstory =
    { char | story = newstory }


asClassIn : Character -> CharacterTextProp -> Character
asClassIn char newclass =
    { char | class = newclass }


asTextValueIn : CharacterTextProp -> String -> CharacterTextProp
asTextValueIn charp newvalue =
    { charp | value = newvalue }


asNumberValueIn : CharacterNumberProp -> Int -> CharacterNumberProp
asNumberValueIn charp newvalue =
    { charp | value = newvalue }


asEditableTextIn : AppSettings -> Maybe TextAttribute -> AppSettings
asEditableTextIn setting editable =
    { setting | editableText = editable }


asEditableNumberIn : AppSettings -> Maybe NumberAttribute -> AppSettings
asEditableNumberIn setting editable =
    { setting | editableNumber = editable }


asSettingsIn : Model -> AppSettings -> Model
asSettingsIn model settings =
    { model | settings = settings }
