module Main exposing (..)

-- import Html.Events exposing (onClick)

import Browser
import Element exposing (Attr, Attribute, Element, alignRight, centerX, centerY, clip, column, el, fill, fillPortion, height, maximum, minimum, padding, paddingXY, px, rgb255, row, scrollbarX, scrollbars, spaceEvenly, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border exposing (widthXY)
import Element.Events as Events
import Element.Font as Font exposing (center)
import Element.Input as Input
import Html exposing (Html, label)
import Maybe
import String exposing (fromInt, pad, toInt)
import Svg
import Svg.Attributes


main : Program () Model Msg
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
        { value = 30
        , id = Hitpoints
        , editvalue = 0
        }
    , equipped = []
    , carried = []
    , stats = Stats 0 0 0 0 0 0 0 0 0 0 10 1
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


type alias AppSettings =
    { editableText : Maybe TextAttribute
    , editableNumber : Maybe NumberAttribute
    }


init : Model
init =
    { character = tabula_rasa
    , settings =
        { editableText = Nothing
        , editableNumber = Nothing
        }
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


type Item
    = Item String Stats


type alias CharacterTextProp =
    { value : String
    , id : TextAttribute
    }


type alias CharacterNumberProp =
    { value : Int
    , id : NumberAttribute
    , editvalue : Int
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

                checkEmpty value =
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
                    (model.character.hitpoints.value + model.character.hitpoints.editvalue)
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


asDeathtimerIn : Character -> CharacterNumberProp -> Character
asDeathtimerIn char newvalue =
    { char | deathtimer = newvalue }


asEditValueIn : CharacterNumberProp -> Int -> CharacterNumberProp
asEditValueIn charp newvalue =
    { charp | editvalue = newvalue }


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



-- ANCHOR View


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
        , Font.family
            [ Font.typeface "Patrick Hand"
            ]
        , Font.size (scaled 1)
        , Background.color <| Element.rgb255 0 0 0
        ]
    <|
        Element.column
            [ width <| px 1200
            , centerX
            , Element.height Element.fill
            , Background.color <| Element.rgb255 255 255 255
            , paddingXY 50 0
            , spacingXY 0 23
            ]
            [ infoRow model
            , storyRow model
            , heartRow model
            , effortRow model.character
            , row [ width fill ]
                [ el [ width (fillPortion 1) ] <|
                    armorBlock "Armor" model.character.stats.armor 0
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
            if heartCount > 8 then
                0

            else
                8 - heartCount

        fieldStyle =
            [ spacingXY 10 0
            , height <| px 40
            ]
    in
    column [ width fill ]
        [ row []
            [ row [ width <| px 250 ]
                [ text <| "Hit Points: "
                , editableNumberField fieldStyle model.settings.editableNumber model.character.hitpoints
                ]
            , row [ width <| px 250 ]
                [ text <| "Coin: "
                , editableNumberField fieldStyle model.settings.editableNumber model.character.coin
                ]
            , row [ width <| px 250 ]
                [ text <| "† Dying?: "
                , editableNumberField fieldStyle model.settings.editableNumber model.character.deathtimer
                ]
            ]
        , row [] <|
            List.repeat heartCount filledHearts
                ++ List.repeat remainingHearts emptyHearts
        ]


filledHearts : Element Msg
filledHearts =
    el [ width fill ]
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
    el [ width fill ]
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
            , el blockRowBlockStyle <| statBlock char.stats.str 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Dex"
            , el blockRowBlockStyle <| statBlock char.stats.dex 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Con"
            , el blockRowBlockStyle <| statBlock char.stats.con 0
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
            , el blockRowBlockStyle <| statBlock char.stats.int 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Wis"
            , el blockRowBlockStyle <| statBlock char.stats.wis 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Cha"
            , el blockRowBlockStyle <| statBlock char.stats.cha 0
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
            , el blockRowBlockStyle <| statBlock char.stats.basic 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Weapon (D6)"
            , el blockRowBlockStyle <| statBlock char.stats.weapon 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Magic (D8)"
            , el blockRowBlockStyle <| statBlock char.stats.magic 0
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Ultimate (D12)"
            , el blockRowBlockStyle <| statBlock char.stats.ultimate 0
            ]
        ]


equippedCol : Character -> Element Msg
equippedCol char =
    Element.column
        [ width fill
        , Element.alignTop
        ]
    <|
        [ el [ Font.size (scaled 2), Element.alignTop ] <| text "Equipped Gear :"
        ]
            ++ List.repeat 1
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
        [ el [ Font.size (scaled 2), Element.alignTop ] <| text "Carried Gear :"
        ]
            ++ List.repeat 1
                (el
                    [ Element.paddingEach { bottom = 8, left = scaled 1, right = 0, top = 8 }
                    , Font.size (scaled 1)
                    , Element.alignTop
                    ]
                 <|
                    text "None yet!"
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
        , Element.column [ Font.size (scaled -6), Element.alignRight ]
            [ text "Base"
            , text <| String.fromInt basestat
            , text "Loot"
            , text <| String.fromInt lootstat
            ]
        ]


armorBlock : String -> Int -> Int -> Element Msg
armorBlock label basestat lootstat =
    Element.column [ Element.spacing 5, Element.centerX ] <|
        [ el
            (blockStyle
                ++ [ Font.size (scaled 3), Element.paddingXY 15 10, Element.centerX, Border.roundEach { bottomLeft = 25, topLeft = 0, bottomRight = 25, topRight = 0 } ]
            )
          <|
            text (String.fromInt (lootstat + basestat))
        , Element.column [] <| List.map (\l -> el [ Font.size (scaled 2), Element.centerX ] <| text l) <| String.split " " label
        , Element.row [ Element.centerX, Font.size (scaled -3), Element.alignRight ]
            [ text "Base "
            , text <| String.fromInt basestat
            , text " "
            , text "Loot "
            , text <| String.fromInt lootstat
            ]
        ]
