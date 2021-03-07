module Main exposing (..)

-- import Html.Events exposing (onClick)

import Browser
import Dict exposing (Dict)
import Element exposing (Attr, Element, alignRight, centerX, centerY, column, el, fill, padding, paddingXY, px, rgb255, row, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border exposing (widthXY)
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe
import String exposing (fromInt, toInt)


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
        { value = "Lord Commander of the Red Knight"
        , id = Story
        }
    , hitpoints =
        { value = 30
        , id = Hitpoints
        , editvalue = 0
        }
    , equipped = []
    , carried = []
    , stats = Stats 0 0 0 0 0 0 0 0 0 0 0
    , deadCount = Nothing
    , coin =
        { value = 0
        , id = Coin
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
    , deadCount : Maybe Int
    , coin : CharacterNumberProp
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



-- UpdateNumberAttr attr value ->
--     asCharIn model <|
--         case attr of
--             Coin ->
--                 value
--                     |> asNumberValueIn model.character.coin
--                     |> asCoinIn model.character
--             Hitpoints ->
--                 value
--                     |> asNumberValueIn model.character.hitpoints
--                     |> asHitpointsIn model.character


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


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
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
            , width fill
            , Element.height Element.fill
            , Background.color <| Element.rgb255 255 255 255
            ]
            [ infoRow model
            , storyRow model
            , heartRow model
            , Element.wrappedRow [ Element.spacingXY (scaled 8) 0, width fill ]
                [ Element.row [ Element.spacingXY (scaled 1) 0, Element.height Element.fill ]
                    [ statCol model.character
                    , effortCol model.character
                    ]
                , Element.row [ Element.spacingXY (scaled 1) 0, width fill, Element.height Element.fill ]
                    [ equippedCol model.character
                    , unEquippedCol model.character
                    ]
                ]
            ]


infoRow : Model -> Element Msg
infoRow model =
    let
        labelStyle =
            [ Font.size (scaled -1) ]

        valueStyle =
            [ width fill, Element.paddingXY 10 0 ]
    in
    Element.row
        [ width fill
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        ]
        [ el labelStyle (text "Name :")
        , editableTextField model.settings.editableText model.character.name
        , el labelStyle (text "Class :")
        , editableTextField model.settings.editableText model.character.class
        , el labelStyle (text "Bioform :")
        , editableTextField model.settings.editableText model.character.bioform
        ]


storyRow : Model -> Element Msg
storyRow model =
    let
        labelStyle =
            [ Font.size (scaled -1) ]
    in
    Element.row
        [ width fill
        , Border.solid
        , Border.color <| Element.rgb255 0 0 0
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Element.paddingEach { bottom = 10, left = 0, right = 0, top = 0 }
        ]
        [ el labelStyle (text "Story :")
        , editableTextField model.settings.editableText model.character.story
        ]


editableTextField : Maybe TextAttribute -> CharacterTextProp -> Element Msg
editableTextField editable prop =
    let
        style =
            [ width fill
            , Element.paddingXY 10 0
            ]

        write_style =
            [ Events.onLoseFocus <| DisableTextEditing prop.id
            ]

        readField =
            Input.button style <| { label = text prop.value, onPress = Just <| MakeTextEditable prop.id }

        writeField =
            Input.text (style ++ write_style)
                { onChange = UpdateTextAttr prop.id
                , text = prop.value
                , placeholder = Just <| Input.placeholder [] <| text (printTextAttribute prop.id)
                , label = Input.labelHidden (printTextAttribute prop.id)
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



-- ANCHOR NumberField


editableNumberField : Maybe NumberAttribute -> CharacterNumberProp -> Element Msg
editableNumberField editable prop =
    let
        readField =
            Input.button [ paddingXY 10 18, width fill ] <|
                { label = text <| fromInt prop.value, onPress = Just <| MakeNumberEditable prop.id }

        writeField =
            row
                [ paddingXY 10 10
                , spacingXY 10 0
                , width fill
                ]
                [ Input.button [] <|
                    { label =
                        text <|
                            fromInt prop.value
                    , onPress =
                        Just <|
                            DisableNumberEditing
                    }
                , Input.text
                    [ paddingXY 5 0
                    , widthXY 2 4

                    -- , spacingXY 5 5
                    , width (px <| scaled 8)
                    ]
                  <|
                    { label = Input.labelHidden (printNumberAttribute prop.id)
                    , text = String.fromInt prop.editvalue
                    , onChange = UpdateEditField prop.id
                    , placeholder = Just <| Input.placeholder [] <| text "Halloo"
                    }
                , Element.column
                    []
                    [ Input.button [] <| { label = text <| "+", onPress = Just <| IncreaseNumberAttribute prop.id }
                    , Input.button [] <| { label = text <| "-", onPress = Just <| DecreaseNumberAttribute prop.id }
                    ]
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
            10 - heartCount

        labelStyle =
            [ Font.size (scaled 2) ]
    in
    row [ Font.size (scaled 2), width fill ]
        [ row [ width fill, centerX ]
            [ text <| "Hit Points: "
            , editableNumberField model.settings.editableNumber model.character.hitpoints
            ]
        , row [ width fill, centerX ]
            [ text <| "Coin: "
            , text <| String.fromInt model.character.coin.value
            ]
        ]


filledHearts : Element Msg
filledHearts =
    el [ width fill ] <| text "♥"


emptyHearts : Element Msg
emptyHearts =
    el [ width fill ] <| text "♡"


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
        [ el [ Font.size (scaled 1) ] <| text label
        , el blockStyle <|
            text (String.fromInt basestat)
        , Element.column [ Font.size (scaled -3), Element.alignRight ]
            [ text "Base"
            , text <| String.fromInt basestat
            , text "Loot"
            , text <| String.fromInt lootstat
            ]
        ]


effortBlock : String -> Int -> Int -> Element Msg
effortBlock label basestat lootstat =
    Element.row [ Element.spacing 5, Element.alignRight, Element.height Element.fill ] <|
        [ Element.column [] <| List.map (\l -> el [ Font.size (scaled 1), Element.alignRight ] <| text l) <| String.split " " label
        , el
            blockStyle
          <|
            text (String.fromInt basestat)
        , Element.column [ Font.size (scaled -3), Element.alignRight ]
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
            text (String.fromInt (10 + basestat))
        , Element.column [] <| List.map (\l -> el [ Font.size (scaled 2), Element.centerX ] <| text l) <| String.split " " label
        , Element.row [ Element.centerX, Font.size (scaled -3), Element.alignRight ]
            [ text "Base "
            , text <| String.fromInt basestat
            , text " "
            , text "Loot "
            , text <| String.fromInt lootstat
            ]
        ]
