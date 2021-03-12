module Main exposing (..)

import Browser
import Element exposing (Attr, Attribute, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, clip, column, el, fill, fillPortion, height, image, inFront, minimum, newTabLink, none, padding, paddingEach, paddingXY, px, rgb, rgb255, row, scrollbarX, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List
import List.Extra
import Maybe
import String
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
    , items : List Item
    , stats : Stats
    , coin : CharacterNumberProp
    , deathtimer : CharacterNumberProp
    }


type EditingState
    = EditingStats
    | EditingItem Int
    | NotEditing



-- TODO: Everything should be an EditingState!


type alias AppSettings =
    { editableText : Maybe CharacterTextAttribute
    , editableNumber : Maybe CharacterNumberAttribute
    , editingState : EditingState
    }


type alias CharacterTextProp =
    { value : String
    , id : CharacterTextAttribute
    , hovered : Bool
    }


type alias CharacterNumberProp =
    { value : Int
    , id : CharacterNumberAttribute
    , editvalue : Int
    }


type CharacterTextAttribute
    = Name
    | Story
    | Class
    | Bioform


type CharacterNumberAttribute
    = Coin
    | Hitpoints
    | Deathtimer


type StatAttribute
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


type ItemAttribute
    = ItemName
    | Description


type alias Item =
    { name : String
    , description : String
    , stats : Stats
    , equipped : Bool
    }



-- ANCHOR init


init : Model
init =
    { character = tabula_rasa
    , settings =
        { editableText = Nothing
        , editableNumber = Nothing
        , editingState = NotEditing
        }
    }


tabula_rasa : Character
tabula_rasa =
    { name =
        { value = "Thuldir"
        , id = Name
        , hovered = False
        }
    , bioform =
        { value = "Dwarf"
        , id = Bioform
        , hovered = False
        }
    , class =
        { value = "Knight"
        , id = Class
        , hovered = False
        }
    , story =
        { value = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
        , id = Story
        , hovered = False
        }
    , hitpoints =
        { value = 10
        , id = Hitpoints
        , editvalue = 0
        }
    , items =
        [ Item "Heartstone" "Adds 1 heart" (Stats 0 0 0 0 0 0 0 0 0 0 0 1) True
        , Item "Sword" "Makes you strong!" (Stats 1 1 0 0 0 0 0 0 0 0 0 0) True
        , Item "Heal" "Wis Spell: Heal an ally" (Stats 0 0 0 0 0 0 0 0 0 0 0 1) False
        ]
    , stats = Stats 0 0 10 0 0 0 0 0 0 0 0 1
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


emptyStats : Stats
emptyStats =
    Stats 0 0 0 0 0 0 0 0 0 0 0 0


type Msg
    = Increment
    | Decrement
    | MakeTextEditable CharacterTextAttribute
    | UpdateTextAttr CharacterTextAttribute String
    | DisableTextEditing CharacterTextAttribute
    | MakeNumberEditable CharacterNumberAttribute
    | DisableNumberEditing
    | IncreaseNumberAttribute CharacterNumberAttribute
    | DecreaseNumberAttribute CharacterNumberAttribute
    | UpdateEditField CharacterNumberAttribute String
    | EditBaseStats
    | ChangeStatAttribute StatAttribute String
    | ChangeItemAttribute ItemAttribute String
    | ToggleItem Int
    | Hovered CharacterTextAttribute
    | Unhovered CharacterTextAttribute
    | EditItem Int
    | NewItem Bool
    | DisableEdit



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
                            if result > (model.character.stats.hearts + .hearts (totalEquippedStats model.character.items) * 10) then
                                (model.character.stats.hearts + .hearts (totalEquippedStats model.character.items)) * 10

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
                    Maybe.withDefault 0 (String.toInt value)
                        |> asEditValueIn model.character.coin
                        |> asCoinIn model.character
                        |> asCharIn model

                Hitpoints ->
                    Maybe.withDefault 0 (String.toInt value)
                        |> asEditValueIn model.character.hitpoints
                        |> asHitpointsIn model.character
                        |> asCharIn model

                Deathtimer ->
                    Maybe.withDefault 0 (String.toInt value)
                        |> asEditValueIn model.character.deathtimer
                        |> asDeathtimerIn model.character
                        |> asCharIn model

        EditBaseStats ->
            EditingStats
                |> asEditingStateIn model.settings
                |> asSettingsIn model

        DisableEdit ->
            NotEditing
                |> asEditingStateIn model.settings
                |> asSettingsIn model

        ChangeStatAttribute stat value ->
            case String.toInt value of
                Just intVal ->
                    case model.settings.editingState of
                        EditingStats ->
                            updateStat model stat intVal

                        EditingItem ix ->
                            updateItemStat model stat intVal ix

                        _ ->
                            model

                Nothing ->
                    -- updateStat model stat 0
                    model

        ChangeItemAttribute attr value ->
            case model.settings.editingState of
                EditingItem ix ->
                    updateItemAttribute model attr value ix

                _ ->
                    model

        ToggleItem ix ->
            case List.Extra.getAt ix model.character.items of
                Just item ->
                    let
                        totalItems =
                            List.length <| List.filter ((/=) item.equipped << .equipped) model.character.items

                        newItem =
                            { item | equipped = not item.equipped }

                        itemsRemoved =
                            List.Extra.removeAt ix model.character.items

                        newItems =
                            itemsRemoved ++ [ newItem ]
                    in
                    if totalItems < 10 then
                        newItems
                            |> asItemsIn model.character
                            |> asCharIn model

                    else
                        model

                Nothing ->
                    model

        NewItem equippedState ->
            let
                totalItems =
                    List.length <| List.filter ((==) equippedState << .equipped) model.character.items

                newItem =
                    Item "Edit me!" "" emptyStats equippedState
            in
            if totalItems < 10 then
                model.character.items
                    ++ [ newItem ]
                    |> asItemsIn model.character
                    |> asCharIn model

            else
                model

        Hovered attribute ->
            case attribute of
                Name ->
                    True
                        |> asHoveredIn model.character.name
                        |> asNameIn model.character
                        |> asCharIn model

                Class ->
                    True
                        |> asHoveredIn model.character.class
                        |> asClassIn model.character
                        |> asCharIn model

                Bioform ->
                    True
                        |> asHoveredIn model.character.bioform
                        |> asBioformIn model.character
                        |> asCharIn model

                Story ->
                    True
                        |> asHoveredIn model.character.story
                        |> asStoryIn model.character
                        |> asCharIn model

        Unhovered attribute ->
            case attribute of
                Name ->
                    False
                        |> asHoveredIn model.character.name
                        |> asNameIn model.character
                        |> asCharIn model

                Class ->
                    False
                        |> asHoveredIn model.character.class
                        |> asClassIn model.character
                        |> asCharIn model

                Bioform ->
                    False
                        |> asHoveredIn model.character.bioform
                        |> asBioformIn model.character
                        |> asCharIn model

                Story ->
                    False
                        |> asHoveredIn model.character.story
                        |> asStoryIn model.character
                        |> asCharIn model

        EditItem ix ->
            EditingItem ix
                |> asEditingStateIn model.settings
                |> asSettingsIn model


itemsIndexed : Bool -> List Item -> List ( Int, Item )
itemsIndexed b =
    List.filter ((==) b << .equipped << Tuple.second) << List.indexedMap Tuple.pair


updateStat : Model -> StatAttribute -> Int -> Model
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


updateItemAttribute : Model -> ItemAttribute -> String -> Int -> Model
updateItemAttribute model attr value ix =
    case List.Extra.getAt ix model.character.items of
        Just item ->
            let
                itemToItems ix_ newItem =
                    List.Extra.setAt ix_ newItem model.character.items
                        |> asItemsIn model.character
                        |> asCharIn model
            in
            case attr of
                ItemName ->
                    { item | name = value } |> itemToItems ix

                Description ->
                    { item | description = value } |> itemToItems ix

        Nothing ->
            model


updateItemStat : Model -> StatAttribute -> Int -> Int -> Model
updateItemStat model stat value ix =
    case List.Extra.getAt ix model.character.items of
        Just oldItem ->
            let
                stats =
                    oldItem.stats

                statsToItem ix_ newStat =
                    List.Extra.setAt ix_ { oldItem | stats = newStat } model.character.items
                        |> asItemsIn model.character
                        |> asCharIn model
            in
            case stat of
                Str ->
                    { stats | str = value } |> statsToItem ix

                Dex ->
                    { stats | dex = value } |> statsToItem ix

                Con ->
                    { stats | con = value } |> statsToItem ix

                Wis ->
                    { stats | wis = value } |> statsToItem ix

                Int ->
                    { stats | int = value } |> statsToItem ix

                Cha ->
                    { stats | cha = value } |> statsToItem ix

                Armor ->
                    { stats | armor = value } |> statsToItem ix

                Basic ->
                    { stats | basic = value } |> statsToItem ix

                Weapon ->
                    { stats | weapon = value } |> statsToItem ix

                Magic ->
                    { stats | magic = value } |> statsToItem ix

                Ultimate ->
                    { stats | ultimate = value } |> statsToItem ix

                Hearts ->
                    { stats | hearts = value } |> statsToItem ix

        Nothing ->
            model


printNumberAttribute : CharacterNumberAttribute -> String
printNumberAttribute attr =
    case attr of
        Coin ->
            "Coin"

        Hitpoints ->
            "Hitpoints"

        Deathtimer ->
            "Deathtimer"


printTextAttribute : CharacterTextAttribute -> String
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
        activeOverlay =
            case model.settings.editingState of
                EditingStats ->
                    [ editStatsModal model ]

                EditingItem ix ->
                    case List.Extra.getAt ix model.character.items of
                        Just item ->
                            [ editItemModal item ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    Element.layout
        [ width fill
        , Font.family
            [ Font.typeface "Patrick Hand"
            ]
        , Font.size (scaled 1)
        , Background.color <| Element.rgb255 0 0 0
        ]
    <|
        Element.column
            ([ width <| px 1280
             , centerX
             , Element.height Element.fill
             , Background.color <| Element.rgb255 255 255 255
             , paddingXY 50 23
             , spacingXY 0 23
             ]
                ++ activeOverlay
            )
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
                        .armor (totalEquippedStats model.character.items)
                , column [ width (fillPortion 7), spacingXY 0 10 ]
                    [ statRow1 model.character
                    , statRow2 model.character
                    ]
                ]
            , row
                [ width fill
                , spacingXY 10 0
                ]
                [ itemCol True "Equipped " "Carry" model.character.items
                , itemCol False "Carried " "Equip" model.character.items
                ]
            , spacerRow
            , row
                [ paddingEach { top = 20, right = 0, bottom = 10, left = 0 }
                , centerX
                , spacingXY 10 0
                , Font.size (scaled -1)
                ]
                [ row [ alignBottom ]
                    [ text "Made "
                    , newTabLink [ Font.color (rgb255 194 0 0), Font.underline ]
                        { url = "https://github.com/Swendude/ICRPG-sheet"
                        , label = el [] <| text "(open source) "
                        }
                    , text "by "
                    , newTabLink [ Font.color (rgb255 194 0 0), Font.underline ]
                        { url = "https://github.com/Swendude"
                        , label = el [] <| text "Swendude"
                        }
                    ]
                ]
            , image [ centerX, width (px 60), height (px 60) ] { src = "public/icrpg.png", description = "For use with ICRPG" }
            ]


editStatsModal : Model -> Attribute Msg
editStatsModal model =
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
                [ row [ spacingXY 10 0, centerX ]
                    [ el [ padding 5, Border.color (rgb 255 255 255), Font.size (scaled 2) ] <| text "Edit base stats"
                    ]
                , row [ spacingXY 10 0, centerX ]
                    [ el [ padding 5, Border.color (rgb 255 255 255), Font.size (scaled -1) ] <| text "These mostly come from your choice of Bioform and Class, but your GM might give you other reasons to add base stats!"
                    ]
                , row [ spacingXY 10 0, centerX ]
                    [ statEditor Basic model.character.stats.basic "Basic"
                    , statEditor Weapon model.character.stats.weapon "Weapon"
                    , statEditor Magic model.character.stats.magic "Magic"
                    , statEditor Armor model.character.stats.armor "Armor"
                    ]
                , row [ spacingXY 10 0, centerX ]
                    [ statEditor Str model.character.stats.str "Str"
                    , statEditor Dex model.character.stats.dex "Dex"
                    , statEditor Con model.character.stats.con "Con"
                    , statEditor Int model.character.stats.int "Int"
                    , statEditor Wis model.character.stats.wis "Wis"
                    , statEditor Cha model.character.stats.cha "Cha"
                    ]
                , Input.button [ centerX ]
                    { onPress = Just DisableEdit
                    , label = el [ padding 5, Border.width 1, Border.color (rgb 255 255 255) ] <| text "Close"
                    }
                ]


editItemModal : Item -> Attribute Msg
editItemModal item =
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
                [ row [ spacingXY 10 0, centerX ]
                    [ el [ padding 5, Border.color (rgb 255 255 255), Font.size (scaled 2) ] <| text "Edit item"
                    ]
                , row [ centerX ]
                    [ textEditor ItemName item.name "Name"
                    ]
                , row [ centerX, width fill ]
                    [ textEditor Description item.description "Description"
                    ]
                , row [ spacingXY 10 0, centerX ]
                    [ statEditor Hearts item.stats.hearts "Hearts"
                    , statEditor Basic item.stats.basic "Basic"
                    , statEditor Weapon item.stats.weapon "Weapon"
                    , statEditor Magic item.stats.magic "Magic"
                    , statEditor Armor item.stats.armor "Armor"
                    ]
                , row [ spacingXY 10 0, centerX ]
                    [ statEditor Str item.stats.str "Str"
                    , statEditor Dex item.stats.dex "Dex"
                    , statEditor Con item.stats.con "Con"
                    , statEditor Int item.stats.int "Int"
                    , statEditor Wis item.stats.wis "Wis"
                    , statEditor Cha item.stats.cha "Cha"
                    ]
                , Input.button [ centerX ]
                    { onPress = Just DisableEdit
                    , label = el [ padding 5, Border.width 1, Border.color (rgb 255 255 255) ] <| text "Close"
                    }
                ]


statEditor : StatAttribute -> Int -> String -> Element Msg
statEditor stat value label =
    el [ width fill ] <|
        Input.text [ Font.color (rgb 0 0 0) ]
            { onChange = ChangeStatAttribute stat
            , text = String.fromInt value
            , placeholder = Just <| Input.placeholder [ Font.color (rgb 244 244 244) ] <| text <| "0"
            , label = Input.labelAbove [ centerX ] <| text label
            }


textEditor : ItemAttribute -> String -> String -> Element Msg
textEditor attr value label =
    el [ width fill ] <|
        Input.text [ Font.color (rgb 0 0 0) ]
            { onChange = ChangeItemAttribute attr
            , text = value
            , placeholder = Just <| Input.placeholder [ Font.color (rgb 244 244 244) ] <| text <| ".."
            , label = Input.labelAbove [ centerX ] <| text label
            }


debugbg : Attr decorative msg
debugbg =
    Background.color <| Element.rgb255 0 255 255


spacerRow : Element Msg
spacerRow =
    el
        [ width fill
        , Border.widthEach
            { bottom = 2
            , left = 0
            , right = 0
            , top = 0
            }
        ]
    <|
        none


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


editableTextField : List (Attribute Msg) -> Maybe CharacterTextAttribute -> CharacterTextProp -> Element Msg
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

        buttonStyle =
            if prop.hovered then
                [ scrollbarX, width fill, Events.onMouseEnter (Hovered prop.id), Events.onMouseLeave (Unhovered prop.id) ]

            else
                [ clip, width fill, Events.onMouseEnter (Hovered prop.id), Events.onMouseLeave (Unhovered prop.id) ]

        readField =
            row style
                [ Input.button buttonStyle
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


editableNumberField : List (Attribute Msg) -> Maybe CharacterNumberAttribute -> CharacterNumberProp -> Element Msg
editableNumberField style editable prop =
    let
        labelEl =
            el
                [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
                , Border.dotted
                ]
            <|
                text <|
                    String.fromInt prop.value

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
                    model.character.stats.hearts + .hearts (totalEquippedStats model.character.items)
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
                    [ Svg.Attributes.strokeWidth "5", Svg.Attributes.fill "rgb(194 0 0)", Svg.Attributes.stroke "black" ]
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
            , el blockRowBlockStyle <| statBlock char.stats.str <| Debug.log "lootStr" (.str (totalEquippedStats char.items))
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Dex"
            , el blockRowBlockStyle <| statBlock char.stats.dex <| .dex (totalEquippedStats char.items)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Con"
            , el blockRowBlockStyle <| statBlock char.stats.con <| .con (totalEquippedStats char.items)
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
            , el blockRowBlockStyle <| statBlock char.stats.int <| .int (totalEquippedStats char.items)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Wis"
            , el blockRowBlockStyle <| statBlock char.stats.wis <| .wis (totalEquippedStats char.items)
            ]
        , row blockRowStyle
            [ el blockRowLabelStyle <| text "Cha"
            , el blockRowBlockStyle <| statBlock char.stats.cha <| .cha (totalEquippedStats char.items)
            ]
        ]


effortRow : Character -> Element Msg
effortRow char =
    let
        labelStyle =
            [ centerX, Font.size (scaled -1) ]
    in
    row
        [ width fill
        , spacingXY 10 0
        ]
        [ row blockRowStyle
            [ el labelStyle <| text "Basic (D4)"
            , el blockRowBlockStyle <| statBlock char.stats.basic <| .basic (totalEquippedStats char.items)
            ]
        , row blockRowStyle
            [ el labelStyle <| text "Weapon (D6)"
            , el blockRowBlockStyle <| statBlock char.stats.weapon <| .weapon (totalEquippedStats char.items)
            ]
        , row blockRowStyle
            [ el labelStyle <| text "Magic (D8)"
            , el blockRowBlockStyle <| statBlock char.stats.magic <| .magic (totalEquippedStats char.items)
            ]
        , row blockRowStyle
            [ el labelStyle <| text "Ultimate (D12)"
            , el blockRowBlockStyle <| statBlock char.stats.ultimate <| .ultimate (totalEquippedStats char.items)
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


fillListUntil : a -> Int -> List a -> List a
fillListUntil filler amount list =
    let
        fillerCount =
            amount - List.length list
    in
    if fillerCount <= 0 then
        list

    else
        list ++ List.repeat fillerCount filler



-- ANCHOR itemCol


itemCol : Bool -> String -> String -> List Item -> Element Msg
itemCol equippedState label modifyLabel items =
    column
        [ width fill
        , Element.alignTop
        , spacingXY 0 10
        ]
        [ row [ width fill ]
            [ el
                [ alignLeft
                , alignTop
                , Font.size (scaled -1)
                , paddingXY 0 10
                ]
              <|
                text
                    (label
                        ++ (String.fromInt <|
                                List.length <|
                                    itemsIndexed equippedState items
                           )
                        ++ "/10"
                    )
            , newItemButton equippedState
            ]
        , column
            [ width fill
            , spacingXY 0 10
            , paddingXY 10 10
            , Background.color (rgb255 244 244 244)
            ]
          <|
            List.map (itemRow (stateModifier modifyLabel) editModifier)
                (itemsIndexed equippedState items)
        ]


stateModifier : String -> Int -> Element Msg
stateModifier label ix =
    Input.button []
        { onPress = Just <| ToggleItem ix
        , label = el [ Font.size (scaled -1) ] <| text label
        }


editModifier : Int -> Element Msg
editModifier ix =
    Input.button []
        { onPress = Just <| EditItem ix
        , label = el [ Font.size (scaled -1) ] <| text "Edit"
        }


newItemButton : Bool -> Element Msg
newItemButton equipped =
    Input.button [ alignRight ]
        { onPress = Just <| NewItem equipped
        , label =
            el
                [ width fill
                , Border.width 1
                , padding 5
                , Border.dotted
                , Font.center
                , Font.size (scaled -2)
                ]
            <|
                text "Add item"
        }



-- ANCHOR itemrow


itemRow : (Int -> Element Msg) -> (Int -> Element Msg) -> ( Int, Item ) -> Element Msg
itemRow modifierButton editButton ( ix, item ) =
    row
        [ spacingXY 10 0
        , width fill
        , paddingXY 10 10
        , Border.widthEach
            { bottom = 2
            , left = 1
            , right = 1
            , top = 1
            }
        , Border.rounded 5
        , Background.color (rgb255 255 255 255)
        ]
        [ el [ Font.bold, alignBottom ] <| text item.name
        , el [ Font.size (scaled -3), alignBottom ] <| text <| printStats item.stats
        , el [ Font.size (scaled -2), alignBottom ] <| text item.description
        , el [ alignRight ] <| editButton ix
        , el [ alignRight ] <| modifierButton ix
        ]


printStats : Stats -> String
printStats stats =
    List.foldl joinStrings "" <|
        List.map printStat <|
            [ ( "Str", stats.str )
            , ( "Dex", stats.dex )
            , ( "Con", stats.con )
            , ( "Wis", stats.wis )
            , ( "Int", stats.int )
            , ( "Cha", stats.cha )
            , ( "Basic", stats.basic )
            , ( "Weapon", stats.weapon )
            , ( "Magic", stats.magic )
            , ( "Ultimate", stats.ultimate )
            , ( "Armor", stats.armor )
            , ( "Heart", stats.hearts )
            ]


joinStrings : Maybe String -> String -> String
joinStrings mstr res =
    case mstr of
        Just str ->
            case res of
                "" ->
                    str

                _ ->
                    res ++ ", " ++ str

        Nothing ->
            res


printStat : ( String, Int ) -> Maybe String
printStat val_stat =
    let
        val =
            Tuple.first val_stat

        stat =
            Tuple.second val_stat
    in
    if stat > 0 then
        Just <| val ++ " +" ++ String.fromInt stat

    else if stat < 0 then
        Just <| val ++ " -" ++ String.fromInt stat

    else
        Nothing


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
            text (String.fromInt <| basestat + lootstat)
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


totalEquippedStats : List Item -> Stats
totalEquippedStats items =
    List.foldr sumStatsEquipped (Stats 0 0 0 0 0 0 0 0 0 0 0 0) <|
        List.map .stats (List.filter .equipped items)


sumStatsEquipped : Stats -> Stats -> Stats
sumStatsEquipped s1 s2 =
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


asEditingStateIn : AppSettings -> EditingState -> AppSettings
asEditingStateIn state newState =
    { state | editingState = newState }


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


asEditableTextIn : AppSettings -> Maybe CharacterTextAttribute -> AppSettings
asEditableTextIn setting editable =
    { setting | editableText = editable }


asEditableNumberIn : AppSettings -> Maybe CharacterNumberAttribute -> AppSettings
asEditableNumberIn setting editable =
    { setting | editableNumber = editable }


asSettingsIn : Model -> AppSettings -> Model
asSettingsIn model settings =
    { model | settings = settings }


asItemsIn : Character -> List Item -> Character
asItemsIn char items =
    { char | items = items }


asHoveredIn : CharacterTextProp -> Bool -> CharacterTextProp
asHoveredIn charp hovered =
    { charp | hovered = hovered }
