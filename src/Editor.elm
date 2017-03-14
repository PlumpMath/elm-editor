module Editor exposing (main)

import Html exposing (Html, Attribute, div, span, p, text)
import Html.Events exposing (on)
import Html.Attributes exposing (class, tabindex, style)
import Json.Decode as Decode exposing (Decoder, field)
import Dict as Dict exposing (Dict)
import KeyboardUtil exposing (..)
import Types exposing (..)
import Util exposing (..)
import Selection exposing (..)


-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model initParas Nothing, Cmd.none )


initParas : Dict Int Para
initParas =
    Dict.fromList [ ( 0, { text = "Hello World!" } ), ( 1, { text = "This is a new paragraph!" } ) ]



-- UPDATE


type Msg
    = Blur
    | UpdateSelection TextCoordinate
    | KeyDown KeyInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Blur ->
            { model | selection = Nothing } ! []

        UpdateSelection textCoordinate ->
            let
                modelNew =
                    createSelection textCoordinate
                        |> asSelectionIn model
            in
                modelNew ! []

        KeyDown keyInput ->
            case model.selection of
                Nothing ->
                    model ! []

                Just selectionCur ->
                    (handleKeyInput model selectionCur keyInput) ! []


handleKeyInput : Model -> Selection -> KeyInput -> Model
handleKeyInput model selectionCur keyInput =
    case (inputKindFromKeyInput keyInput) of
        Nothing ->
            model

        Just inputKind ->
            case (inputKind) of
                Char stChar ->
                    handleCharInput model selectionCur stChar

                Arrow direction ->
                    handleArrowInput model selectionCur direction

                Enter ->
                    handleEnterInput model selectionCur


handleCharInput : Model -> Selection -> String -> Model
handleCharInput model ({ cursorCoordinate } as selection) stChar =
    let
        textPosCur =
            cursorCoordinate.textPos

        dictParasNew =
            Dict.update textPosCur.iPara (newPara stChar selection) model.paras

        modelNew =
            (textPosCur.offset + 1)
                |> asOffsetIn textPosCur
                |> asTextPosIn cursorCoordinate
                |> asCursorCoordinateIn selection
                |> Just
                |> asSelectionIn model
                |> setParas dictParasNew
    in
        modelNew


handleArrowInput : Model -> Selection -> Direction -> Model
handleArrowInput model { cursorCoordinate } direction =
    let
        movedSelection =
            case (direction) of
                Left ->
                    moveLeft model cursorCoordinate

                Right ->
                    moveRight model cursorCoordinate

                Up ->
                    moveUpOrDown model cursorCoordinate (cursorCoordinate.textPos.iPara - 1)

                Down ->
                    moveUpOrDown model cursorCoordinate (cursorCoordinate.textPos.iPara + 1)
    in
        movedSelection
            |> asSelectionIn model


handleEnterInput : Model -> Selection -> Model
handleEnterInput model ({ cursorCoordinate } as selection) =
    let
        paraCur =
            Dict.get cursorCoordinate.textPos.iPara model.paras

        modelNew =
            case paraCur of
                Nothing ->
                    model

                Just para ->
                    (cursorCoordinate.textPos.iPara + 1)
                        |> asIParaIn cursorCoordinate.textPos
                        |> setOffset 0
                        |> asTextPosIn cursorCoordinate
                        |> setLocation Before
                        |> asCursorCoordinateIn selection
                        |> Just
                        |> asSelectionIn model
                        |> setParas (updateParas model.paras para cursorCoordinate)
    in
        modelNew


updateParas : Dict Int Para -> Para -> TextCoordinate -> Dict Int Para
updateParas dictParas para ({ textPos } as cursorCoordinate) =
    let
        ( paraBefore, paraAfter ) =
            splitPara para cursorCoordinate

        parasAsList =
            Dict.toList dictParas

        parasAsListUpdated =
            List.map (shiftPara textPos.iPara paraBefore) parasAsList

        parasAsListCompleted =
            List.append parasAsListUpdated [ ( textPos.iPara + 1, paraAfter ) ]
    in
        Dict.fromList parasAsListCompleted


splitPara : Para -> TextCoordinate -> ( Para, Para )
splitPara para ({ textPos, location } as cursorCoordinate) =
    let
        paraLength =
            String.length para.text
    in
        if (cursorCoordinate |> isAtBeginning) then
            ( Para "", Para para.text )
        else if (cursorCoordinate |> isAfter paraLength) then
            ( Para para.text, Para "" )
        else
            let
                offsetNormalized =
                    case location of
                        Before ->
                            textPos.offset

                        After ->
                            textPos.offset + 1

                ( textLeft, textRight ) =
                    ( String.left offsetNormalized para.text, String.dropLeft offsetNormalized para.text )
            in
                ( Para textLeft, Para textRight )


isAtBeginning : TextCoordinate -> Bool
isAtBeginning { textPos, location } =
    textPos.offset == 0 && location == Before


isAfter : Int -> TextCoordinate -> Bool
isAfter end { textPos, location } =
    (textPos.offset == end - 1 && location == After)
        || (textPos.offset >= end)


shiftPara : Int -> Para -> ( Int, Para ) -> ( Int, Para )
shiftPara iParaChanged paraBefore ( iParaCur, paraCur ) =
    if (iParaCur < iParaChanged) then
        ( iParaCur, paraCur )
    else if (iParaCur == iParaChanged) then
        ( iParaCur, paraBefore )
    else
        ( iParaCur + 1, paraCur )


newPara : String -> Selection -> Maybe Para -> Maybe Para
newPara stChar selection paraToUpdate =
    case paraToUpdate of
        Just para ->
            let
                insertionPoint =
                    getInsertionPoint selection.cursorCoordinate

                paraLeft =
                    String.left insertionPoint para.text

                paraRight =
                    String.dropLeft insertionPoint para.text
            in
                Just { para | text = paraLeft ++ stChar ++ paraRight }

        Nothing ->
            Nothing


getInsertionPoint : TextCoordinate -> Int
getInsertionPoint tc =
    if (tc.location == Before) then
        tc.textPos.offset
    else
        tc.textPos.offset + 1



-- VIEW


view : Model -> Html Msg
view model =
    let
        paras =
            Dict.values model.paras
    in
        div
            [ style [ ( "white-space", "pre-wrap" ) ] ]
        <|
            List.indexedMap (viewPara model.selection) paras


viewPara : Maybe Selection -> Int -> Para -> Html Msg
viewPara selection iPara para =
    let
        textAsList =
            String.toList para.text

        lineLength =
            List.length textAsList
                |> cropTo 0 (List.length textAsList)

        rangeCharas =
            List.range 0 lineLength

        textPositions =
            List.map (TextPosition iPara) rangeCharas

        paraList =
            if (para.text == "") then
                [ viewStChar selection lineLength (TextPosition iPara 0) "" ]
            else
                List.map2 (viewChar selection lineLength) textPositions textAsList
    in
        p [ paraStyle ] paraList


paraStyle : Attribute msg
paraStyle =
    style
        [ ( "border", "1px dashed blue" )
        , ( "margin", "1px" )
        , ( "-webkit-touch-callout", "none" )
        , ( "-webkit-user-select", "none" )
        , ( "-khtml-user-select", "none" )
        , ( "-moz-user-select", "none" )
        , ( "-ms-user-select", "none" )
        , ( "user-select", "none" )
        ]


viewStChar : Maybe Selection -> Int -> TextPosition -> String -> Html Msg
viewStChar selection maxLength textPos stChar =
    span
        [ class <| getCaretClass selection maxLength textPos
        , tabindex -1
        , onClick textPos UpdateSelection
        , onKeyDown KeyDown
        ]
        [ text stChar ]


viewChar : Maybe Selection -> Int -> TextPosition -> Char -> Html Msg
viewChar selection maxLength textPos char =
    viewStChar selection maxLength textPos <| String.fromChar char


onClick : TextPosition -> (TextCoordinate -> Msg) -> Attribute Msg
onClick textPos tagger =
    on "click" (Decode.map tagger (textCoordinateDecoder textPos))


onKeyDown : (KeyInput -> Msg) -> Attribute Msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger decodeKeyInput)


getCaretClass : Maybe Selection -> Int -> TextPosition -> String
getCaretClass selection maxLength textPos =
    case selection of
        Nothing ->
            ""

        Just sel ->
            let
                textPosSel =
                    sel.cursorCoordinate.textPos
            in
                if (textPos |> atLocation textPosSel maxLength) then
                    case sel.cursorCoordinate.location of
                        Before ->
                            "caretB"

                        After ->
                            "caretA"
                else
                    ""


atLocation : TextPosition -> Int -> TextPosition -> Bool
atLocation textPosSel maxLength textPos =
    textPosSel.iPara
        == textPos.iPara
        && (textPosSel.offset == textPos.offset || textPosSel.offset >= maxLength && textPos.offset == maxLength)


decodeKeyInput : Decoder KeyInput
decodeKeyInput =
    Decode.map4
        KeyInput
        (field "key" Decode.string)
        (field "shiftKey" Decode.bool)
        (field "ctrlKey" Decode.bool)
        (field "altKey" Decode.bool)


textCoordinateDecoder : TextPosition -> Decoder TextCoordinate
textCoordinateDecoder textPos =
    Decode.map3
        (newTextCoordinate textPos)
        (field "target" (field "offsetWidth" Decode.float))
        (field "pageX" Decode.int)
        (field "target" (field "offsetLeft" Decode.int))


newTextCoordinate : TextPosition -> Float -> Int -> Int -> TextCoordinate
newTextCoordinate textPos targetWidth pageX targetOffsetLeft =
    let
        middle =
            targetWidth / 2

        clickX =
            toFloat (pageX - targetOffsetLeft)
    in
        TextCoordinate textPos
            (if (clickX < middle) then
                Before
             else
                After
            )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Platform.Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
