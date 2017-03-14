module Selection
    exposing
        ( moveLeft
        , moveRight
        , moveUpOrDown
        , createSelection
        )

import Types exposing (..)
import Dict exposing (Dict)
import Util exposing (..)


createSelection : TextCoordinate -> Maybe Selection
createSelection textCoordinate =
    Just <| Selection textCoordinate textCoordinate


dismantle : TextCoordinate -> ( Int, Int, Location )
dismantle tc =
    ( tc.textPos.offset, tc.textPos.iPara, tc.location )


moveLeft : Model -> TextCoordinate -> Maybe Selection
moveLeft model ({ textPos } as cursorCoordinate) =
    let
        ( offsetCur, iParaCur, locationCur ) =
            dismantle cursorCoordinate

        paraCur =
            Dict.get iParaCur model.paras

        cursorCoordinateNew =
            case paraCur of
                Nothing ->
                    -- this should never happen, since we should have a valid text coordinate at that point
                    cursorCoordinate

                Just para ->
                    let
                        paraLength =
                            String.length para.text

                        iParaAbove =
                            iParaCur - 1

                        offsetNew =
                            (offsetCur - 1)
                                |> cropTo -1 (paraLength - 1)
                    in
                        if (offsetNew < 0) then
                            if (paraLength > 0 && locationCur == After) then
                                -- we moved "after -1st" character: normalize to "before 0st"
                                textPos
                                    |> setOffset 0
                                    |> asTextPosIn cursorCoordinate
                                    |> setLocation Before
                            else
                                -- we moved "before -1st" character or we are in an empty para:
                                -- => normalize to "after last of prev. para"
                                let
                                    mbParaAbove =
                                        Dict.get iParaAbove model.paras
                                in
                                    case mbParaAbove of
                                        Nothing ->
                                            textPos
                                                |> setOffset 0
                                                |> setIPara 0
                                                |> asTextPosIn cursorCoordinate
                                                |> setLocation Before

                                        Just paraAbove ->
                                            let
                                                paraAboveLength =
                                                    String.length paraAbove.text

                                                offsetAbove =
                                                    paraAboveLength |> cropTo 0 paraAboveLength
                                            in
                                                textPos
                                                    |> setOffset offsetAbove
                                                    |> setIPara iParaAbove
                                                    |> asTextPosIn cursorCoordinate
                                                    |> setLocation After
                        else
                            textPos
                                |> setOffset offsetNew
                                |> asTextPosIn cursorCoordinate
    in
        createSelection cursorCoordinateNew


moveRight : Model -> TextCoordinate -> Maybe Selection
moveRight model ({ textPos } as cursorCoordinate) =
    let
        ( offsetCur, iParaCur, locationCur ) =
            dismantle cursorCoordinate

        mbParaCur =
            Dict.get iParaCur model.paras

        cursorCoordinateNew =
            case mbParaCur of
                Nothing ->
                    -- this should never happen, since we should have a valid text coordinate at that point
                    cursorCoordinate

                Just para ->
                    let
                        paraLength =
                            String.length para.text

                        iParaBelow =
                            iParaCur + 1

                        offsetNew =
                            (offsetCur + 1)
                                |> cropTo 0 (paraLength + 1)
                    in
                        if (offsetNew >= paraLength) then
                            if (paraLength > 0 && locationCur == Before) then
                                -- we moved "before last+1" character: normalize to "after last"
                                textPos
                                    |> setOffset (paraLength - 1)
                                    |> asTextPosIn cursorCoordinate
                                    |> setLocation After
                            else
                                -- we moved "after last+1" character or we are in an empty para:
                                -- => normalize to "before 1st of prev. para"
                                let
                                    mbParaBelow =
                                        Dict.get iParaBelow model.paras
                                in
                                    case mbParaBelow of
                                        Nothing ->
                                            textPos
                                                |> setOffset (paraLength - 1)
                                                |> setIPara iParaCur
                                                |> asTextPosIn cursorCoordinate
                                                |> setLocation After

                                        Just paraBelow ->
                                            let
                                                paraBelowLength =
                                                    String.length paraBelow.text

                                                offsetBelow =
                                                    0 |> cropTo 0 paraBelowLength
                                            in
                                                textPos
                                                    |> setOffset offsetBelow
                                                    |> setIPara iParaBelow
                                                    |> asTextPosIn cursorCoordinate
                                                    |> setLocation Before
                        else
                            textPos
                                |> setOffset offsetNew
                                |> asTextPosIn cursorCoordinate
    in
        createSelection cursorCoordinateNew



{--needs to get a Selection instead of TextCoordinate to handle ranges as well --}


moveUpOrDown : Model -> TextCoordinate -> Int -> Maybe Selection
moveUpOrDown model ({ textPos, location } as cursorCoordinate) iParaNew =
    let
        mbParaNext =
            Dict.get iParaNew model.paras
    in
        case (mbParaNext) of
            Nothing ->
                createSelection cursorCoordinate

            Just paraNext ->
                let
                    lengthParaNext =
                        String.length paraNext.text - 1

                    locationNew =
                        if (textPos.offset > lengthParaNext) then
                            After
                        else
                            location
                in
                    textPos
                        |> setIPara iParaNew
                        |> asTextPosIn cursorCoordinate
                        |> setLocation locationNew
                        |> createSelection
