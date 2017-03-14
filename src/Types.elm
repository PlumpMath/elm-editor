module Types
    exposing
        ( Model
        , setSelection
        , asSelectionIn
        , setParas
        , asParasIn
        , Para
        , setText
        , asTextIn
        , Selection
        , setCursorCoordinate
        , asCursorCoordinateIn
        , setAnchorCoordinate
        , asAnchorCoordinateIn
        , TextCoordinate
        , setTextPos
        , asTextPosIn
        , setLocation
        , asLocationIn
        , TextPosition
        , setOffset
        , asOffsetIn
        , setIPara
        , asIParaIn
        , Location(Before, After)
        )

import Dict exposing (Dict)


type alias Model =
    { paras : Dict Int Para
    , selection : Maybe Selection
    }


setSelection : Maybe Selection -> Model -> Model
setSelection selectionNew modelCur =
    { modelCur | selection = selectionNew }


asSelectionIn : Model -> Maybe Selection -> Model
asSelectionIn =
    flip setSelection


setParas : Dict Int Para -> Model -> Model
setParas parasNew modelCur =
    { modelCur | paras = parasNew }


asParasIn : Model -> Dict Int Para -> Model
asParasIn =
    flip setParas


type alias Para =
    { text : String }


setText : String -> Para -> Para
setText textNew paraCur =
    { paraCur | text = textNew }


asTextIn : Para -> String -> Para
asTextIn =
    flip setText


type alias Selection =
    { cursorCoordinate : TextCoordinate
    , anchorCoordinate : TextCoordinate
    }


setCursorCoordinate : TextCoordinate -> Selection -> Selection
setCursorCoordinate cursorCoordinateNew selectionCur =
    { selectionCur | cursorCoordinate = cursorCoordinateNew }


asCursorCoordinateIn : Selection -> TextCoordinate -> Selection
asCursorCoordinateIn =
    flip setCursorCoordinate


setAnchorCoordinate : TextCoordinate -> Selection -> Selection
setAnchorCoordinate anchorCoordinateNew selectionCur =
    { selectionCur | anchorCoordinate = anchorCoordinateNew }


asAnchorCoordinateIn : Selection -> TextCoordinate -> Selection
asAnchorCoordinateIn =
    flip setAnchorCoordinate


type alias TextCoordinate =
    { textPos : TextPosition
    , location : Location
    }


setTextPos : TextPosition -> TextCoordinate -> TextCoordinate
setTextPos textPosNew textCoordinateCur =
    { textCoordinateCur | textPos = textPosNew }


asTextPosIn : TextCoordinate -> TextPosition -> TextCoordinate
asTextPosIn =
    flip setTextPos


setLocation : Location -> TextCoordinate -> TextCoordinate
setLocation locationNew textCoordinateCur =
    { textCoordinateCur | location = locationNew }


asLocationIn : TextCoordinate -> Location -> TextCoordinate
asLocationIn =
    flip setLocation


type alias TextPosition =
    { iPara : Int
    , offset : Int
    }


setIPara : Int -> TextPosition -> TextPosition
setIPara iParaNew textPosCur =
    { textPosCur | iPara = iParaNew }


asIParaIn : TextPosition -> Int -> TextPosition
asIParaIn =
    flip setIPara


setOffset : Int -> TextPosition -> TextPosition
setOffset offsetNew textPosCur =
    { textPosCur | offset = offsetNew }


asOffsetIn : TextPosition -> Int -> TextPosition
asOffsetIn =
    flip setOffset


type Location
    = Before
    | After
