module KeyboardUtil
    exposing
        ( inputKindFromKeyInput
        , KeyInput
        , InputKind(Char, Arrow, Enter)
        , Direction(Left, Right, Up, Down)
        )

import Dict exposing (Dict)


type alias KeyInput =
    { value : String
    , fShift : Bool
    , fCtrl : Bool
    , fAlt : Bool
    }


type Direction
    = Left
    | Right
    | Up
    | Down


type InputKind
    = Char String
    | Arrow Direction
    | Enter


inputKindFromKeyInput : KeyInput -> Maybe InputKind
inputKindFromKeyInput keyInput =
    let
        hasModifier =
            keyInput.fCtrl || keyInput.fAlt
    in
        if (hasModifier) then
            Nothing
        else
            Dict.get keyInput.value dictInputKind


dictInputKind : Dict String InputKind
dictInputKind =
    Dict.fromList
        [ ( "a", Char "a" )
        , ( "b", Char "b" )
        , ( "c", Char "c" )
        , ( "d", Char "d" )
        , ( "e", Char "e" )
        , ( "f", Char "f" )
        , ( "g", Char "g" )
        , ( "h", Char "h" )
        , ( "i", Char "i" )
        , ( "j", Char "j" )
        , ( "k", Char "k" )
        , ( "l", Char "l" )
        , ( "m", Char "m" )
        , ( "n", Char "n" )
        , ( "o", Char "o" )
        , ( "p", Char "p" )
        , ( "q", Char "q" )
        , ( "r", Char "r" )
        , ( "s", Char "s" )
        , ( "t", Char "t" )
        , ( "u", Char "u" )
        , ( "v", Char "v" )
        , ( "w", Char "w" )
        , ( "x", Char "x" )
        , ( "y", Char "y" )
        , ( "z", Char "z" )
        , ( "A", Char "A" )
        , ( "B", Char "B" )
        , ( "C", Char "C" )
        , ( "D", Char "D" )
        , ( "E", Char "E" )
        , ( "F", Char "F" )
        , ( "G", Char "G" )
        , ( "H", Char "H" )
        , ( "I", Char "I" )
        , ( "J", Char "J" )
        , ( "K", Char "K" )
        , ( "L", Char "L" )
        , ( "M", Char "M" )
        , ( "N", Char "N" )
        , ( "O", Char "O" )
        , ( "P", Char "P" )
        , ( "Q", Char "Q" )
        , ( "R", Char "R" )
        , ( "S", Char "S" )
        , ( "T", Char "T" )
        , ( "U", Char "U" )
        , ( "V", Char "V" )
        , ( "W", Char "W" )
        , ( "X", Char "X" )
        , ( "Y", Char "Y" )
        , ( "Z", Char "Z" )
        , ( "1", Char "1" )
        , ( "2", Char "2" )
        , ( "3", Char "3" )
        , ( "4", Char "4" )
        , ( "5", Char "5" )
        , ( "6", Char "6" )
        , ( "7", Char "7" )
        , ( "8", Char "8" )
        , ( "9", Char "9" )
        , ( " ", Char " " )
        , ( "@", Char "@" )
        , ( "€", Char "€" )
        , ( "-", Char "-" )
        , ( "_", Char "_" )
        , ( ".", Char "." )
        , ( ":", Char ":" )
        , ( ",", Char "," )
        , ( ";", Char ";" )
        , ( "+", Char "+" )
        , ( "*", Char "*" )
        , ( "~", Char "~" )
        , ( "#", Char "#" )
        , ( "'", Char "'" )
        , ( "\"", Char "\"" )
        , ( "!", Char "!" )
        , ( "§", Char "§" )
        , ( "$", Char "$" )
        , ( "%", Char "%" )
        , ( "&", Char "&" )
        , ( "/", Char "/" )
        , ( "(", Char "(" )
        , ( ")", Char ")" )
        , ( "[", Char "]" )
        , ( "[", Char "]" )
        , ( "{", Char "{" )
        , ( "}", Char "}" )
        , ( "=", Char "=" )
        , ( "?", Char "?" )
        , ( "`", Char "`" )
        , ( "´", Char "´" )
        , ( "<", Char "<" )
        , ( ">", Char ">" )
        , ( "|", Char "|" )
        , ( "²", Char "²" )
        , ( "³", Char "³" )
        , ( "ArrowLeft", Arrow Left )
        , ( "ArrowUp", Arrow Up )
        , ( "ArrowRight", Arrow Right )
        , ( "ArrowDown", Arrow Down )
        , ( "Enter", Enter )
        ]
