module String.UTF32 exposing (foldl, foldlUTF8, toBytes)

import Bitwise exposing (and, or, shiftLeftBy)
import Char


toBytes : String -> List Int
toBytes =
    foldl (::) [] >> List.reverse


foldl : (Int -> a -> a) -> a -> String -> a
foldl op acc input =
    String.foldl (Char.toCode >> utf16ToUtf32 op) ( acc, Nothing ) input
        |> Tuple.first


type alias UTF16Acc a =
    ( a, Maybe Int )


utf16ToUtf32 : (Int -> a -> a) -> Int -> UTF16Acc a -> UTF16Acc a
utf16ToUtf32 add char ( acc, combine ) =
    case combine of
        Nothing ->
            if char >= 0xD800 && char < 0xE000 then
                ( acc, Just char )
            else
                ( add char acc, Nothing )

        Just prev ->
            ( prev
                |> and 0x03FF
                |> shiftLeftBy 10
                |> or (and 0x03FF char)
                |> (+) 0x00010000
                |> flip add acc
            , Nothing
            )


foldlUTF8 : (Int -> a -> a) -> a -> List Int -> Result String a
foldlUTF8 op acc input =
    case List.foldl (utf8ToUtf32 op) ( 0, 0, acc ) input of
        ( _, 0, res ) ->
            Ok res

        _ ->
            Err "invalid bytes"


utf8ToUtf32 : (Int -> a -> a) -> Int -> UTF8Acc a -> UTF8Acc a
utf8ToUtf32 add char ( curr, need, acc ) =
    let
        shiftAndAdd : Int -> Int
        shiftAndAdd int =
            shiftLeftBy 6 curr
                |> or (and 0x3F int)
    in
    if need == 0 then
        if and 0x80 char == 0 then
            ( 0, 0, add char acc )
        else if and 0xE0 char == 0xC0 then
            ( and 0x1F char, 1, acc )
        else if and 0xF0 char == 0xE0 then
            ( and 0x0F char, 2, acc )
        else
            ( and 7 char, 3, acc )
    else if need == 1 then
        ( 0, 0, add (shiftAndAdd char) acc )
    else
        ( shiftAndAdd char, need - 1, acc )


type alias UTF8Acc a =
    ( Int, Int, a )
