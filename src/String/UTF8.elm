module String.UTF8 exposing (foldl, length, toBytes, toString)

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import String.UTF32 as UTF32 exposing (foldlUTF8)


toString : List Int -> Result String String
toString input =
    foldlUTF8 (\char string -> string ++ UTF32.byteToString char) "" input


toBytes : String -> List Int
toBytes input =
    foldl (::) [] input |> List.reverse


foldl : (Int -> a -> a) -> a -> String -> a
foldl op acc input =
    UTF32.foldl (utf32ToUtf8 op) acc input


length : String -> Int
length input =
    foldl (always <| (+) 1) 0 input


type alias Accumulator a =
    ( a, Maybe Int )


utf32ToUtf8 : (Int -> a -> a) -> Int -> a -> a
utf32ToUtf8 add char acc =
    if char < 0x80 then
        acc |> add char
    else if char < 0x0800 then
        acc
            |> add (or 0xC0 <| shiftRightZfBy 6 char)
            |> add (or 0x80 <| and 0x3F char)
    else if char < 0x00010000 then
        acc
            |> add (or 0xE0 <| shiftRightZfBy 12 char)
            |> add (or 0x80 <| and 0x3F <| shiftRightZfBy 6 char)
            |> add (or 0x80 <| and 0x3F char)
    else
        acc
            |> add (or 0xF0 <| shiftRightZfBy 18 char)
            |> add (or 0x80 <| and 0x3F <| shiftRightZfBy 12 char)
            |> add (or 0x80 <| and 0x3F <| shiftRightZfBy 6 char)
            |> add (or 0x80 <| and 0x3F char)
