module String.UTF8 exposing (length, toBytes, toString, foldl)

{-| Need UTF-8 bytes for your algorithm? Don't want to waste time investigating
how to go back and forth between UTF-32 and UTF-8? I hear ya.

Examples in the documentation assume an import like this:

    import String.UTF8 as UTF8

@docs length, toBytes, toString, foldl

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import String.UTF32 as UTF32


{-| Convert a sequence of UTF-8 bytes to an Elm `String`.

    UTF8.toString [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ]
    --> Ok "hello"

    UTF8.toString [ 0xF0, 0x9F, 0x92, 0xA9 ]
    --> Ok "💩"

If the input is not a valid UTF-8 sequence, you'll receive an error.

    UTF8.toString [ 0xF0]
    --> Err "invalid UTF-8 sequence"

-}
toString : List Int -> Result String String
toString input =
    foldlUTF8 (\char string -> string ++ utf32ByteToString char) "" input


utf32ByteToString : Int -> String
utf32ByteToString byte =
    Char.fromCode byte |> String.fromChar


foldlUTF8 : (Int -> a -> a) -> a -> List Int -> Result String a
foldlUTF8 op acc input =
    case List.foldl (utf8ToUtf32 op) ( 0, 0, acc ) input of
        ( _, 0, res ) ->
            Ok res

        _ ->
            Err "invalid UTF-8 sequence"


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


{-| Convert a `String` to a sequence of UTF-8 bytes. The inverse of `toString`.

    UTF8.toBytes "✓ a-ok"
    --> [ 0xe2, 0x9c, 0x93, 0x20, 0x61, 0x2D, 0x6F, 0x6B ]

-}
toBytes : String -> List Int
toBytes input =
    foldl (::) [] input |> List.reverse


{-| Fold over a string, left to right, accumulating UTF-8 bytes along the way.
-}
foldl : (Int -> a -> a) -> a -> String -> a
foldl op initialAcc input =
    String.foldl
        (\char acc ->
            utf32ToUtf8 op
                (Char.toCode char)
                acc
        )
        initialAcc
        input


{-| Number of UTF-8 codepoints in a string.

    UTF8.length "a"
    --> 1

    UTF8.length "à"
    --> 2

    UTF8.length "✓"
    --> 3

    UTF8.length "💩"
    --> 4

-}
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
