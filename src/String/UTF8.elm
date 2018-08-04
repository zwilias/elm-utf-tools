module String.UTF8 exposing (foldl, length, toBytes, toString)

{-| Need UTF-8 bytes for your algorithm? Don't want to waste time investigating
how to go back and forth between UTF-16 and UTF-8? I hear ya.

Examples in the documentation assume an import like this:

    import String.UTF8 as UTF8

@docs length, toBytes, toString, foldl

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import String.UTF32 as UTF32 exposing (foldlUTF8)


{-| Convert a sequence of UTF-8 bytes to an Elm UTF-16 `String`.

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
    foldlUTF8 (\char string -> string ++ UTF32.byteToString char) "" input


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
foldl op acc input =
    UTF32.foldl (utf32ToUtf8 op) acc input


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
