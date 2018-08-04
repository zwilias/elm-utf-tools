module String.UTF16 exposing (foldl, length, toBytes, toString)

{-| For some reason want to reason about strings as if they were UTF-16
sequences?

Technically, that's what they are, but Elm allows treating them as Unicode
codepoints, which is super-convenient. Occasionally, however, you may find
yourself in need of some old fashioned UTF-16 bytes, so here you go!

@docs length, toBytes, toString, foldl

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)


{-| -}
foldl : (Int -> acc -> acc) -> acc -> String -> acc
foldl op initialAcc string =
    String.foldr (utf32ToUtf16Bytes op) initialAcc string


utf32ToUtf16Bytes : (Int -> acc -> acc) -> Char -> acc -> acc
utf32ToUtf16Bytes op char acc =
    let
        int : Int
        int =
            Char.toCode char
    in
    if int <= 0x00010000 then
        op int acc
    else
        let
            c =
                int - 0x00010000
        in
        op
            (shiftRightZfBy 10 c |> or 0xD800)
            (op (and 0x03FF c |> or 0xDC00) acc)


{-| Count the number of UTF-16 bytes in a `String`.

Note that this should always equal `String.length` in Elm.

-}
length : String -> Int
length =
    String.length


{-| Turn a `String` into a list of UTF-16 bytes.
-}
toBytes : String -> List Int
toBytes s =
    foldl (::) [] s


{-| Turn a list of UTF-16 bytes into a `String`.
-}
toString : List Int -> String
toString bytes =
    bytes
        |> List.foldl utf16ToUtf32 ( "", Nothing )
        |> Tuple.first


type alias UTF16Acc =
    ( String, Maybe Int )


utf16ToUtf32 : Int -> UTF16Acc -> UTF16Acc
utf16ToUtf32 char ( acc, combine ) =
    case combine of
        Nothing ->
            if char >= 0xD800 && char < 0xE000 then
                ( acc, Just char )
            else
                ( acc ++ utf32ByteToString char, Nothing )

        Just prev ->
            ( prev
                |> and 0x03FF
                |> shiftLeftBy 10
                |> or (and 0x03FF char)
                |> (+) 0x00010000
                |> (\x -> acc ++ utf32ByteToString x)
            , Nothing
            )


utf32ByteToString : Int -> String
utf32ByteToString byte =
    Char.fromCode byte |> String.fromChar
