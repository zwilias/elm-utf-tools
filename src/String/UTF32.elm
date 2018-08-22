module String.UTF32 exposing (length, toBytes, toString, foldl)

{-| Need to work in unicode codepoints? Tired of UTF-8's silliness? You came to
the right place!

Examples in the documentation assume an import like this:

    import String.UTF32 as UTF32

@docs length, toBytes, toString, foldl

-}

import Bitwise exposing (and, or, shiftLeftBy, shiftRightZfBy)
import Char


{-| Build a `String` from a list of unicode codepoints.

    UTF32.toString [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ]
    --> "hello"

    UTF32.toString [ 0x0001F4A9 ]
    --> "💩"

-}
toString : List Int -> String
toString bytes =
    List.foldl (\char string -> string ++ byteToString char) "" bytes


{-| Calculates the number UTF-32 characters in a `String`.

    String.length "💩"
    --> 2

    UTF32.length "💩"
    --> 1

-}
length : String -> Int
length input =
    foldl (\_ acc -> acc + 1) 0 input


{-| Converts a String to a list of unicode codepoints. The inverse of `toString`

    UTF32.toBytes "hello"
    --> [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ]

    UTF32.toBytes "💩"
    --> [ 0x0001F4A9 ]

-}
toBytes : String -> List Int
toBytes =
    foldl (::) [] >> List.reverse


{-| Fold over a string, left to right, accumulating unicode codepoints.
-}
foldl : (Int -> a -> a) -> a -> String -> a
foldl op initial input =
    String.foldl (\c acc -> op (Char.toCode c) acc) initial input



--     input
--         |> String.foldl (Char.toCode >> utf16ToUtf32 op) ( acc, Nothing )
--         |> Tuple.first
-- type alias UTF16Acc a =
--     ( a, Maybe Int )
-- utf16ToUtf32 : (Int -> a -> a) -> Int -> UTF16Acc a -> UTF16Acc a
-- utf16ToUtf32 add char ( acc, combine ) =
--     case combine of
--         Nothing ->
--             if char >= 0xD800 && char < 0xE000 then
--                 ( acc, Just char )
--             else
--                 ( add char acc, Nothing )
--         Just prev ->
--             ( prev
--                 |> and 0x03FF
--                 |> shiftLeftBy 10
--                 |> or (and 0x03FF char)
--                 |> (+) 0x00010000
--                 |> (\x -> add x acc)
--             , Nothing
--             )
-- {-| Fold over a list of UTF-8 bytes, converting them to unicode codepoints and
-- feeding those to your accumulator. If the input contains invalid sequences,
-- you'll receive an `invalid UTF-8 sequence` error.
-- -}
-- foldlUTF8 : (Int -> a -> a) -> a -> List Int -> Result String a
-- foldlUTF8 op acc input =
--     case List.foldl (utf8ToUtf32 op) ( 0, 0, acc ) input of
--         ( _, 0, res ) ->
--             Ok res
--         _ ->
--             Err "invalid UTF-8 sequence"
-- utf8ToUtf32 : (Int -> a -> a) -> Int -> UTF8Acc a -> UTF8Acc a
-- utf8ToUtf32 add char ( curr, need, acc ) =
--     let
--         shiftAndAdd : Int -> Int
--         shiftAndAdd int =
--             shiftLeftBy 6 curr
--                 |> or (and 0x3F int)
--     in
--     if need == 0 then
--         if and 0x80 char == 0 then
--             ( 0, 0, add char acc )
--         else if and 0xE0 char == 0xC0 then
--             ( and 0x1F char, 1, acc )
--         else if and 0xF0 char == 0xE0 then
--             ( and 0x0F char, 2, acc )
--         else
--             ( and 7 char, 3, acc )
--     else if need == 1 then
--         ( 0, 0, add (shiftAndAdd char) acc )
--     else
--         ( shiftAndAdd char, need - 1, acc )
-- type alias UTF8Acc a =
--     ( Int, Int, a )
-- {-| Convert a single codepoint to the equivalent Elm string.
--     byteToString 0x0001F4A9
--     --> "💩"
-- -}


byteToString : Int -> String
byteToString int =
    Char.fromCode int |> String.fromChar



-- if int <= 0x00010000 then
--     Char.fromCode int |> String.fromChar
-- else
--     let
--         c =
--             int - 0x00010000
--     in
--     [ Char.fromCode (shiftRightZfBy 10 c |> or 0xD800)
--     , Char.fromCode (and 0x03FF c |> or 0xDC00)
--     ]
--         |> String.fromList
