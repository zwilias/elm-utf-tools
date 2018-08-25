module UTF8Test exposing (cases, lengthTest, toBytesTest, toStringTest)

import Expect exposing (Expectation)
import String.UTF8 as UTF8
import Test exposing (..)


cases : List ( String, List Int )
cases =
    [ ( "", [] )
    , ( "hello", [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ] )
    , ( "💩", [ 0xF0, 0x9F, 0x92, 0xA9 ] )
    , ( "abc💩def💩ghi", [ 0x61, 0x62, 0x63, 0xF0, 0x9F, 0x92, 0xA9, 0x64, 0x65, 0x66, 0xF0, 0x9F, 0x92, 0xA9, 0x67, 0x68, 0x69 ] )
    , ( "✓ à la mode", [ 0xE2, 0x9C, 0x93, 0x20, 0xC3, 0xA0, 0x20, 0x6C, 0x61, 0x20, 0x6D, 0x6F, 0x64, 0x65 ] )
    ]


lengthTest : Test
lengthTest =
    cases
        |> List.map (Tuple.mapSecond List.length)
        |> List.map
            (\( input, output ) ->
                test ("length of '" ++ input ++ "'") <|
                    \_ ->
                        UTF8.length input
                            |> Expect.equal output
            )
        |> describe "length"


toBytesTest : Test
toBytesTest =
    cases
        |> List.map
            (\( input, output ) ->
                test ("'" ++ input ++ "' to bytes") <|
                    \_ ->
                        UTF8.toBytes input
                            |> Expect.equal output
            )
        |> describe "bytes"


toStringTest : Test
toStringTest =
    cases
        |> List.map
            (\( output, input ) ->
                test ("decode into '" ++ output ++ "'") <|
                    \_ ->
                        UTF8.toString input
                            |> Expect.equal (Ok output)
            )
        |> describe "bytes to String"
