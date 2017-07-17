module UTF32Test exposing (..)

import Expect exposing (Expectation)
import String.UTF32 as UTF32
import Test exposing (..)


cases : List ( String, List Int )
cases =
    [ ( "", [] )
    , ( "hello", [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ] )
    , ( "💩", [ 0x0001F4A9 ] )
    , ( "abc💩def💩ghi", [ 0x61, 0x62, 0x63, 0x0001F4A9, 0x64, 0x65, 0x66, 0x0001F4A9, 0x67, 0x68, 0x69 ] )
    , ( "✓ à la mode", [ 0x2713, 0x20, 0xE0, 0x20, 0x6C, 0x61, 0x20, 0x6D, 0x6F, 0x64, 0x65 ] )
    ]


lengthTest : Test
lengthTest =
    cases
        |> List.map (Tuple.mapSecond List.length)
        |> List.map
            (\( input, output ) ->
                test ("length of '" ++ input ++ "'") <|
                    \_ ->
                        UTF32.length input
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
                        UTF32.toBytes input
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
                        UTF32.toString input
                            |> Expect.equal output
            )
        |> describe "bytes to String"
