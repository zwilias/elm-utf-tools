module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String.UTF8 as UTF8
import Test exposing (..)


lengthTest : Test
lengthTest =
    [ ( "", 0 )
    , ( "hello", 5 )
    , ( "💩", 4 )
    , ( "abc💩def💩ghi", 17 )
    , ( "✓ à la mode", 14 )
    ]
        |> List.map
            (\( input, output ) ->
                test ("length of '" ++ input ++ "'") <|
                    \_ ->
                        UTF8.length input
                            |> Expect.equal output
            )
        |> describe "UTF8 length"


toBytesTest : Test
toBytesTest =
    [ ( "", [] )
    , ( "hello", [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ] )
    , ( "💩", [ 0xF0, 0x9F, 0x92, 0xA9 ] )
    , ( "abc💩def💩ghi", [ 0x61, 0x62, 0x63, 0xF0, 0x9F, 0x92, 0xA9, 0x64, 0x65, 0x66, 0xF0, 0x9F, 0x92, 0xA9, 0x67, 0x68, 0x69 ] )
    , ( "✓ à la mode", [ 0xE2, 0x9C, 0x93, 0x20, 0xC3, 0xA0, 0x20, 0x6C, 0x61, 0x20, 0x6D, 0x6F, 0x64, 0x65 ] )
    ]
        |> List.map
            (\( input, output ) ->
                test ("'" ++ input ++ "' to UTF8 bytes") <|
                    \_ ->
                        UTF8.toBytes input
                            |> Expect.equal output
            )
        |> describe "UTF8 bytes"


toStringTest : Test
toStringTest =
    [ ( "", [] )
    , ( "hello", [ 0x68, 0x65, 0x6C, 0x6C, 0x6F ] )
    , ( "💩", [ 0xF0, 0x9F, 0x92, 0xA9 ] )
    , ( "abc💩def💩ghi", [ 0x61, 0x62, 0x63, 0xF0, 0x9F, 0x92, 0xA9, 0x64, 0x65, 0x66, 0xF0, 0x9F, 0x92, 0xA9, 0x67, 0x68, 0x69 ] )
    , ( "✓ à la mode", [ 0xE2, 0x9C, 0x93, 0x20, 0xC3, 0xA0, 0x20, 0x6C, 0x61, 0x20, 0x6D, 0x6F, 0x64, 0x65 ] )
    ]
        |> List.map
            (\( output, input ) ->
                test ("decode into '" ++ output ++ "'") <|
                    \_ ->
                        UTF8.toString input
                            |> Expect.equal (Ok output)
            )
        |> describe "UTF8 bytes to String"
