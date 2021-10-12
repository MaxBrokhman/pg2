module PgTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Pg
import Test exposing (..)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue Pg.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")
