module PgTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Pg exposing (Model, Msg(..), Photo, initialModel, update)
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


slideHueSetsHue : Test
slideHueSetsHue =
    fuzz int "SlideHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlideHue amount)
                |> Tuple.first
                |> .imageEffects
                |> .hue
                |> Expect.equal amount
