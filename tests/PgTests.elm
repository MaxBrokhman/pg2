module PgTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Json.Decode exposing (decodeString)
import Pg
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)"
        (\_ ->
            """
                { "url": "fruits.com", "size": 5}
            """
                |> decodeString Pg.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)" })
        )
