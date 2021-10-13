module PgTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import Pg exposing (ImageEffects, Model, Msg(..), Photo, initialModel, update)
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


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlideHue" SlideHue .hue
        , testSlider "SlideRipple" SlideRipple .ripple
        , testSlider "SlideNoise" SlideNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (ImageEffects -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> .imageEffects
                |> amountFromModel
                |> Expect.equal amount
