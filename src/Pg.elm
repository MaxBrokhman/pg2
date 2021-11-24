port module Pg exposing (ImageEffects, Model, Msg(..), Photo, Status(..), init, initialModel, photoDecoder, update, urlPrefix, view, subscriptions)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import List
import Random


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias ImageEffects =
    { hue : Int
    , ripple : Int
    , noise : Int
    }


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , imageEffects : ImageEffects
    , activity : String
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    , imageEffects =
        { hue = 5
        , ripple = 5
        , noise = 5
        }
    , activity = ""
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title <| thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]"
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick <| ClickedPhoto thumb.url
        ]
        []


sizeChooserCheckHandler : ThumbnailSize -> ThumbnailSize -> Bool -> Msg
sizeChooserCheckHandler currentSize selectedSize checked =
    if checked == True then
        ClickedSize currentSize

    else
        ClickedSize selectedSize


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label
        [ onCheck <| sizeChooserCheckHandler size selectedSize ]
        [ input [ type_ "radio", name "size", checked (size == selectedSize) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = ClickedPhoto String
    | GotRandomPhoto Photo
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotPhotos (Result Http.Error (List Photo))
    | SlideHue Int
    | SlideRipple Int
    | SlideNoise Int
    | GotActivity String


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" <| Json.Encode.int magnitude
            , onSlide toMsg
            ]
            []
        , label [] [ text <| String.fromInt magnitude ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div
        [ class "filters" ]
        [ viewFilter SlideHue "Hue" model.imageEffects.hue
        , viewFilter SlideRipple "Ripple" model.imageEffects.ripple
        , viewFilter SlideNoise "Noise" model.imageEffects.noise
        ]
    , h3 [] [ text "Thumbnail size:" ]
    , div [ id "choose-size" ] <|
        List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ]
    , div [ id "thumbnails", class <| sizeToString model.chosenSize ] <|
        List.map (viewThumbnail selectedUrl) photos
    , canvas [ id "main-canvas", class "large" ] []
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            case photos of
                first :: _ ->
                    applyFilters
                        { model
                            | status = Loaded photos first.url
                        }

                [] ->
                    ( { model | status = Errored "No photos found!" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error !" }, Cmd.none )

        SlideHue num ->
            let
                imageEffects =
                    model.imageEffects

                updatedEffects =
                    { imageEffects | hue = num }
            in
            applyFilters { model | imageEffects = updatedEffects }

        SlideNoise num ->
            let
                imageEffects =
                    model.imageEffects

                updatedEffects =
                    { imageEffects | noise = num }
            in
            applyFilters { model | imageEffects = { updatedEffects | noise = num } }

        SlideRipple num ->
            let
                imageEffects =
                    model.imageEffects

                updatedEffects =
                    { imageEffects | noise = num }
            in
            applyFilters { model | imageEffects = { updatedEffects | ripple = num } }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.imageEffects.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.imageEffects.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.imageEffects.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, initialCmd )


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    Json.Decode.at [ "detail", "userSlideTo" ] int
        |> Json.Decode.map toMsg
        |> Html.Events.on "slide"
