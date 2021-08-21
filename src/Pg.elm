module Pg exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import List
import Random


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
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
        |> optional "title" string "untitled"


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


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                [ text "Loading..." ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise me!" ]
    , h3 [] [ text "Thumbnail size:" ]
    , div [ id "choose-size" ] <|
        List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ]
    , div [ id "thumbnails", class <| sizeToString chosenSize ] <|
        List.map (viewThumbnail selectedUrl) photos
    , img
        [ class "large"
        , src <| urlPrefix ++ "large/" ++ selectedUrl
        ]
        []
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

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
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok receivedPhotos) ->
            case receivedPhotos of
                (firstPhoto :: _) as photos ->
                    ( { model | status = Loaded photos firstPhoto.url }, Cmd.none )

                [] ->
                    ( { model | status = Errored "No photos found!" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error !" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
