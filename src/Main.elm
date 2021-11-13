module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, caption, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)


type alias Model =
    { page : Page }


type Page
    = Gallery
    | Folders
    | NotFound


view : Model -> Document Msg
view model =
    { title = "Photo Groove, SPA style"
    , body =
        [ viewHeader model.page

        -- , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage { url, caption } =
            li [ classList [ ( "active", page == targetPage ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


viewFooter : Html Msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


type Msg
    = NothigYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { page = Folders }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
