module Pages.Home_ exposing (Model, Msg, page)

import Api.Request
import Data.Types exposing (Sheet)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html
import Html.Attributes exposing (class)
import Icons
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Shared
import Utils
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared.apiBaseUrl
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { sheets : WebData (List Sheet) }


init : String -> ( Model, Cmd Msg )
init baseUrl =
    ( { sheets = Loading }, Api.Request.getSheets baseUrl GotSheets )



-- UPDATE


type Msg
    = GotSheets (WebData (List Sheet))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSheets data ->
            ( { model | sheets = data }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Signup | Home"
    , body =
        [ Html.div
            [ Utils.classes
                [ "flex"
                , "flex-col"
                , "h-full"
                , "min-h-full"
                ]
            ]
            [ Html.h1
                [ Utils.classes
                    [ "p-32"
                    , "text-center"
                    , "bg-black"
                    , "text-white"
                    , "flex"
                    , "flex-col"
                    , "items-center"
                    , "space-y-7"
                    ]
                ]
                [ Html.p [ Utils.classes [ "text-3xl" ] ]
                    [ Html.text "Signup Sheets" ]
                , Html.a
                    [ Utils.classes
                        [ "bg-white"
                        , "text-black"
                        , "p-2"
                        , "rounded-lg"
                        , "w-44"
                        ]
                    , Html.Attributes.href (Route.toHref Route.New)
                    ]
                    [ Html.text "New Signup" ]
                ]
            , Html.div [ Utils.classes [ "p-4", "flex-1" ] ]
                [ contentView model.sheets ]
            ]
        ]
    }


contentView : WebData (List Sheet) -> Html.Html Msg
contentView data =
    case data of
        NotAsked ->
            Html.div [] [ Html.text "No data fetch requested" ]

        Loading ->
            Html.div [ Utils.classes [ "flex", "items-center", "justify-center", "h-full" ] ] [ Icons.spinner ]

        Failure _ ->
            Html.div [] [ Html.text "Failed" ]

        Success _ ->
            Html.div [] [ Html.text "Succeeded" ]
