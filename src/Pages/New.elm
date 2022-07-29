module Pages.New exposing (Model, Msg, page)

import Browser.Events
import Data.Types exposing (Column, ColumnType(..), Index(..), Sheet, Widget(..), sheetWidgetRowsLens, signupModelColumns, widgetRowsRow, widgetSignupPrism)
import Gen.Params.New exposing (Params)
import Gen.Route as Route
import Html
import Html.Attributes
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode
import Maybe.Extra
import Monocle.Common
import Monocle.Compose
import Monocle.Optional
import Page
import Request
import Shared
import Svg.Attributes exposing (r)
import Utils
import View exposing (View)
import Views.Helpers
import Views.Signup


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { sheet : Sheet
    , workingWidget : Maybe ( Index, Widget )
    }


defaultWidget : Widget
defaultWidget =
    SignUp { columns = [] }


init : ( Model, Cmd Msg )
init =
    ( { sheet =
            { title = ""
            , slug = ""
            , widgetRows = []
            }
      , workingWidget = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddRow
    | StartAddingWidget Index
    | StopEditingWidget
    | ChangeEditingWidget Widget
    | NewSignupColumn
    | RemoveSignupColumn Index
    | SignupColumnTextChanged Index String
    | SignupColumnTypeChanged Index ColumnType
    | AddWidget


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRow ->
            let
                sheet =
                    model.sheet
            in
            ( { model | sheet = { sheet | widgetRows = sheet.widgetRows ++ [ [] ] } }, Cmd.none )

        StartAddingWidget rowIndex ->
            ( { model | workingWidget = Just ( rowIndex, defaultWidget ) }, Cmd.none )

        StopEditingWidget ->
            ( { model | workingWidget = Nothing }, Cmd.none )

        ChangeEditingWidget widget ->
            ( { model
                | workingWidget =
                    Maybe.map (\( idx, _ ) -> ( idx, widget )) model.workingWidget
              }
            , Cmd.none
            )

        NewSignupColumn ->
            let
                columnsOptional =
                    Monocle.Common.maybe
                        |> Monocle.Compose.optionalWithLens Monocle.Common.second
                        |> Monocle.Compose.optionalWithPrism widgetSignupPrism
                        |> Monocle.Compose.optionalWithLens signupModelColumns
            in
            ( { model
                | workingWidget =
                    model.workingWidget
                        |> Monocle.Optional.modify
                            columnsOptional
                            (\cs -> cs ++ [ Column "" SignupColumnText ])
              }
            , Cmd.none
            )

        RemoveSignupColumn idx ->
            let
                columnsOptional =
                    Monocle.Common.maybe
                        |> Monocle.Compose.optionalWithLens Monocle.Common.second
                        |> Monocle.Compose.optionalWithPrism widgetSignupPrism
                        |> Monocle.Compose.optionalWithLens signupModelColumns
            in
            ( { model
                | workingWidget =
                    model.workingWidget
                        |> Monocle.Optional.modify
                            columnsOptional
                            (\cs ->
                                cs
                                    |> List.indexedMap Tuple.pair
                                    |> List.filter (\( i, _ ) -> Index i /= idx)
                                    |> List.map Tuple.second
                            )
              }
            , Cmd.none
            )

        SignupColumnTextChanged (Index idx) str ->
            let
                columnOptional =
                    Monocle.Common.maybe
                        |> Monocle.Compose.optionalWithLens Monocle.Common.second
                        |> Monocle.Compose.optionalWithPrism widgetSignupPrism
                        |> Monocle.Compose.optionalWithLens signupModelColumns
                        |> Monocle.Compose.optionalWithOptional (Monocle.Common.list idx)
            in
            ( { model
                | workingWidget =
                    model.workingWidget
                        |> Monocle.Optional.modify
                            columnOptional
                            (\c -> { c | columnName = str })
              }
            , Cmd.none
            )

        SignupColumnTypeChanged (Index idx) ct ->
            let
                columnOptional =
                    Monocle.Common.maybe
                        |> Monocle.Compose.optionalWithLens Monocle.Common.second
                        |> Monocle.Compose.optionalWithPrism widgetSignupPrism
                        |> Monocle.Compose.optionalWithLens signupModelColumns
                        |> Monocle.Compose.optionalWithOptional (Monocle.Common.list idx)
            in
            ( { model
                | workingWidget =
                    model.workingWidget
                        |> Monocle.Optional.modify
                            columnOptional
                            (\c -> { c | columnType = ct })
              }
            , Cmd.none
            )

        AddWidget ->
            case model.workingWidget of
                Just ( idx, ww ) ->
                    let
                        widgetOptional =
                            Monocle.Compose.lensWithOptional (widgetRowsRow idx) sheetWidgetRowsLens
                    in
                    ( { model
                        | sheet =
                            model.sheet
                                |> Monocle.Optional.modify
                                    widgetOptional
                                    (\r -> r ++ [ ww ])
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if Maybe.Extra.isJust model.workingWidget then
        Browser.Events.onKeyUp
            (Decode.andThen
                (\s ->
                    case s of
                        "Escape" ->
                            Decode.succeed StopEditingWidget

                        _ ->
                            Decode.fail "I could not recognize the key pressed"
                )
                (Decode.field "key" Decode.string)
            )

    else
        Sub.none



-- VIEW


widgetTypes : List ( String, Widget )
widgetTypes =
    [ ( "Signup Form", SignUp { columns = [] } )
    , ( "Survey", Survey { answers = [], isAnonymous = False } )
    , ( "Voting Form", Upvotes { options = [] } )
    ]


isWidgetMatch : Maybe Widget -> Widget -> Bool
isWidgetMatch w w_ =
    case ( w, w_ ) of
        ( Just (SignUp _), SignUp _ ) ->
            True

        ( Just (Survey _), Survey _ ) ->
            True

        ( Just (Upvotes _), Upvotes _ ) ->
            True

        _ ->
            False


view : Model -> View Msg
view model =
    { title = "Signup | New"
    , body =
        [ Html.div
            [ Utils.classes
                [ "flex"
                , "flex-col"
                , "h-full"
                ]
            ]
            [ Html.div
                [ Utils.classes
                    [ "h-16"
                    , "bg-black"
                    , "text-white"
                    , "flex"
                    , "items-center"
                    , "p-4"
                    ]
                ]
                [ Html.a [ Html.Attributes.href (Route.toHref Route.Home_) ]
                    [ Html.text "< Home" ]
                ]
            , Html.div
                [ Utils.classes
                    [ "grow"
                    , "p-4"
                    , "group"
                    , "flex"
                    , "flex-col"
                    , "space-y-4"
                    ]
                ]
                [ Html.div
                    [ Utils.classes
                        [ "flex"
                        , "flex-col"
                        , "space-y-2"
                        ]
                    ]
                    (model.sheet.widgetRows
                        |> List.indexedMap viewWidgetRow
                    )
                , Html.button
                    [ Utils.classes
                        [ "group-hover:block"
                        , "rounded-full"
                        , "border"
                        , "h-8"
                        , "w-8"
                        , "hover:bg-gray-500"
                        , "hover:text-white"
                        , "transition-all"
                        ]
                    , onClick AddRow
                    ]
                    [ Html.text "+" ]
                ]
            ]
        , Html.div
            -- Modal
            [ Utils.classes
                [ "absolute"
                , "top-0"
                , "left-0"
                , "h-screen"
                , "w-screen"
                ]
            , Html.Attributes.classList
                [ ( "hidden"
                  , Maybe.Extra.isNothing model.workingWidget
                  )
                ]
            ]
            [ Html.div
                [ Utils.classes
                    [ "bg-gray-500/50"
                    , "h-full"
                    , "w-full"
                    , "flex"
                    , "justify-center"
                    ]
                ]
                [ Html.div
                    [ Utils.classes
                        [ "p-16"
                        , "flex"
                        , "flex-col"
                        , "space-y-2"
                        , "relative"
                        ]
                    , Html.Attributes.classList
                        [ ( "top-0", Maybe.Extra.isJust model.workingWidget )
                        , ( "-top-[40%]", Maybe.Extra.isNothing model.workingWidget )
                        ]
                    ]
                    [ Html.button
                        [ Utils.classes
                            [ "flex"
                            , "justify-end"
                            ]
                        , onClick StopEditingWidget
                        ]
                        [ Icons.close ]
                    , Html.div
                        [ Utils.classes
                            [ "p-4"
                            , "bg-white"
                            , "rounded-xl"
                            , "flex"
                            , "flex-col"
                            , "space-y-4"
                            ]
                        ]
                        [ Html.div
                            [ Utils.classes
                                [ "flex"
                                , "justify-center"
                                ]
                            ]
                            (widgetTypes
                                |> List.map
                                    (\( t, w ) ->
                                        Html.button
                                            [ Utils.classes
                                                [ "border"
                                                , "first:rounded-l-md"
                                                , "p-2"
                                                , "border-r-0"
                                                , "last:border-r"
                                                , "last:rounded-r-md"
                                                , "transition-all"
                                                ]
                                            , Html.Attributes.classList
                                                [ ( "bg-gray-500 text-white"
                                                  , isWidgetMatch (Maybe.map Tuple.second model.workingWidget) w
                                                  )
                                                ]
                                            , onClick (ChangeEditingWidget w)
                                            ]
                                            [ Html.text t ]
                                    )
                            )
                        , case model.workingWidget of
                            Nothing ->
                                Html.div [] []

                            Just ( _, SignUp mdl ) ->
                                Html.div []
                                    [ Views.Signup.defaultConfig
                                        |> Views.Helpers.editing
                                        |> Views.Signup.setNewColumnMsg NewSignupColumn
                                        |> Views.Signup.setRemoveColumnMsg RemoveSignupColumn
                                        |> Views.Signup.setColumnTextChangedMsg SignupColumnTextChanged
                                        |> Views.Signup.setColumnTypeChangeMsg SignupColumnTypeChanged
                                        |> Views.Signup.view mdl
                                    ]

                            Just ( idx, Survey mdl ) ->
                                Html.div [] [ Html.text "Survey" ]

                            Just ( idx, Upvotes mdl ) ->
                                Html.div [] [ Html.text "Upvotes" ]
                        ]
                    , Html.div
                        [ Utils.classes
                            [ "flex"
                            , "justify-end"
                            ]
                        ]
                        [ Html.button
                            [ Utils.classes
                                [ "bg-green-300"
                                , "text-black"
                                , "px-4"
                                , "py-2"
                                , "rounded-xl"
                                , "shadow-md"
                                , "hover:bg-green-600"
                                , "hover:text-white"
                                , "transition-colors"
                                ]
                            , onClick AddWidget
                            ]
                            [ Html.text "Ok" ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewWidgetRow : Int -> List Widget -> Html.Html Msg
viewWidgetRow idx widgets =
    Html.div
        [ Utils.classes
            [ "border-dashed"
            , "border"
            , "p-4"
            ]
        ]
        [ Html.button
            [ Utils.classes
                [ "group-hover:block"
                , "rounded-full"
                , "border"
                , "h-8"
                , "w-8"
                , "hover:bg-gray-500"
                , "hover:text-white"
                , "transition-all"
                ]
            , onClick (StartAddingWidget (Index idx))
            ]
            [ Html.text "+"
            ]
        ]
