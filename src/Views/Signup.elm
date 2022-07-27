module Views.Signup exposing (..)

import Data.Types exposing (Column, ColumnType(..), Index(..), SignupModel, WidgetMode(..), columnName, columnType)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Icons
import Maybe.Extra
import Utils


type alias Config msg =
    { mode : WidgetMode
    , newColumnMsg : Maybe msg
    , removeColumnMsg : Maybe (Index -> msg)
    , columnTextChangedMsg : Maybe (Index -> String -> msg)
    , columnTypeChangeMsg : Maybe (Index -> ColumnType -> msg)
    }


defaultConfig : Config msg
defaultConfig =
    { mode = ReadOnly
    , newColumnMsg = Nothing
    , removeColumnMsg = Nothing
    , columnTextChangedMsg = Nothing
    , columnTypeChangeMsg = Nothing
    }


setNewColumnMsg : msg -> Config msg -> Config msg
setNewColumnMsg msg cfg =
    { cfg | newColumnMsg = Just msg }


setRemoveColumnMsg : (Index -> msg) -> Config msg -> Config msg
setRemoveColumnMsg msg cfg =
    { cfg | removeColumnMsg = Just msg }


setColumnTextChangedMsg : (Index -> String -> msg) -> Config msg -> Config msg
setColumnTextChangedMsg msg cfg =
    { cfg | columnTextChangedMsg = Just msg }


setColumnTypeChangeMsg : (Index -> ColumnType -> msg) -> Config msg -> Config msg
setColumnTypeChangeMsg msg cfg =
    { cfg | columnTypeChangeMsg = Just msg }


columnTypeView : ColumnType -> Html msg
columnTypeView ct =
    case ct of
        SignupColumnText ->
            Html.div [] [ Html.text "SignupColumnText" ]

        SignupColumnRange s e ->
            Html.div [] [ Html.text "SignupColumnRange" ]

        SignupColumnNumber n ->
            Html.div [] [ Html.text "SignupColumnNumber" ]


columnTypeToString : ColumnType -> String
columnTypeToString ct =
    case ct of
        SignupColumnText ->
            "signupcolumntext"

        SignupColumnRange _ _ ->
            "signupcolumnrange"

        SignupColumnNumber _ ->
            "signupcolumnnumber"


stringToColumnType : String -> ColumnType
stringToColumnType s =
    case s of
        "signupcolumntext" ->
            SignupColumnText

        "signupcolumnrange" ->
            SignupColumnRange 1 10

        "signupcolumnnumber" ->
            SignupColumnNumber 1

        _ ->
            SignupColumnText


columnTypes : List ( String, ColumnType )
columnTypes =
    [ ( "Text Entry", SignupColumnText )
    , ( "Range", SignupColumnRange 1 10 )
    , ( "Number", SignupColumnNumber 1 )
    ]


columnTypeMatch : ColumnType -> ColumnType -> Bool
columnTypeMatch c1 c2 =
    case ( c1, c2 ) of
        ( SignupColumnText, SignupColumnText ) ->
            True

        ( SignupColumnRange _ _, SignupColumnRange _ _ ) ->
            True

        ( SignupColumnNumber _, SignupColumnNumber _ ) ->
            True

        _ ->
            False


columnView :
    Config msg
    -> Int
    -> Column
    -> Html msg
columnView { mode, removeColumnMsg, columnTextChangedMsg, columnTypeChangeMsg } idx { columnName, columnType } =
    Html.div []
        [ case mode of
            Editing ->
                Html.div [ Utils.classes [ "relative", "group" ] ]
                    [ Html.div [ Utils.classes [ "flex", "flex-col" ] ]
                        [ Html.input
                            (Maybe.Extra.values
                                [ Just (Html.Attributes.type_ "text")
                                , Just (Html.Attributes.value columnName)
                                , Just
                                    (Utils.classes
                                        [ "border"
                                        , "px-2"
                                        , "py-1"
                                        , "rounded-md"
                                        ]
                                    )
                                , Just
                                    (Html.Attributes.placeholder
                                        ("Column " ++ String.fromInt (idx + 1))
                                    )
                                , Maybe.map (\fn -> onInput (fn (Index idx)))
                                    columnTextChangedMsg
                                ]
                            )
                            []
                        , Html.div [ Utils.classes [ "w-full" ] ]
                            [ Html.select
                                (Maybe.Extra.values
                                    [ Just (Utils.classes [ "w-full" ])
                                    , columnTypeChangeMsg
                                        |> Maybe.map
                                            (\fn ->
                                                onInput (fn (Index idx) << stringToColumnType)
                                            )
                                    ]
                                )
                                (columnTypes
                                    |> List.map (\( t, v ) -> Html.option [ Html.Attributes.selected (columnTypeMatch columnType v), Html.Attributes.value (columnTypeToString v) ] [ Html.text t ])
                                )
                            , columnTypeView columnType
                            ]
                        ]
                    , Html.button
                        (Maybe.Extra.values
                            [ Just
                                (Utils.classes
                                    [ "absolute"
                                    , "right-0"
                                    , "top-[12%]"
                                    ]
                                )
                            , Maybe.map (\fn -> onClick (fn (Index idx)))
                                removeColumnMsg
                            ]
                        )
                        [ Html.i
                            [ Utils.classes
                                [ "text-red-300"
                                , "hover:text-red-600"
                                , "transition-colors"
                                ]
                            ]
                            [ Icons.trash ]
                        ]
                    ]

            FillingOut ->
                Debug.todo "branch 'FillingOut' not implemented"

            ReadOnly ->
                Debug.todo "branch 'ReadOnly' not implemented"
        ]


view : SignupModel -> Config msg -> Html msg
view mdl ({ newColumnMsg } as config) =
    Html.div
        [ Utils.classes
            [ "flex"
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
            [ Html.div [ Utils.classes [ "text-lg" ] ]
                [ Html.text "Columns" ]
            , Html.div
                [ Utils.classes
                    [ "flex"
                    , "space-x-2"
                    ]
                ]
                (List.indexedMap (columnView config) mdl.columns)
            ]
        , Html.button
            (Maybe.Extra.values
                [ Just
                    (Utils.classes
                        [ "rounded-full"
                        , "border"
                        , "h-8"
                        , "w-8"
                        , "hover:bg-gray-500"
                        , "hover:text-white"
                        , "transition-all"
                        ]
                    )
                , Maybe.map onClick newColumnMsg
                ]
            )
            [ Html.text "+"
            ]
        ]
