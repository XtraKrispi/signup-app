module Views.Signup exposing (..)

import Data.Types exposing (ChoiceType(..), Column, ColumnType(..), Index(..), SignupModel, WidgetMode(..), columnName, columnType)
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


columnTypeEditView : Maybe (ColumnType -> msg) -> ColumnType -> Html msg
columnTypeEditView onChange ct =
    case ct of
        SignupColumnText ->
            Html.div [] []

        SignupColumnRange s e ->
            Html.div
                [ Utils.classes
                    [ "flex"
                    , "flex-col"
                    , "space-y-2"
                    ]
                ]
                [ Html.div
                    [ Utils.classes
                        []
                    ]
                    [ Html.label
                        [ Utils.classes
                            [ "block"
                            , "mb-2"
                            , "text-sm"
                            , "font-medium"
                            , "text-gray-900"
                            ]
                        ]
                        [ Html.text ("From: " ++ String.fromInt s) ]
                    , Html.input
                        (Maybe.Extra.values
                            [ Just <|
                                Utils.classes
                                    [ "w-full"
                                    , "h-2"
                                    , "bg-gray-200"
                                    , "rounded-lg"
                                    , "appearance-none"
                                    , "cursor-pointer"
                                    ]
                            , Just <| Html.Attributes.placeholder "Min"
                            , Just <| Html.Attributes.value (String.fromInt s)
                            , Just <| Html.Attributes.type_ "range"
                            , Just <| Html.Attributes.min "0"
                            , Just <| Html.Attributes.max "100"
                            , Maybe.map
                                (\fn ->
                                    onInput
                                        (\str ->
                                            fn
                                                (SignupColumnRange
                                                    (str
                                                        |> String.toInt
                                                        |> Maybe.withDefault s
                                                    )
                                                    e
                                                )
                                        )
                                )
                                onChange
                            ]
                        )
                        []
                    ]
                , Html.div
                    [ Utils.classes
                        []
                    ]
                    [ Html.label
                        [ Utils.classes
                            [ "block"
                            , "mb-2"
                            , "text-sm"
                            , "font-medium"
                            , "text-gray-900"
                            ]
                        ]
                        [ Html.text ("To: " ++ String.fromInt e) ]
                    , Html.input
                        (Maybe.Extra.values
                            [ Just <|
                                Utils.classes
                                    [ "w-full"
                                    , "h-2"
                                    , "bg-gray-200"
                                    , "rounded-lg"
                                    , "appearance-none"
                                    , "cursor-pointer"
                                    ]
                            , Just <| Html.Attributes.placeholder "Max"
                            , Just <| Html.Attributes.value (String.fromInt e)
                            , Just <| Html.Attributes.type_ "range"
                            , Just <| Html.Attributes.min "0"
                            , Just <| Html.Attributes.max "100"
                            , Maybe.map
                                (\fn ->
                                    onInput
                                        (\str ->
                                            fn
                                                (SignupColumnRange
                                                    s
                                                    (str
                                                        |> String.toInt
                                                        |> Maybe.withDefault e
                                                    )
                                                )
                                        )
                                )
                                onChange
                            ]
                        )
                        []
                    ]
                ]

        SignupColumnNumber ->
            Html.div [] []

        SignupColumnChoice choiceType choices ->
            Html.div
                [ Utils.classes
                    [ "flex"
                    , "flex-col"
                    , "space-y-2"
                    ]
                ]
                (List.concat
                    [ []
                    , choices
                        |> List.indexedMap
                            (\i c ->
                                Html.div []
                                    [ Html.input
                                        (Maybe.Extra.values
                                            [ Just <| Html.Attributes.value c
                                            , Just <|
                                                Utils.classes
                                                    textStyles
                                            , onChange
                                                |> Maybe.map
                                                    (\fn ->
                                                        onInput
                                                            (\s ->
                                                                choices
                                                                    |> List.indexedMap
                                                                        (\i_ c_ ->
                                                                            if i == i_ then
                                                                                s

                                                                            else
                                                                                c_
                                                                        )
                                                                    |> SignupColumnChoice choiceType
                                                                    |> fn
                                                            )
                                                    )
                                            ]
                                        )
                                        []
                                    ]
                            )
                    , [ Html.button
                            (Maybe.Extra.values
                                [ Maybe.map
                                    (\fn ->
                                        onClick
                                            ((choices ++ [ "" ])
                                                |> SignupColumnChoice choiceType
                                                |> fn
                                            )
                                    )
                                    onChange
                                ]
                            )
                            [ Html.text "+ Option" ]
                      ]
                    ]
                )


columnTypeToString : ColumnType -> String
columnTypeToString ct =
    case ct of
        SignupColumnText ->
            "signupcolumntext"

        SignupColumnRange _ _ ->
            "signupcolumnrange"

        SignupColumnNumber ->
            "signupcolumnnumber"

        SignupColumnChoice _ _ ->
            "signupcolumnchoice"


stringToColumnType : String -> ColumnType
stringToColumnType s =
    case s of
        "signupcolumntext" ->
            SignupColumnText

        "signupcolumnrange" ->
            SignupColumnRange 1 10

        "signupcolumnnumber" ->
            SignupColumnNumber

        "signupcolumnchoice" ->
            SignupColumnChoice Single []

        _ ->
            SignupColumnText


columnTypes : List ( String, ColumnType )
columnTypes =
    [ ( "Text Entry", SignupColumnText )
    , ( "Range", SignupColumnRange 1 10 )
    , ( "Number", SignupColumnNumber )
    , ( "Choice", SignupColumnChoice Single [] )
    ]


columnTypeMatch : ColumnType -> ColumnType -> Bool
columnTypeMatch c1 c2 =
    case ( c1, c2 ) of
        ( SignupColumnText, SignupColumnText ) ->
            True

        ( SignupColumnRange _ _, SignupColumnRange _ _ ) ->
            True

        ( SignupColumnNumber, SignupColumnNumber ) ->
            True

        ( SignupColumnChoice _ _, SignupColumnChoice _ _ ) ->
            True

        _ ->
            False


textStyles =
    [ "bg-gray-50"
    , "border"
    , "border-gray-300"
    , "text-gray-900"
    , "text-sm"
    , "rounded-lg"
    , "focus:ring-blue-500"
    , "focus:border-blue-500"
    , "block"
    , "w-full"
    , "p-2.5"
    ]


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
                    [ Html.div
                        [ Utils.classes
                            [ "flex"
                            , "flex-col"
                            , "space-y-2"
                            ]
                        ]
                        [ Html.div [ Utils.classes [ "flex" ] ]
                            [ Html.input
                                (Maybe.Extra.values
                                    [ Just (Html.Attributes.type_ "text")
                                    , Just (Html.Attributes.value columnName)
                                    , Just
                                        (Utils.classes
                                            textStyles
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
                            , Html.button
                                (Maybe.Extra.values
                                    [ Just
                                        (Utils.classes
                                            []
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
                        , Html.div [ Utils.classes [ "w-full", "flex", "flex-col", "space-y-2" ] ]
                            [ Html.select
                                (Maybe.Extra.values
                                    [ Just
                                        (Utils.classes
                                            [ "block"
                                            , "py-2.5"
                                            , "px-0"
                                            , "w-full"
                                            , "text-sm"
                                            , "text-gray-500"
                                            , "bg-transparent"
                                            , "border-0"
                                            , "border-b-2"
                                            , "border-gray-200"
                                            , "focus:outline-none"
                                            , "focus:ring-0"
                                            , "focus:border-gray-200"
                                            , "peer"
                                            ]
                                        )
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
                            , columnTypeEditView (Maybe.map (\fn -> fn (Index idx)) columnTypeChangeMsg) columnType
                            ]
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
