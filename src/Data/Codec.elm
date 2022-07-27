module Data.Codec exposing (..)

import Codec exposing (Codec)
import Data.Types exposing (Answer, AnswerType(..), Column, ColumnType(..), Sheet, SignupModel, SurveyModel, UpvoteModel, UpvoteOption, Widget(..))


sheetCodec : Codec Sheet
sheetCodec =
    Codec.object Sheet
        |> Codec.field "title" .title Codec.string
        |> Codec.field "slug" .slug Codec.string
        |> Codec.field "widgetRows" .widgetRows (Codec.list (Codec.list widgetCodec))
        |> Codec.buildObject


widgetCodec : Codec Widget
widgetCodec =
    Codec.custom
        (\signup survey upvotes value ->
            case value of
                SignUp mdl ->
                    signup mdl

                Survey mdl ->
                    survey mdl

                Upvotes mdl ->
                    upvotes mdl
        )
        |> Codec.variant1 "SignUp" SignUp signupModelCodec
        |> Codec.variant1 "Survey" Survey surveyModelCodec
        |> Codec.variant1 "Upvotes" Upvotes upvotesModelCodec
        |> Codec.buildCustom


signupModelCodec : Codec SignupModel
signupModelCodec =
    Codec.object SignupModel
        |> Codec.field "columns" .columns (Codec.list columnCodec)
        |> Codec.buildObject


columnCodec : Codec Column
columnCodec =
    Codec.object Column
        |> Codec.field "columnName" .columnName Codec.string
        |> Codec.field "columnType" .columnType columnTypeCodec
        |> Codec.buildObject


columnTypeCodec : Codec ColumnType
columnTypeCodec =
    Codec.custom
        (\text range number value ->
            case value of
                SignupColumnText ->
                    text

                SignupColumnRange min max ->
                    range min max

                SignupColumnNumber n ->
                    number n
        )
        |> Codec.variant0 "SignupColumnText" SignupColumnText
        |> Codec.variant2 "SignupColumnRange" SignupColumnRange Codec.int Codec.int
        |> Codec.variant1 "SignupColumnNumber" SignupColumnNumber Codec.int
        |> Codec.buildCustom


surveyModelCodec : Codec SurveyModel
surveyModelCodec =
    Codec.object SurveyModel
        |> Codec.field "answers" .answers (Codec.list answerCodec)
        |> Codec.field "isAnonymous" .isAnonymous Codec.bool
        |> Codec.buildObject


answerCodec : Codec Answer
answerCodec =
    Codec.object Answer
        |> Codec.field "description" .description Codec.string
        |> Codec.field "answerType" .answerType answerTypeCodec
        |> Codec.buildObject


answerTypeCodec : Codec AnswerType
answerTypeCodec =
    Codec.custom
        (\text predefined calendar value ->
            case value of
                SurveyAnswerText ->
                    text

                SurveyAnswerPredefined ans ->
                    predefined ans

                SurveyAnswerCalendar ->
                    calendar
        )
        |> Codec.variant0 "surveyAnswerText" SurveyAnswerText
        |> Codec.variant1 "surveyAnswerPredefined" SurveyAnswerPredefined (Codec.list Codec.string)
        |> Codec.variant0 "surveyAnswerCalendar" SurveyAnswerCalendar
        |> Codec.buildCustom


upvotesModelCodec : Codec UpvoteModel
upvotesModelCodec =
    Codec.object UpvoteModel
        |> Codec.field "options" .options (Codec.list upvoteOptionCodec)
        |> Codec.buildObject


upvoteOptionCodec : Codec UpvoteOption
upvoteOptionCodec =
    Codec.object UpvoteOption
        |> Codec.field "description" .description Codec.string
        |> Codec.buildObject
