module Data.Types exposing (..)

import Monocle.Lens exposing (Lens)
import Monocle.Prism exposing (Prism)


type Index
    = Index Int


type ChoiceType
    = Single
    | Multiple


type ColumnType
    = SignupColumnText
    | SignupColumnRange Int Int
    | SignupColumnNumber
    | SignupColumnChoice ChoiceType (List String)


type alias Column =
    { columnName : String
    , columnType : ColumnType
    }


columnName : Lens Column String
columnName =
    Lens .columnName (\n c -> { c | columnName = n })


columnType : Lens Column ColumnType
columnType =
    Lens .columnType (\t c -> { c | columnType = t })


type alias SignupModel =
    { columns : List Column }


signupModelColumns : Lens SignupModel (List Column)
signupModelColumns =
    Lens .columns (\cs mdl -> { mdl | columns = cs })


type AnswerType
    = SurveyAnswerText
    | SurveyAnswerPredefined (List String)
    | SurveyAnswerCalendar


type alias Answer =
    { description : String
    , answerType : AnswerType
    }


type alias SurveyModel =
    { answers : List Answer
    , isAnonymous : Bool
    }


type alias UpvoteOption =
    { description : String
    }


type alias UpvoteModel =
    { options : List UpvoteOption
    }


type Widget
    = SignUp SignupModel
    | Survey SurveyModel
    | Upvotes UpvoteModel


widgetSignupPrism : Prism Widget SignupModel
widgetSignupPrism =
    { getOption =
        \a ->
            case a of
                SignUp mdl ->
                    Just mdl

                _ ->
                    Nothing
    , reverseGet = SignUp
    }


widgetSurveyPrism : Prism Widget SurveyModel
widgetSurveyPrism =
    { getOption =
        \a ->
            case a of
                Survey mdl ->
                    Just mdl

                _ ->
                    Nothing
    , reverseGet = Survey
    }


widgetUpvotesPrism : Prism Widget UpvoteModel
widgetUpvotesPrism =
    { getOption =
        \a ->
            case a of
                Upvotes mdl ->
                    Just mdl

                _ ->
                    Nothing
    , reverseGet = Upvotes
    }


type alias Sheet =
    { title : String
    , slug : String
    , widgetRows : List (List Widget)
    }


type WidgetMode
    = Editing
    | FillingOut
    | ReadOnly
