module Views.Helpers exposing (..)

import Data.Types exposing (WidgetMode(..))


editing : { a | mode : WidgetMode } -> { a | mode : WidgetMode }
editing cfg =
    { cfg | mode = Editing }


readonly : { a | mode : WidgetMode } -> { a | mode : WidgetMode }
readonly cfg =
    { cfg | mode = ReadOnly }


fillingOut : { a | mode : WidgetMode } -> { a | mode : WidgetMode }
fillingOut cfg =
    { cfg | mode = FillingOut }
