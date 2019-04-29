module Component.Checkbox exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : (Bool -> msg) -> (String -> msg) -> Bool -> Html msg
view checkedMsg nameMsg isChecked =
    div []
        [ input [ type_ "checkbox", checked isChecked, onCheck checkedMsg ] []
        , input [ type_ "text", placeholder "name", onInput nameMsg ] []
        ]
