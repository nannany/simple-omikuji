module Component.Checkbox exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : (Bool -> msg) -> (String -> msg) -> Bool -> String -> Html msg
view checkedMsg nameMsg isChecked name =
    div []
        [ input [ type_ "checkbox", checked isChecked, onCheck checkedMsg ] []
        , input [ class "is-info is-small", type_ "text", placeholder "name", onInput nameMsg, value name ] []
        ]
