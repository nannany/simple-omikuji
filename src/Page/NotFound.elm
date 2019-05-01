module Page.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    div []
        [ h1 [ style "color" "red" ] [ text "404 Not Found!" ] ]
