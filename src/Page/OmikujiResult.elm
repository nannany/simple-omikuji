port module Page.OmikujiResult exposing (Model, Msg, copy, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed)
import Random.List exposing (shuffle)


type alias Model =
    { roles : List String
    , names : List String
    }



-- init でshuffleする必要あり


init : Maybe String -> Maybe Int -> ( Model, Cmd msg )
init names seed =
    let
        initialNames =
            case names of
                Nothing ->
                    []

                Just str ->
                    String.split "," str

        basicRoles =
            [ "S", "P" ]

        initialRoles =
            makeInitialRoles basicRoles (List.length initialNames - List.length basicRoles)
                |> shuffleRole seed
    in
    ( Model initialRoles initialNames, Cmd.none )


makeInitialRoles : List String -> Int -> List String
makeInitialRoles roles n =
    if n == 0 then
        roles

    else
        makeInitialRoles ("D" :: roles) (n - 1)


shuffleRole : Maybe Int -> List String -> List String
shuffleRole seed srcList =
    Random.step (shuffle srcList) (initialSeed (Maybe.withDefault 0 seed))
        |> Tuple.first



--update


type Msg
    = Copy



-- JavaScriptにクリップボードへのコピーを依頼するコマンド


port copy : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Copy ->
            let
                targetStr =
                    List.map2 Tuple.pair model.names model.roles
                        |> List.map (\t -> "* " ++ Tuple.first t ++ ": " ++ Tuple.second t)
                        |> String.join "\r\n"
            in
            ( model, copy targetStr )



-- clipboard copyとかすることになればupdate書く必要あり。
--view


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Result" ]
            , br [] []
            , table [ class "table is-bordered is-hoverable is-striped" ]
                (showTableData model.names model.roles)
            , button [ class "button is-info", onClick Copy ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-clipboard" ] [] ]
                ]
            ]
        ]



-- resultページにて結果の表を表示するためのメソッド


showTableData : List String -> List String -> List (Html msg)
showTableData nameList roleList =
    List.map2 Tuple.pair nameList roleList
        |> List.map (\t -> tr [] [ td [] [ text (Tuple.first t) ], td [] [ text (Tuple.second t) ] ])
        |> (::) (tr [] [ th [] [ text "name" ], th [] [ text "role" ] ])
