module Page.Home exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Component.Checkbox
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Url
import Url.Builder exposing (relative)


type alias Model =
    { records : List Record
    , key : Nav.Key
    }


type alias Record =
    { checked : Bool
    , name : String
    }


type Msg
    = PlusClicked
    | ChangeName Int String -- チェックボックスのテキストを変更したときに発動
    | ChangeChecked Int Bool -- チェックボックスのチェックを変更したときに発動
    | ClickResult
    | GoToResult Time.Posix



-- このページの初期化処理


init : Maybe String -> Nav.Key -> ( Model, Cmd Msg )
init names key =
    let
        initRecordList =
            case names of
                Nothing ->
                    []

                Just str ->
                    String.split "," str
                        |> List.map (\s -> Record True s)
    in
    ( Model initRecordList key, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlusClicked ->
            ( { model
                | records = Record True "" :: model.records
              }
            , Cmd.none
            )

        ChangeName index str ->
            ( { model | records = alterRecordName str index model.records }, Cmd.none )

        ChangeChecked index bool ->
            ( { model | records = alterRecordChecked bool index model.records }, Cmd.none )

        ClickResult ->
            ( model, Task.perform GoToResult Time.now )

        GoToResult time ->
            ( model, Nav.pushUrl model.key (relative [ "result?names=" ++ getEffectiveNames model ++ "&seed=" ++ String.fromInt (Time.posixToMillis time) ] []) )


alterRecordName : String -> Int -> List Record -> List Record
alterRecordName str index targetLists =
    List.indexedMap
        (\i r ->
            if i == index then
                Record r.checked str

            else
                r
        )
        targetLists


alterRecordChecked : Bool -> Int -> List Record -> List Record
alterRecordChecked bool index targetLists =
    List.indexedMap
        (\i r ->
            if i == index then
                Record bool r.name

            else
                r
        )
        targetLists


getEffectiveNames : Model -> String
getEffectiveNames model =
    List.filter (\r -> r.checked) model.records
        |> List.map (\r -> r.name)
        |> String.join ","


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "omikuji application" ]
        , text "Click +, add user."
        , br [] []
        , button [ onClick PlusClicked ] [ text "+" ]
        , p [] (showList model.records)
        , button [ onClick ClickResult ] [ text "show result" ]
        ]


showList : List Record -> List (Html Msg)
showList records =
    List.indexedMap (\i r -> Component.Checkbox.view (ChangeChecked i) (ChangeName i) r.checked r.name) records
