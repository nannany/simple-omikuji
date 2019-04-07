module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Route exposing (Route)
import Url



-- Main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged -- この処理はURLが変わった直後に呼ばれる
        , onUrlRequest = LinkClicked -- この処理はページ遷移する前に必ず呼ばれる
        }



--model


type alias Model =
    { records : List Record
    , key : Nav.Key
    , url : Url.Url
    , page : Page
    }


type Page
    = NotFound
    | TopPage
    | ResultPage


type alias Record =
    { checked : Bool
    , name : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model [] key url TopPage
        |> goTo (Route.parse url)



--update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PlusClicked
    | ChangeName String Int -- チェックボックスのテキストを変更したときに発動
    | ChangeChecked Bool Int -- チェックボックスのチェックを変更したときに発動


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        PlusClicked ->
            ( { model | records = Record True "" :: model.records }, Cmd.none )

        ChangeName str index ->
            ( { model | records = alterRecordName str index model.records }, Cmd.none )

        ChangeChecked bool index ->
            ( { model | records = alterRecordChecked bool index model.records }, Cmd.none )


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



-- routing


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            ( { model | page = TopPage }, Cmd.none )

        Just Route.Result ->
            ( { model | page = ResultPage }, Cmd.none )



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--view


view : Model -> Browser.Document Msg
view model =
    { title = "omikuji"
    , body =
        case model.page of
            NotFound ->
                viewNotFound

            TopPage ->
                viewTopPage model

            ResultPage ->
                viewNotFound
    }


viewNotFound : List (Html Msg)
viewNotFound =
    [ h1 [ style "color" "red" ] [ text "404 Not Found!" ] ]


viewTopPage : Model -> List (Html Msg)
viewTopPage model =
    [ h1 [] [ text "omikuji application" ]
    , text "Click +, add user."
    , br [] []
    , button [ onClick PlusClicked ] [ text "+" ]
    , p [] (showList model.records)
    ]


showList : List Record -> List (Html Msg)
showList records =
    List.indexedMap (\i r -> checkbox r i) records


checkbox : Record -> Int -> Html Msg
checkbox record index =
    div []
        [ input [ type_ "checkbox", checked record.checked, onCheck (\b -> ChangeChecked b index) ] []
        , input [ type_ "text", placeholder "name", onInput (\s -> ChangeName s index) ] []
        ]