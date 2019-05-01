module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Component.Checkbox
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Page.Home
import Page.NotFound
import Page.OmikujiResult
import Random exposing (Seed, initialSeed)
import Random.List exposing (shuffle)
import Route exposing (Route, fromUrl, replaceUrl)
import Task
import Time exposing (..)
import Url
import Url.Builder exposing (relative)



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
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | HomePage Page.Home.Model
    | OmikujiResultPage Page.OmikujiResult.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key NotFound
        |> Debug.log "model"
        |> goTo (Route.fromUrl (Debug.log "org" url))



--update


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Page.Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- URLが変更された直後に実行される
        UrlChanged url ->
            goTo (Route.fromUrl url) model

        -- Home ページのメッセージが来た時
        HomeMsg homeMsg ->
            -- 現在表示しているページで場合分け
            case model.page of
                HomePage homeModel ->
                    let
                        ( newHomeModel, homeCmd ) =
                            Page.Home.update homeMsg homeModel
                    in
                    ( { model | page = HomePage newHomeModel }
                    , Cmd.map HomeMsg homeCmd
                    )

                _ ->
                    ( model, Cmd.none )



-- routing
-- ここで初期化処理を走らせる


goTo : Maybe Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case Debug.log "maybeRoute" maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just (Route.Home names) ->
            let
                ( homeModel, homeCmd ) =
                    Page.Home.init names model.key
            in
            ( { model | page = HomePage homeModel }, Cmd.map HomeMsg homeCmd )

        Just (Route.OmikujiResult names seed) ->
            let
                ( omikujiResultModel, omikujiResultCmd ) =
                    Page.OmikujiResult.init names seed
            in
            ( { model | page = OmikujiResultPage omikujiResultModel }, omikujiResultCmd )



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--view


view : Model -> Browser.Document Msg
view model =
    { title = "omikuji"
    , body =
        [ case model.page of
            NotFound ->
                Page.NotFound.view

            HomePage homePageModel ->
                Page.Home.view homePageModel
                    |> Html.map HomeMsg

            OmikujiResultPage omikujiResultPageModel ->
                Page.OmikujiResult.view omikujiResultPageModel
        ]
    }
