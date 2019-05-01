module Route exposing (Route(..), fromUrl, parser, replaceUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing ((<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q


type Route
    = Home (Maybe String)
    | OmikujiResult (Maybe String) (Maybe Int)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Debug.log "url"
        |> Url.Parser.parse parser


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home (top <?> Q.string "names")
        , map OmikujiResult (s "result" <?> Q.string "names" <?> Q.int "seed")
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home names ->
                    [ "?names=", Maybe.withDefault "" names ]

                OmikujiResult names seed ->
                    [ "result?names=", Maybe.withDefault "" names, "&seed=", String.fromInt (Maybe.withDefault 0 seed) ]
    in
    "#/" ++ String.join "" pieces
