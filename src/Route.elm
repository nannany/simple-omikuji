module Route exposing (Route(..), parse, parser)

import Url exposing (Url)
import Url.Parser exposing ((<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q


type Route
    = Top
    | Result (Maybe Int)


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        , map Result (s "result" <?> Q.int "seed")
        ]
