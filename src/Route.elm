module Route exposing (Route(..), parse, parser)

import Url exposing (Url)
import Url.Parser exposing ((<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q


type Route
    = Home (Maybe String)
    | OmikujiResult (Maybe String) (Maybe Int)


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home (top <?> Q.string "names")
        , map OmikujiResult (s "result" <?> Q.string "names" <?> Q.int "seed")
        ]
