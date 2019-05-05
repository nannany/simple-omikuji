module RoutingTests exposing (suite)

import Expect exposing (Expectation)
import Route exposing (Route, fromUrl)
import Test exposing (..)
import Url exposing (Url)


suite : Test
suite =
    describe "Route"
        [ test "should parse URL to home without names" <|
            \_ ->
                Url.fromString "http://test.com/"
                    |> Maybe.andThen Route.fromUrl
                    |> Expect.equal (Just (Route.Home Nothing))
        , test "should parse URL to home with names" <|
            \_ ->
                Url.fromString "http://test.com/#/?names=nannany,n1"
                    |> Maybe.andThen Route.fromUrl
                    |> Expect.equal (Just (Route.Home (Just "nannany,n1")))
        , test "should parse URL to result" <|
            \_ ->
                Url.fromString "http://test.com/#/result?names=nannany,n1&seed=1"
                    |> Maybe.andThen Route.fromUrl
                    |> Expect.equal (Just (Route.OmikujiResult (Just "nannany,n1") (Just 1)))
        ]
