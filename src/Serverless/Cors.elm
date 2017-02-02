module Serverless.Cors
    exposing
        ( Config
        , Reflectable(..)
        , configDecoder
        , methodsDecoder
        , reflectableDecoder
        , cors
        , allowOrigin
        , exposeHeaders
        , maxAge
        , allowCredentials
        , allowMethods
        , allowHeaders
        )

{-| CORS Middleware for elm-serverless.

There are two ways to use the middleware:

* use `cors` passing in a `Config` (likely decoded from JSON using configDecoder)
* call individual headers like `allowOrigin` and `allowMethods` separately

## Types

@docs Config, Reflectable

## Decoders

@docs configDecoder, methodsDecoder, reflectableDecoder

## Middleware

@docs cors, allowOrigin, exposeHeaders, maxAge, allowCredentials, allowMethods, allowHeaders

-}

import Dict
import Json.Decode exposing (andThen, bool, fail, int, list, oneOf, string, succeed, Decoder)
import Json.Decode.Pipeline exposing (decode, optional)
import Serverless.Conn exposing (..)
import Serverless.Conn.Types exposing (..)
import Serverless.Types exposing (Conn)
import Toolkit.Helpers exposing (maybeList)


-- TYPES


{-|
-}
type alias Config =
    { origin : Reflectable (List String)
    , expose : List String
    , maxAge : Int
    , credentials : Bool
    , methods : List Method
    , headers : Reflectable (List String)
    }


{-|
-}
type Reflectable a
    = ReflectRequest
    | Exactly a



-- DECODERS


{-|
-}
configDecoder : Decoder Config
configDecoder =
    decode Config
        |> optional "origin" reflectableDecoder (Exactly [])
        |> optional "expose" stringListDecoder []
        |> optional "maxAge" positiveIntDecoder 0
        |> optional "credentials" truthyDecoder False
        |> optional "methods" methodsDecoder []
        |> optional "headers" reflectableDecoder (Exactly [])


{-|
-}
reflectableDecoder : Decoder (Reflectable (List String))
reflectableDecoder =
    stringListDecoder
        |> andThen
            (\strings ->
                if strings == [ "*" ] then
                    succeed ReflectRequest
                else
                    strings |> Exactly |> succeed
            )


positiveIntDecoder : Decoder Int
positiveIntDecoder =
    int
        |> andThen
            (\val ->
                if val < 0 then
                    fail "negative value when zero or positive was expected"
                else
                    succeed val
            )


truthyDecoder : Decoder Bool
truthyDecoder =
    oneOf
        [ bool
        , int |> andThen (\val -> val /= 0 |> succeed)
        , string |> andThen (\val -> not (String.isEmpty val) |> succeed)
        ]


stringListDecoder : Decoder (List String)
stringListDecoder =
    oneOf
        [ list string
        , string |> andThen (String.split "," >> succeed)
        ]


{-|
-}
methodsDecoder : Decoder (List Method)
methodsDecoder =
    stringListDecoder
        |> andThen
            (\strings ->
                case
                    strings
                        |> List.map
                            (\w ->
                                case w |> String.toLower of
                                    "get" ->
                                        Just GET

                                    "post" ->
                                        Just POST

                                    "put" ->
                                        Just PUT

                                    "delete" ->
                                        Just DELETE

                                    "options" ->
                                        Just OPTIONS

                                    _ ->
                                        Nothing
                            )
                        |> maybeList
                of
                    Just methods ->
                        succeed methods

                    Nothing ->
                        fail
                            ("Invalid CORS methods: "
                                ++ (strings |> String.join ",")
                            )
            )



-- MIDDLEWARE


{-|
-}
cors : Config -> Conn config model -> Conn config model
cors config =
    allowOrigin config.origin
        >> exposeHeaders config.expose
        >> maxAge config.maxAge
        >> allowCredentials config.credentials
        >> allowMethods config.methods
        >> allowHeaders config.headers


{-|
-}
allowOrigin : Reflectable (List String) -> Conn config model -> Conn config model
allowOrigin origin conn =
    case origin of
        ReflectRequest ->
            conn
                |> header
                    ( "access-control-allow-origin"
                    , conn.req.headers
                        |> Dict.fromList
                        |> Dict.get "origin"
                        |> Maybe.withDefault "*"
                    )

        Exactly origins ->
            if origins |> List.isEmpty then
                conn
            else
                conn
                    |> header
                        ( "access-control-allow-origin"
                        , origins |> String.join ","
                        )


{-|
-}
exposeHeaders : List String -> Conn config model -> Conn config model
exposeHeaders headers conn =
    if headers |> List.isEmpty then
        conn
    else
        conn
            |> header
                ( "access-control-expose-headers"
                , headers |> String.join ","
                )


{-|
-}
maxAge : Int -> Conn config model -> Conn config model
maxAge age conn =
    if age > 0 then
        conn
            |> header
                ( "access-control-max-age"
                , age |> toString
                )
    else
        conn


{-|
-}
allowCredentials : Bool -> Conn config model -> Conn config model
allowCredentials allow conn =
    if allow then
        conn |> header ( "access-control-allow-credentials", "true" )
    else
        conn


{-|
-}
allowMethods : List Method -> Conn config model -> Conn config model
allowMethods methods conn =
    if methods |> List.isEmpty then
        conn
    else
        conn
            |> header
                ( "access-control-allow-methods"
                , methods |> List.map toString |> String.join ","
                )


{-|
-}
allowHeaders : Reflectable (List String) -> Conn config model -> Conn config model
allowHeaders headers conn =
    case headers of
        ReflectRequest ->
            case
                conn.req.headers
                    |> Dict.fromList
                    |> Dict.get "access-control-request-headers"
            of
                Just requestHeaders ->
                    conn
                        |> header
                            ( "access-control-allow-headers"
                            , requestHeaders
                            )

                Nothing ->
                    conn

        Exactly h ->
            conn
                |> header
                    ( "access-control-allow-headers"
                    , h |> String.join ","
                    )
