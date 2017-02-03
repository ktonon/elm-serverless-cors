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

* use `cors` passing in a `Config` (likely decoded from JSON using `configDecoder`)
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


{-| Specify all CORS configuration in one record.
-}
type alias Config =
    { origin : Reflectable (List String)
    , expose : List String
    , maxAge : Int
    , credentials : Bool
    , methods : List Method
    , headers : Reflectable (List String)
    }


{-| A reflectable header value.

A reflectable value can either be

* `ReflectRequest` derive the headers from the request
* `Exactly` set to a specific value
-}
type Reflectable a
    = ReflectRequest
    | Exactly a



-- DECODERS


{-| Decode CORS configuration from JSON.
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


{-| Decode a reflectable value from JSON.

* `"*"` decodes to `ReflectRequest`
* `"foo,bar"` or `["foo", "bar"]` decodes to `Exactly ["foo", "bar"]`
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


{-| Case-insensitive decode a list of HTTP methods.
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


{-| Set CORS headers according to a configuration record.

This function is best used when the configuration is provided externally and
decoded using `configDecoder`. For example, npm rc and AWS Lambda environment
variables can be used as the source of CORS configuration.
-}
cors : Config -> Conn config model -> Conn config model
cors config =
    allowOrigin config.origin
        >> exposeHeaders config.expose
        >> maxAge config.maxAge
        >> allowCredentials config.credentials
        >> allowMethods config.methods
        >> allowHeaders config.headers


{-| Sets `access-control-allow-origin`.

`ReflectRequest` will reflect the request `origin` header, or if absent, will
just be set to `*`
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


{-| Sets `access-control-expose-headers`.
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


{-| Sets `access-control-max-age`.

If the value is not positive, the header will not be set.
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


{-| Sets `access-control-allow-credentials`.

Only sets the header if the value is `True`.
-}
allowCredentials : Bool -> Conn config model -> Conn config model
allowCredentials allow conn =
    if allow then
        conn |> header ( "access-control-allow-credentials", "true" )
    else
        conn


{-| Sets `access-control-allow-methods`.
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


{-| Sets `access-control-allow-headers`.

`ReflectRequest` will reflect the request `access-control-request-headers` headers
or if absent, it will not set the header at all.
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
            if h |> List.isEmpty then
                conn
            else
                conn
                    |> header
                        ( "access-control-allow-headers"
                        , h |> String.join ","
                        )
