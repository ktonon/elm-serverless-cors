module Tests exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect
import Expect.Extra as Expect exposing (regexPattern)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Serverless.Conn as Conn
import Serverless.Conn.Request as Request exposing (Method(..))
import Serverless.Cors exposing (..)
import Test exposing (..)
import Test.Extra exposing (..)


all : Test
all =
    describe "Serverless.Cors"
        [ decoderTests
        , middlewareTests
        ]


testHeader : String -> Conn -> List ( String, String ) -> Test
testHeader label conn headers =
    it (label ++ " (headers: " ++ (headers |> toString) ++ ")") <|
        let
            resp =
                conn
                    |> Conn.jsonEncodedResponse
                    |> encode 0
        in
        if List.isEmpty headers then
            Expect.match
                (regexPattern
                    """"headers":{"content-type":"text/text; charset=utf-8","cache-control":"max-age=0, private, must-revalidate"}"""
                )
                resp
        else
            Expect.all
                (headers
                    |> List.map
                        (\( key, val ) ->
                            Expect.match
                                (regexPattern <| "\"" ++ key ++ "\":\"" ++ val ++ "\"")
                        )
                )
                resp


middlewareTests : Test
middlewareTests =
    describe "CORS Middleware"
        [ describe "allowOrigin"
            [ testHeader "Reflect allow missing"
                (conn |> allowOrigin ReflectRequest)
                [ ( "access-control-allow-origin", "*" ) ]
            , testHeader "Reflect allow provided"
                (connWithHeader "origin" "foo.bar.ca"
                    |> allowOrigin ReflectRequest
                )
                [ ( "access-control-allow-origin", "foo.bar.ca" ) ]
            , testHeader "Allow exactly none"
                (conn
                    |> allowOrigin (Exactly [])
                )
                []
            , testHeader "Allow exactly one"
                (conn
                    |> allowOrigin (Exactly [ "foo.bar.com" ])
                )
                [ ( "access-control-allow-origin", "foo.bar.com" ) ]
            , testHeader "Reflect headers empty"
                (conn |> allowHeaders ReflectRequest)
                []
            , testHeader "Reflect headers one"
                (connWithHeader "access-control-request-headers" "foo,bar,car"
                    |> allowHeaders ReflectRequest
                )
                [ ( "access-control-allow-headers", "foo,bar,car" ) ]
            , testHeader "Allow methods"
                (conn |> allowMethods [ GET, OPTIONS ])
                [ ( "access-control-allow-methods", "GET,OPTIONS" ) ]
            , testHeader "Disallow credentials"
                (conn |> allowCredentials False)
                []
            , testHeader "Allow credentials"
                (conn |> allowCredentials True)
                [ ( "access-control-allow-credentials", "true" ) ]
            , testHeader "Negative max age"
                (conn |> maxAge -1)
                []
            , testHeader "Zero max age"
                (conn |> maxAge 0)
                []
            , testHeader "Positive max age"
                (conn |> maxAge 10)
                [ ( "access-control-max-age", "10" ) ]
            , testHeader "Expose headers"
                (conn
                    |> exposeHeaders [ "foo", "bar-car" ]
                )
                [ ( "access-control-expose-headers", "foo,bar-car" ) ]
            , testHeader "Expose no headers"
                (conn |> exposeHeaders [])
                []
            , testHeader "Null config"
                (conn |> cors nullConfig)
                []
            , testHeader "Load from config"
                (conn
                    |> cors
                        { nullConfig
                            | origin = ReflectRequest
                            , methods = [ GET, OPTIONS ]
                        }
                )
                [ ( "access-control-allow-methods", "GET,OPTIONS" )
                , ( "access-control-allow-origin", "*" )
                ]
            ]
        ]


decoderTests : Test
decoderTests =
    describe "Decoders"
        [ describeDecoder "configDecoder"
            configDecoder
            [ ( "{}", DecodesTo nullConfig )
            , ( """{ "origin": "*" }"""
              , DecodesTo { nullConfig | origin = ReflectRequest }
              )
            , ( """{ "origin": ["x.y.z", "a.b.c"] }"""
              , DecodesTo { nullConfig | origin = Exactly [ "x.y.z", "a.b.c" ] }
              )
            , ( """{ "origin": "x.y.z,a.b.c" }"""
              , DecodesTo { nullConfig | origin = Exactly [ "x.y.z", "a.b.c" ] }
              )
            , ( """{ "methods": ["get", "options"] }"""
              , DecodesTo { nullConfig | methods = [ GET, OPTIONS ] }
              )
            , ( """{ "methods": "get,options" }"""
              , DecodesTo { nullConfig | methods = [ GET, OPTIONS ] }
              )
            , ( """{ "expose": "foo-bar,car" }"""
              , DecodesTo { nullConfig | expose = [ "foo-bar", "car" ] }
              )
            , ( """{ "maxAge": 8 }"""
              , DecodesTo { nullConfig | maxAge = 8 }
              )
            , ( """{ "maxAge": "8" }"""
              , DecodesTo { nullConfig | maxAge = 8 }
              )
            , ( """{ "maxAge": "four" }"""
              , FailsToDecode
              )
            , ( """{ "maxAge": -1 }"""
              , FailsToDecode
              )
            , ( """{ "credentials": true }"""
              , DecodesTo { nullConfig | credentials = True }
              )
            , ( """{ "credentials": false }"""
              , DecodesTo nullConfig
              )
            , ( """{ "credentials": 1 }"""
              , DecodesTo { nullConfig | credentials = True }
              )
            , ( """{ "credentials": 0 }"""
              , DecodesTo nullConfig
              )
            , ( """{ "credentials": "" }"""
              , DecodesTo nullConfig
              )
            , ( """{ "credentials": "true" }"""
              , DecodesTo { nullConfig | credentials = True }
              )
            , ( """{ "headers": "*" }"""
              , DecodesTo { nullConfig | headers = ReflectRequest }
              )
            , ( """{ "headers": ["foo-bar", "car"] }"""
              , DecodesTo { nullConfig | headers = Exactly [ "foo-bar", "car" ] }
              )
            , ( """{ "headers": "foo-bar,car" }"""
              , DecodesTo { nullConfig | headers = Exactly [ "foo-bar", "car" ] }
              )
            ]
        , describeDecoder "methodsDecoder"
            methodsDecoder
            [ ( "[]", DecodesTo [] )
            , ( "\"get\"", DecodesTo [ GET ] )
            , ( "\"post\"", DecodesTo [ POST ] )
            , ( "\"put\"", DecodesTo [ PUT ] )
            , ( "\"delete\"", DecodesTo [ DELETE ] )
            , ( "\"options\"", DecodesTo [ OPTIONS ] )
            , ( "\"GeT\"", DecodesTo [ GET ] )
            , ( "\"get,post\"", DecodesTo [ GET, POST ] )
            , ( "[\"get\", \"post\"]", DecodesTo [ GET, POST ] )
            ]
        , describeDecoder "reflectableDecoder"
            reflectableDecoder
            [ ( "[]", DecodesTo (Exactly []) )
            , ( "\"*\"", DecodesTo ReflectRequest )
            , ( "\"x.y.z\"", DecodesTo (Exactly [ "x.y.z" ]) )
            , ( "\"x.y.z,a.b.c\"", DecodesTo (Exactly [ "x.y.z", "a.b.c" ]) )
            , ( "[\"x.y.z\", \"a.b.c\"]", DecodesTo (Exactly [ "x.y.z", "a.b.c" ]) )
            ]
        ]



-- HELPERS


nullConfig : Config
nullConfig =
    Config
        (Exactly [])
        []
        0
        False
        []
        (Exactly [])


type alias Conn =
    Conn.Conn Config () () ()


connWithHeader : String -> String -> Conn
connWithHeader key value =
    case
        decodeString Request.decoder
            ("""{
  "body": "null",
  "headers": {"""
                ++ "\""
                ++ key
                ++ "\":\""
                ++ value
                ++ "\""
                ++ """},
  "host": "localhost",
  "method": "GET",
  "path": "/",
  "port": 3000,
  "queryParams": null,
  "queryString": "?null=null",
  "remoteIp": "127.0.0.1",
  "scheme": "http",
  "stage": "dev"
}
"""
            )
    of
        Ok req ->
            Conn.init "id" nullConfig () () req

        Err err ->
            Debug.crash "Failed to create conn with header"


conn : Conn
conn =
    Conn.init "id" nullConfig () () Request.init
