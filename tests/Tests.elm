module Tests exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Serverless.Conn.Types exposing (..)
import Serverless.Cors exposing (..)
import Serverless.Types exposing (PipelineState(..), Sendable(..))
import Test exposing (..)
import Test.Extra exposing (..)


all : Test
all =
    describe "Serverless.Cors"
        [ decoderTests
        , middlewareTests
        ]


testHeader : Conn -> List ( String, String ) -> Test
testHeader conn headers =
    it ("headers: " ++ (headers |> toString)) <|
        case conn.resp of
            Unsent resp ->
                expect
                    resp.headers
                    to
                    equal
                    headers

            Sent ->
                fail ""


middlewareTests : Test
middlewareTests =
    describe "CORS Middleware"
        [ describe "allowOrigin"
            [ testHeader
                (conn |> allowOrigin ReflectRequest)
                [ ( "access-control-allow-origin", "*" ) ]
            , testHeader
                (connWithHeader "origin" "foo.bar.com"
                    |> allowOrigin ReflectRequest
                )
                [ ( "access-control-allow-origin", "foo.bar.com" ) ]
            , testHeader
                (conn
                    |> allowOrigin (Exactly [])
                )
                []
            , testHeader
                (conn
                    |> allowOrigin (Exactly [ "foo.bar.com" ])
                )
                [ ( "access-control-allow-origin", "foo.bar.com" ) ]
            , testHeader
                (conn |> allowHeaders ReflectRequest)
                []
            , testHeader
                (connWithHeader "access-control-request-headers" "foo,bar,car"
                    |> allowHeaders ReflectRequest
                )
                [ ( "access-control-allow-headers", "foo,bar,car" ) ]
            , testHeader
                (conn |> allowMethods [ GET, OPTIONS ])
                [ ( "access-control-allow-methods", "GET,OPTIONS" ) ]
            , testHeader
                (conn |> allowCredentials False)
                []
            , testHeader
                (conn |> allowCredentials True)
                [ ( "access-control-allow-credentials", "true" ) ]
            , testHeader
                (conn |> maxAge -1)
                []
            , testHeader
                (conn |> maxAge 0)
                []
            , testHeader
                (conn |> maxAge 10)
                [ ( "access-control-max-age", "10" ) ]
            , testHeader
                (conn
                    |> exposeHeaders [ "foo", "bar-car" ]
                )
                [ ( "access-control-expose-headers", "foo,bar-car" ) ]
            , testHeader
                (conn |> exposeHeaders [])
                []
            , testHeader
                (conn |> cors nullConfig)
                []
            , testHeader
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
    Serverless.Types.Conn Bool Bool


connWithHeader : String -> String -> Conn
connWithHeader key value =
    let
        req =
            conn.req
    in
        { conn | req = { req | headers = [ ( key, value ) ] } }


conn : Conn
conn =
    Serverless.Types.Conn
        Processing
        False
        (Request
            ""
            NoBody
            []
            ""
            GET
            ""
            80
            (Ip4 ( 127, 0, 0, 1 ))
            (Http Insecure)
            ""
            []
        )
        (Unsent
            (Response
                NoBody
                Utf8
                []
                InvalidStatus
            )
        )
        False
