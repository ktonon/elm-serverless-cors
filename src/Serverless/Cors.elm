module Serverless.Cors exposing (..)

{-| This small module contains a single function for enabling CORS.

@docs cors
-}

import Serverless.Conn as Conn
import Serverless.Conn.Types exposing (Method)
import Serverless.Types exposing (Conn)


{-| Add cors headers to the response.

    pipeline
        |> plug (cors "*" [ GET, OPTIONS ])
-}
cors : String -> List Method -> Conn config model -> Conn config model
cors origin methods =
    (Conn.header ( "access-control-allow-origin", origin ))
        >> (Conn.header
                ( "access-control-allow-headers"
                , methods
                    |> List.map toString
                    |> String.join ", "
                )
           )
