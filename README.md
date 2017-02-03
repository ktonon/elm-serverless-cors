# elm-serverless-cors

[![serverless](http://public.serverless.com/badges/v3.svg)](http://www.serverless.com)
[![Elm Package](https://img.shields.io/badge/elm-1.0.0-blue.svg)](http://package.elm-lang.org/packages/ktonon/elm-serverless-cors/latest)
[![CircleCI](https://img.shields.io/circleci/project/github/ktonon/elm-serverless-cors.svg)](https://circleci.com/gh/ktonon/elm-serverless-cors)

This is [CORS][] middleware for [elm-serverless][].

There are two ways to use the middleware:

* use `cors` passing in a `Config` (likely decoded from JSON using `configDecoder`)
* call individual headers like `allowOrigin` and `allowMethods` separately

[CORS]:https://en.wikipedia.org/wiki/Cross-origin_resource_sharing
[elm-serverless]:http://package.elm-lang.org/packages/ktonon/elm-serverless/latest
