{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "apie"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "console"
    , "effect"
    , "foreign"
    , "formatters"
    , "media-types"
    , "parsing"
    , "psci-support"
    , "uri"
    , "b64"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
