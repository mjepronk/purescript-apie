{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "apie"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "b64"
    , "console"
    , "effect"
    , "foreign"
    , "formatters"
    , "media-types"
    , "parsing"
    , "psci-support"
    , "web-url"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
