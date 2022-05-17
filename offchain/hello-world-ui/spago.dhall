{ name = "hell/-world-ui"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "effect"
  , "either"
  , "foreign-object"
  , "halogen"
  , "http-methods"
  , "maybe"
  , "prelude"
  , "remotedata"
  , "transformers"
  , "tuples"
  , "type-equality"
  ]
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
