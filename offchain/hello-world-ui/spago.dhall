{ name = "hell/-world-ui"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-core"
  , "effect"
  , "either"
  , "halogen"
  , "http-methods"
  , "maybe"
  , "prelude"
  , "remotedata"
  , "transformers"
  , "type-equality"
  ]
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
