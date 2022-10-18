{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, backend = "purerl"
, dependencies =
  [ "datetime"
  , "effect"
  , "either"
  , "erl-kernel"
  , "erl-maps"
  , "erl-process"
  , "erl-tuples"
  , "foreign"
  , "maybe"
  , "partial"
  , "prelude"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
