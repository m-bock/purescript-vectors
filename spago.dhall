{ name = "vectors"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "prelude"
  , "profunctor-lenses"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
