{ name = "selective-functors"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "functors"
  , "identity"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "run"
  , "st"
  , "transformers"
  , "tuples"
  , "uncurried-transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
