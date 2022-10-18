let base = ./spago.dhall

in    base
    ⫽ { sources =
          base.sources # [ "test/**/*.purs" ]
      , dependencies =
          base.dependencies # [ "assert", "erl-test-eunit", "free", "console", "erl-atom" ]
      }
