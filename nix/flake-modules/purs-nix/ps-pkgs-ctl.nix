with builtins;
{ ps-pkgs, ctl-rev, ... }:
self:
with (removeAttrs ps-pkgs (attrNames self));
{
  aeson =
    {
      src.git =
        {
          repo = "https://github.com/mlabs-haskell/purescript-aeson.git";
          rev = "286862a975f4bafbef15540c365bbbb0480e0bf7";
          ref = "master";
        };
      info =
        {
          version = "1.0.0";

          dependencies =
            [
              self.aeson
              self.aeson-helpers
              aff
              aff-promise
              aff-retry
              affjax
              arraybuffer-types
              arrays
              bifunctors
              bigints
              checked-exceptions
              console
              const
              contravariant
              control
              datetime
              debug
              effect
              either
              encoding
              enums
              exceptions
              foldable-traversable
              foreign
              foreign-object
              http-methods
              identity
              integers
              js-date
              self.lattice
              lists
              maybe
              self.medea
              media-types
              monad-logger
              self.mote
              newtype
              node-buffer
              node-child-process
              node-fs
              node-fs-aff
              node-path
              node-process
              node-streams
              nonempty
              now
              numbers
              optparse
              ordered-collections
              orders
              parallel
              partial
              posix-types
              prelude
              profunctor
              profunctor-lenses
              self.toppokki
              quickcheck
              quickcheck-combinators
              quickcheck-laws
              rationals
              record
              refs
              safe-coerce
              self.sequences
              spec
              spec-quickcheck
              strings
              stringutils
              tailrec
              text-encoding
              these
              transformers
              tuples
              typelevel
              typelevel-prelude
              uint
              undefined
              unfoldable
              untagged-union
              variant
              heterogeneous
            ];
        };
    };

  aeson-helpers =
    {
      src.git =
        {
          repo = "https://github.com/mlabs-haskell/purescript-bridge-aeson-helpers.git";
          rev = "44d0dae060cf78babd4534320192b58c16a6f45b";
          ref = "main";
        };

      info =
        {
          dependencies =
            [
              aff
              argonaut-codecs
              argonaut-core
              arrays
              bifunctors
              contravariant
              control
              effect
              either
              enums
              foldable-traversable
              foreign-object
              maybe
              newtype
              ordered-collections
              prelude
              profunctor
              psci-support
              quickcheck
              record
              spec
              spec-quickcheck
              transformers
              tuples
              typelevel-prelude
            ];
        };
    };

  cardano-transaction-lib =
    {
      src.git =
        {
          repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git";
          ref = "develop";
          rev = ctl-rev;
        };

      info =
        {
          dependencies =
            [
              self.aeson
              self.aeson-helpers
              aff
              aff-promise
              aff-retry
              affjax
              arraybuffer-types
              arrays
              bifunctors
              bigints
              checked-exceptions
              console
              const
              contravariant
              control
              datetime
              debug
              effect
              either
              encoding
              enums
              exceptions
              foldable-traversable
              foreign
              foreign-object
              http-methods
              identity
              integers
              js-date
              self.lattice
              lists
              maybe
              self.medea
              media-types
              monad-logger
              self.mote
              newtype
              node-buffer
              node-child-process
              node-fs
              node-fs-aff
              node-path
              node-process
              node-streams
              nonempty
              optparse
              now
              numbers
              ordered-collections
              orders
              parallel
              partial
              posix-types
              prelude
              profunctor
              profunctor-lenses
              self.toppokki
              quickcheck
              quickcheck-combinators
              quickcheck-laws
              rationals
              record
              refs
              spec
              spec-quickcheck
              strings
              tailrec
              text-encoding
              these
              transformers
              tuples
              typelevel
              typelevel-prelude
              uint
              undefined
              unfoldable
              untagged-union
              variant
              stringutils
            ];
        };
    };

  lattice =
    {
      src.git =
        {
          repo = "https://github.com/Risto-Stevcev/purescript-lattice.git";
          rev = "aebe3686eba30f199d17964bfa892f0176c1742d";
        };

      info =
        {
          version = "0.3.0";
          dependencies = [ prelude console self.properties ];
        };
    };

  medea =
    {
      src.git =
        {
          repo = "https://github.com/juspay/medea-ps.git";
          rev = "8b215851959aa8bbf33e6708df6bd683c89d1a5a";
        };

      info =
        {
          dependencies =
            [
              aff
              argonaut
              arrays
              bifunctors
              control
              effect
              either
              enums
              exceptions
              foldable-traversable
              foreign-object
              free
              integers
              lists
              maybe
              self.mote
              naturals
              newtype
              node-buffer
              node-fs-aff
              node-path
              nonempty
              ordered-collections
              parsing
              partial
              prelude
              psci-support
              quickcheck
              quickcheck-combinators
              safely
              spec
              strings
              these
              transformers
              typelevel
              tuples
              unicode
              unordered-collections
              unsafe-coerce
            ];
        };
    };

  mote =
    {
      src.git =
        {
          repo = "https://github.com/garyb/purescript-mote.git";
          rev = "29aea4ad7b013d50b42629c87b01cf0202451abd";
        };

      info =
        {
          version = "1.1.0";
          dependencies = [ these transformers arrays ];
        };
    };

  properties =
    {
      src.git =
        {
          repo = "https://github.com/Risto-Stevcev/purescript-properties.git";
          rev = "ddcad0f6043cc665037538467a2e2e4173ef276a";
        };
      info =
        {
          version = "0.2.0";
          dependencies = [ prelude console ];
        };
    };

  sequences =
    {
      src.git =
        {
          repo = "https://github.com/hdgarrood/purescript-sequences.git";
          rev = "1f1d828ef30070569c812d0af23eb7253bb1e990";
        };

      info =
        {
          version = "3.0.2";

          dependencies =
            [
              arrays
              ps-pkgs."assert"
              console
              effect
              lazy
              maybe
              newtype
              nonempty
              partial
              prelude
              profunctor
              psci-support
              quickcheck
              quickcheck-laws
              tuples
              unfoldable
              unsafe-coerce
            ];
        };
    };

  toppokki =
    {
      src.git =
        {
          repo = "https://github.com/firefrorefiddle/purescript-toppokki";
          rev = "6983e07bf0aa55ab779bcef12df3df339a2b5bd9";
          ref = "mike/browserpages";
        };

      info =
        {
          dependencies =
            [
              aff-promise
              functions
              node-buffer
              node-http
              prelude
              record
            ];
        };
    };
}
