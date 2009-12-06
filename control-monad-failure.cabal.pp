name: control-monad-failure
version: 0.6.0
Cabal-Version:  >= 1.6
build-type: Simple
license: PublicDomain
author: Pepe Iborra, Michael Snoyman, Nicolas Pouillard
maintainer: pepeiborra@gmail.com
homepage: http://github.com/pepeiborra/control-monad-failure
description: A class for monads which can fail with an error.
synopsis: A class for monads which can fail with an error.
category: Control, Monads
stability: experimental

Library
  buildable: True
  build-depends: base >= 4 && < 5, failure, transformers
  ghc-options: -Wall

  extensions:  MultiParamTypeClasses, FlexibleInstances
  exposed-modules:
     Control.Monad.Failure
     Control.Monad.Failure.Transformers

source-repository head
  type:     git
  location: git://github.com/pepeiborra/control-monad-failure.git
