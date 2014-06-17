LazyArmature
============

An animation application built in Haskell and C

[Inverse Kinematics](https://software.intel.com/en-us/articles/character-animation-skeletons-and-inverse-kinematics)
(not implemented)

##Requirements
- SDL2
- ghc
- [Parsec](http://www.haskell.org/haskellwiki/Parsec) (installed through [cabal](http://www.haskell.org/cabal/))

##Instructions
0. Make sure the requirements are correctly installed
1. Run `make`
2. `./LazyArmature examples/test.laf`
3. `test.laf` can be any LazyArmature file

##Project
- Not everything got implemented, but hey, that's life.
- [GitHub](https://github.com/wft/lazyarmature)
- credits to Will and Hunter
- Will wrote the engine in C, the interface with Haskell, and the
  Skeleton system
- Hunter implemented a totally awesome parser, used the skeleton
  system, wrote the command file syntax, and provided Haskell
  expertise
