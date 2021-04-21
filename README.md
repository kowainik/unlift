# unlift

[![GitHub CI](https://github.com/kowainik/unlift/workflows/CI/badge.svg)](https://github.com/kowainik/unlift/actions)
[![Hackage](https://img.shields.io/hackage/v/unlift.svg?logo=haskell)](https://hackage.haskell.org/package/unlift)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Typeclass for monads that can be unlifted to arbitrary base monads:

```haskell
class (MonadBase b m) => MonadUnlift b m
  where
    withRunInBase :: ((forall a . m a -> b a) -> b x) -> m x
```

This typeclass is helpful when writing code that is polymorphic over the base
monad, so later you can select a different base monad for each specific use-case.

Common usages include:

1. Concurrent testing with [dejafu](https://hackage.haskell.org/package/dejafu).
2. Working with code over polymorphic [PrimMonad](https://hackage.haskell.org/package/primitive-0.7.1.0/docs/Control-Monad-Primitive.html#t:PrimMonad).
