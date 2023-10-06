# purescript-selective-functors

This is a purescript implementation of Selective Applicative Functors, described
in [this paper](https://eprints.ncl.ac.uk/file_store/production/258640/4FF2555F-0AEC-4876-9701-C83A3E5FFF52.pdf).

`Selective` is split into `Select` and `Selective`, where `Select` is
`Selective` without `pure`, to fit in more with the purescript ecosystem. So,
`Select` is to `Selective` as `Apply` is to `Applicative`, and `Bind` is to
`Monad`.

Ideally, `Monad` would be a subclass of `Selective`, because every `Monad` is
a `Selective`, but this is not `prelude`, so we can't do that. Instead, we
provide a wrapper, `SelectM`, which is a newtype, which when it wraps
a `Monad`, is also `Selective`. This library also provides a lot of `Selective`
instances for popular monads.
