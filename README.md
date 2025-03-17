# STLC type inference

Type inference/reconstruction for the _Simply Typed Lambda Calculus_ (STLC), per
the _Types and Programming Languages_ (TAPL) book.

Note that the used calculus is actually System F, but the inference algorithm
only work on the STLC fragment (if any `TAbs` or `TApp` is encountered during
inference, an exception will be thrown); the inferred type is however a (rank 1
/ prenex) System F type.

See the [test file](test/Type.hs) for some examples; you can run the tests using
```
stack test
```