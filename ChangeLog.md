# Changelog for free-algebras

## Version 0.0.4.0

- `S` new type wrapper in `SSet`, and overlappable instance for
  `SSet s a => SSet (S s) (Endo a)`
- `foldrFree` and friends
- `foldrMSet`
- documented intersection with `monad-mmorph` package.

## Version 0.0.3.0

- Breaking change: changed proofs in FreeAlgebra and FreeAlgebra1 class; now
  using `FreeAlgebra` and `FreeAlgebra1` classes let us deduce an adjunction.
- Monadicity example

## Version 0.0.2.0
- Simplified `Proof` type.

## Unreleased changes
