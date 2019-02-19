# Changelog for free-algebras

## Version 0.0.7.2
- Enhanced documentation, and properly attributed fix in ChangeLog.

## Version 0.0.7.1
- `Data.Group.Free.normalize` and `Data.Group.Free.normalizeL` are not both
  `O(n)`, but the former is implemented using the latter (e.g. transforms
  `DList` to a list), by Justin Le (https://github.com/mstsg)

## Version 0.0.7.0
- Poly kinded `Control.Algebra.Free.FreeAlgebra` and
  `Control.Algebra.Free2.FreeAlgebra2`
- removed actions (MSet, SSet), use `monoid-extras` or `semigroups-actions`
  packages

## Version 0.0.6.0
- `Num a => SSet (Sum a) a` and `Num a => SSet (Product a) a` instances
- `Num a => MSet (Sum a) a` and `Num a => MSet (Product a) a` instances
- removed some overlapping instances for `SSet` and `MSet`
- `Control.Algebra.Free2` module, see `free-category` package for applications.

## Version 0.0.5.1
- Improved documentation

## Version 0.0.5.0
- `FreeAlgebra` renamed `proof` to `codom`, added `Proof` smart constructor `proof`.
- `FreeAlgebra` instance for `Identity` functor
- generic `Free` type
- `DList` and `FreeGroup` based on `DList`, `FreeGroupL`
- documentation on strictness / laziness improved

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
