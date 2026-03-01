# Changelog

## semmcci 1.1.6

### Patch

- Addressed deprecation of
  [`lavaan::getCov()`](https://rdrr.io/pkg/lavaan/man/getCov.html) in
  tests.

## semmcci 1.1.5

CRAN release: 2025-10-19

### Patch

- Minor edits to methods.

## semmcci 1.1.4

CRAN release: 2024-03-17

### Patch

- Added the
  [`Func()`](https://github.com/jeksterslab/semmcci/reference/Func.md)
  and
  [`MCFunc()`](https://github.com/jeksterslab/semmcci/reference/MCFunc.md)
  functions.

## semmcci 1.1.3

CRAN release: 2023-10-14

### Patch

- Minor edits to setting seed.
- Added the
  [`MCGeneric()`](https://github.com/jeksterslab/semmcci/reference/MCGeneric.md)
  function.

## semmcci 1.1.2

CRAN release: 2023-08-12

### Patch

- Addressed M1Mac CRAN build issues.

## semmcci 1.1.1

### Patch

- Minor documentation edits.

## semmcci 1.1.0

### Minor

- Added the
  [`MCMI()`](https://github.com/jeksterslab/semmcci/reference/MCMI.md)
  function.

## semmcci 1.0.4

CRAN release: 2022-12-17

### Patch

- Minor refactoring of data generation functions.

## semmcci 1.0.3

CRAN release: 2022-10-16

### Patch

- Added `decomposition`, `pd`, and `tol` arguments in
  [`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md).

## semmcci 1.0.2

CRAN release: 2022-10-04

### Patch

- Random variates from the multivatiate normal distribution are
  generated using the Cholesky decomposition of the sampling
  variance-covariance matrix. Eigen decomposition is used when Cholesky
  decomposition fails.
- `NA` is returned if the calculation of the defined parameter fails in
  [`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md).
- `NA` is returned if standardization fails in
  [`MCStd()`](https://github.com/jeksterslab/semmcci/reference/MCStd.md).
- Added methods.

## semmcci 1.0.1

CRAN release: 2022-09-13

### Patch

- Initial CRAN release.

## semmcci 1.0.0

### Major

- Initial CRAN submission.
