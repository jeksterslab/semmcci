# Changelog

## semmcci 1.1.7

### Patch

- Documentation: the README and vignette cite Pesigan and Cheung (2024)
  with a linked DOI; the vignette math uses `\operatorname{}` and
  `\boldsymbol{}`; `NEWS.md` bullets use `-`.
- [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md),
  [`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md),
  [`MCFunc()`](https://jeksterslab.github.io/semmcci/reference/MCFunc.md),
  and
  [`MCGeneric()`](https://jeksterslab.github.io/semmcci/reference/MCGeneric.md)
  with `decomposition = "chol"` use the eigenvalue decomposition when
  [`chol()`](https://rdrr.io/r/base/chol.html) fails on a singular
  sampling variance-covariance matrix (for example, equality
  constraints); the tests of the equality-constrained model failed with
  newer `lavaan` and BLAS.
- Lint: named arguments and `pkg::fn()` calls in the R sources and
  tests; `# nolint` comments name a rule and a reason.
- Efficiency: new internal `.MCApply()` runs the per-draw functions of
  [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md)
  (defined parameters) and
  [`MCStd()`](https://jeksterslab.github.io/semmcci/reference/MCStd.md)
  under one [`tryCatch()`](https://rdrr.io/r/base/conditions.html), with
  an exact per-draw fallback.
  [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md) is
  about 2.4 times faster at R = 20000 for a model with defined
  parameters;
  [`MCStd()`](https://jeksterslab.github.io/semmcci/reference/MCStd.md)
  about 30% faster. Results are unchanged.
- The percentile intervals use C++ quantiles (`.Quantile()`, from
  nBootstrap 0.1.0), identical to `stats::quantile(type = 7)`; semmcci
  now links to `Rcpp` and `RcppArmadillo`.
- Internal helper copies refreshed from lavHelper 0.0.2, nBootstrap
  0.1.0, and miHelper 0.1.0.
- Internal `.ThetaHatStarScreen()` (used by betaMC through processLM)
  returns the counts of the screened draws as the attribute `"screen"`:
  requested, retained, rejected, redraws, and exhausted (dropped after
  `iter` failed redraws). The redraws use the same `pd` and `tol` as the
  first pass. The draws are unchanged.

## semmcci 1.1.6

CRAN release: 2026-06-12

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
  [`Func()`](https://jeksterslab.github.io/semmcci/reference/Func.md)
  and
  [`MCFunc()`](https://jeksterslab.github.io/semmcci/reference/MCFunc.md)
  functions.

## semmcci 1.1.3

CRAN release: 2023-10-14

### Patch

- Minor edits to setting seed.
- Added the
  [`MCGeneric()`](https://jeksterslab.github.io/semmcci/reference/MCGeneric.md)
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
  [`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md)
  function.

## semmcci 1.0.4

CRAN release: 2022-12-17

### Patch

- Minor refactoring of data generation functions.

## semmcci 1.0.3

CRAN release: 2022-10-16

### Patch

- Added `decomposition`, `pd`, and `tol` arguments in
  [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md).

## semmcci 1.0.2

CRAN release: 2022-10-04

### Patch

- Random variates from the multivatiate normal distribution are
  generated using the Cholesky decomposition of the sampling
  variance-covariance matrix. Eigen decomposition is used when Cholesky
  decomposition fails.
- `NA` is returned if the calculation of the defined parameter fails in
  [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md).
- `NA` is returned if standardization fails in
  [`MCStd()`](https://jeksterslab.github.io/semmcci/reference/MCStd.md).
- Added methods.

## semmcci 1.0.1

CRAN release: 2022-09-13

### Patch

- Initial CRAN release.

## semmcci 1.0.0

### Major

- Initial CRAN submission.
