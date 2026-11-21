# semmcci 1.1.7

## Patch

- Documentation: the README and vignette cite Pesigan and Cheung (2024) with a linked DOI; the vignette math uses `\operatorname{}` and `\boldsymbol{}`; `NEWS.md` bullets use `-`.
- `MC()`, `MCMI()`, `MCFunc()`, and `MCGeneric()` with `decomposition = "chol"` use the eigenvalue decomposition when `chol()` fails on a singular sampling variance-covariance matrix (for example, equality constraints); the tests of the equality-constrained model failed with newer `lavaan` and BLAS.
- Lint: named arguments and `pkg::fn()` calls in the R sources and tests; `# nolint` comments name a rule and a reason.
- Efficiency: new internal `.MCApply()` runs the per-draw functions of `MC()` (defined parameters) and `MCStd()` under one
  `tryCatch()`, with an exact per-draw fallback.
  `MC()` is about 2.4 times faster at R = 20000 for a model with defined parameters; `MCStd()` about 30% faster. Results are unchanged.
- The percentile intervals use C++ quantiles (`.Quantile()`, from nBootstrap 0.1.0), identical to `stats::quantile(type = 7)`;
  semmcci now links to `Rcpp` and `RcppArmadillo`.
- Internal helper copies refreshed from lavHelper 0.0.2, nBootstrap 0.1.0, and miHelper 0.1.0.

# semmcci 1.1.6

## Patch

- Addressed deprecation of `lavaan::getCov()` in tests.

# semmcci 1.1.5

## Patch

- Minor edits to methods.

# semmcci 1.1.4

## Patch

- Added the `Func()` and `MCFunc()` functions.

# semmcci 1.1.3

## Patch

- Minor edits to setting seed.
- Added the `MCGeneric()` function.

# semmcci 1.1.2

## Patch

- Addressed M1Mac CRAN build issues.

# semmcci 1.1.1

## Patch

- Minor documentation edits.

# semmcci 1.1.0

## Minor

- Added the `MCMI()` function.

# semmcci 1.0.4

## Patch

- Minor refactoring of data generation functions.

# semmcci 1.0.3

## Patch

- Added `decomposition`, `pd`, and `tol` arguments in `MC()`.

# semmcci 1.0.2

## Patch

- Random variates from the multivatiate normal distribution are generated using the Cholesky decomposition of the sampling variance-covariance matrix. Eigen decomposition is used when Cholesky decomposition fails.
- `NA` is returned if the calculation of the defined parameter fails in `MC()`.
- `NA` is returned if standardization fails in `MCStd()`.
- Added methods.

# semmcci 1.0.1

## Patch

- Initial CRAN release.

# semmcci 1.0.0

## Major

- Initial CRAN submission.
