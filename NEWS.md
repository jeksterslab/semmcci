# semmcci 1.1.3

## Patch

* Minor edits to setting seed.
* Added the `MCGeneric()` function.

# semmcci 1.1.2

## Patch

* Addressed M1Mac CRAN build issues.

# semmcci 1.1.1

## Patch

* Minor documentation edits.

# semmcci 1.1.0

## Minor

* Added the `MCMI()` function.

# semmcci 1.0.4

## Patch

* Minor refactoring of data generation functions.

# semmcci 1.0.3

## Patch

* Added `decomposition`, `pd`, and `tol` arguments in `MC()`.

# semmcci 1.0.2

## Patch

* Random variates from the multivatiate normal distribution are generated using the Cholesky decomposition of the sampling variance-covariance matrix. Eigen decomposition is used when Cholesky decomposition fails.
* `NA` is returned if the calculation of the defined parameter fails in `MC()`.
* `NA` is returned if standardization fails in `MCStd()`.
* Added methods.

# semmcci 1.0.1

## Patch

* Initial CRAN release.

# semmcci 1.0.0

## Major

* Initial CRAN submission.
