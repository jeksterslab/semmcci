## Release summary

This is a patch update (1.1.6 -> 1.1.7).

- `MC()`, `MCMI()`, `MCFunc()`, and `MCGeneric()` with `decomposition = "chol"` use the eigenvalue decomposition when `chol()` fails on a singular sampling variance-covariance matrix (for example, equality constraints).
- New internal `.MCApply()` makes `MC()` with defined parameters about 2.4 times faster; results are unchanged.
- Documentation and code style updates (named arguments, `pkg::fn()` calls, linked DOI in the README and vignette).
- The percentile confidence intervals use compiled quantiles (C++, `Rcpp` and `RcppArmadillo`), identical to `stats::quantile()`.

## Test environments

- Linux (x86_64, Apptainer image), R 4.6.2: `R CMD check --as-cran`, Status: OK.
- win-builder (R-devel): TODO (the maintainer runs this before submission).
- R-hub: TODO (the maintainer runs this before submission).

## R CMD check results

0 errors | 0 warnings | 0 notes (Linux, R 4.6.2). The win-builder and R-hub results go here.

## Reverse dependencies

- wsMed 1.2.2 (the only reverse dependency on CRAN): `R CMD check` with semmcci 1.1.7, Status: OK
  (2026-11-19; to be repeated with the final 1.1.7 before submission).
