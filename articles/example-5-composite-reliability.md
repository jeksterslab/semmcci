# Example 5: Composite Reliability

``` r

library(semmcci)
library(lavaan)
```

In this example, the Monte Carlo method is used to generate confidence
intervals for composite reliability using the Holzinger and Swineford
(1939) data set.

``` r

data(HolzingerSwineford1939, package = "lavaan")
```

The confirmatory factor analysis model for $`X_{1}, \dots, X_{9}`$ is
given by

![Three-Factor Confirmatory Factor Analysis
Model](https://raw.githubusercontent.com/jeksterslab/semmcci/main/.setup/latex/figures/png/cfa.png)

Three-Factor Confirmatory Factor Analysis Model

$`\eta_{1}`$, $`\eta_{2}`$, and $`\eta_{3}`$ are the latent factors.
$`\eta_{1}`$ has three indicators $`X_{1}`$, $`X_{2}`$, and $`X_{3}`$;
$`\eta_{2}`$ has three indicators $`X_{4}`$, $`X_{5}`$, and $`X_{6}`$;
and $`\eta_{3}`$ has three indicators $`X_{7}`$, $`X_{8}`$, and
$`X_{9}`$ . The variances of $`\eta_{1}`$, $`\eta_{2}`$, and
$`\eta_{3}`$ are constrained to one.

## Model Specification

Assuming that the latent variable variance is constrained to one, the
omega total reliability coefficient is given by

``` math
  \omega_{\mathrm{total}}
  =
  \frac{
  \left(
  \sum_{i = 1}^{k}
  \lambda_{i}
  \right)^2
  }{
  \left(
  \sum_{i = 1}^{k}
  \lambda_{i}
  \right)^2
  +
  \sum_{i = 1}^{k}
  \theta_{\varepsilon_{ii}}
  }
```

where $`\lambda_{i}`$ is the factor loading for item $`i`$,
$`\theta_{\varepsilon_{ii}}`$ is the residual variance for item $`i`$,
and $`k`$ is the number of items for a particular latent variable.

In the model specification below, the variances of the latent variables
`eta1`, `eta2`, and `eta3` are constrained to one, all the relevant
parameters are labeled particularly the factor loadings and the error
variances, and the omega total reliability coefficient per latent
variable are defined using the `:=` operator.

``` r

model <- "
  # fix latent variable variances to 1
  eta1 ~~ 1 * eta1
  eta2 ~~ 1 * eta2
  eta3 ~~ 1 * eta3
  # factor loadings
  eta1 =~ NA * x1 + l11 * x1 + l12 * x2 + l13 * x3
  eta2 =~ NA * x4 + l24 * x4 + l25 * x5 + l26 * x6
  eta3 =~ NA * x7 + l37 * x7 + l38 * x8 + l39 * x9
  # error variances
  x1 ~~ t1 * x1
  x2 ~~ t2 * x2
  x3 ~~ t3 * x3
  x4 ~~ t4 * x4
  x5 ~~ t5 * x5
  x6 ~~ t6 * x6
  x7 ~~ t7 * x7
  x8 ~~ t8 * x8
  x9 ~~ t9 * x9
  # composite reliability
  omega1 := (l11 + l12 + l13)^2 / ((l11 + l12 + l13)^2 + (t1 + t2 + t3))
  omega2 := (l24 + l25 + l26)^2 / ((l24 + l25 + l26)^2 + (t4 + t5 + t6))
  omega3 := (l37 + l38 + l39)^2 / ((l37 + l38 + l39)^2 + (t7 + t8 + t9))
"
```

## Model Fitting

We can now fit the model using the
[`cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) function from
`lavaan`.

``` r

fit <- cfa(model = model, data = HolzingerSwineford1939)
```

### Monte Carlo Confidence Intervals

The `fit` `lavaan` object can then be passed to the
[`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md)
function to generate Monte Carlo confidence intervals.

``` r

MC(fit, R = 20000L, alpha = 0.05)
#> Monte Carlo Confidence Intervals
#>               est     se     R   2.5%  97.5%
#> eta1~~eta1 1.0000 0.0000 20000 1.0000 1.0000
#> eta2~~eta2 1.0000 0.0000 20000 1.0000 1.0000
#> eta3~~eta3 1.0000 0.0000 20000 1.0000 1.0000
#> l11        0.8996 0.0806 20000 0.7390 1.0564
#> l12        0.4979 0.0770 20000 0.3475 0.6498
#> l13        0.6562 0.0745 20000 0.5090 0.8032
#> l24        0.9897 0.0566 20000 0.8783 1.0991
#> l25        1.1016 0.0626 20000 0.9783 1.2240
#> l26        0.9166 0.0533 20000 0.8118 1.0234
#> l37        0.6195 0.0693 20000 0.4827 0.7562
#> l38        0.7309 0.0654 20000 0.6029 0.8596
#> l39        0.6700 0.0655 20000 0.5426 0.7987
#> t1         0.5491 0.1140 20000 0.3266 0.7750
#> t2         1.1338 0.1032 20000 0.9323 1.3370
#> t3         0.8443 0.0916 20000 0.6644 1.0253
#> t4         0.3712 0.0480 20000 0.2758 0.4644
#> t5         0.4463 0.0592 20000 0.3307 0.5633
#> t6         0.3562 0.0425 20000 0.2733 0.4394
#> t7         0.7994 0.0816 20000 0.6407 0.9561
#> t8         0.4877 0.0737 20000 0.3427 0.6324
#> t9         0.5661 0.0708 20000 0.4255 0.7035
#> eta1~~eta2 0.4585 0.0641 20000 0.3349 0.5855
#> eta1~~eta3 0.4705 0.0728 20000 0.3274 0.6127
#> eta2~~eta3 0.2830 0.0688 20000 0.1478 0.4182
#> omega1     0.6253 0.0363 20000 0.5489 0.6910
#> omega2     0.8852 0.0117 20000 0.8597 0.9058
#> omega3     0.6878 0.0312 20000 0.6214 0.7438
```

## References

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
