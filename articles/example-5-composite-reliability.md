# Example 5: Composite Reliability

\
[`library`](https://rdrr.io/r/base/library.html)`(``package ``=`` ``semmcci``)`\
[`library`](https://rdrr.io/r/base/library.html)`(``package ``=`` ``lavaan``)`

In this example, the Monte Carlo method is used to generate confidence
intervals for composite reliability using the Holzinger and Swineford
(1939) data set.

\
`utils``::`[`data`](https://rdrr.io/r/utils/data.html)`(``HolzingerSwineford1939``, package ``=`` ``"lavaan"``)`

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

\
`model`` ``<-`` ``"`\
`  # fix latent variable variances to 1`\
`  eta1 ~~ 1 * eta1`\
`  eta2 ~~ 1 * eta2`\
`  eta3 ~~ 1 * eta3`\
`  # factor loadings`\
`  eta1 =~ NA * x1 + l11 * x1 + l12 * x2 + l13 * x3`\
`  eta2 =~ NA * x4 + l24 * x4 + l25 * x5 + l26 * x6`\
`  eta3 =~ NA * x7 + l37 * x7 + l38 * x8 + l39 * x9`\
`  # error variances`\
`  x1 ~~ t1 * x1`\
`  x2 ~~ t2 * x2`\
`  x3 ~~ t3 * x3`\
`  x4 ~~ t4 * x4`\
`  x5 ~~ t5 * x5`\
`  x6 ~~ t6 * x6`\
`  x7 ~~ t7 * x7`\
`  x8 ~~ t8 * x8`\
`  x9 ~~ t9 * x9`\
`  # composite reliability`\
`  omega1 := (l11 + l12 + l13)^2 / ((l11 + l12 + l13)^2 + (t1 + t2 + t3))`\
`  omega2 := (l24 + l25 + l26)^2 / ((l24 + l25 + l26)^2 + (t4 + t5 + t6))`\
`  omega3 := (l37 + l38 + l39)^2 / ((l37 + l38 + l39)^2 + (t7 + t8 + t9))`\
`"`

## Model Fitting

We can now fit the model using the
[`cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) function from
`lavaan`.

\
`fit`` ``<-`` ``lavaan``::`[`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html)`(``model ``=`` ``model``, data ``=`` ``HolzingerSwineford1939``)`

### Monte Carlo Confidence Intervals

The `fit` `lavaan` object can then be passed to the
[`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md) function
to generate Monte Carlo confidence intervals.

\
[`MC`](https://jeksterslab.github.io/semmcci/reference/MC.md)`(``lav ``=`` ``fit``, R ``=`` ``20000L``, alpha ``=`` ``0.05``)`\
`#> Monte Carlo Confidence Intervals`\
`#>               est     se     R   2.5%  97.5%`\
`#> eta1~~eta1 1.0000 0.0000 20000 1.0000 1.0000`\
`#> eta2~~eta2 1.0000 0.0000 20000 1.0000 1.0000`\
`#> eta3~~eta3 1.0000 0.0000 20000 1.0000 1.0000`\
`#> l11        0.8996 0.0807 20000 0.7399 1.0579`\
`#> l12        0.4979 0.0772 20000 0.3483 0.6510`\
`#> l13        0.6562 0.0746 20000 0.5101 0.8017`\
`#> l24        0.9897 0.0565 20000 0.8797 1.1006`\
`#> l25        1.1016 0.0626 20000 0.9788 1.2259`\
`#> l26        0.9166 0.0534 20000 0.8105 1.0207`\
`#> l37        0.6195 0.0694 20000 0.4832 0.7572`\
`#> l38        0.7309 0.0655 20000 0.6042 0.8596`\
`#> l39        0.6700 0.0655 20000 0.5423 0.7991`\
`#> t1         0.5491 0.1140 20000 0.3257 0.7733`\
`#> t2         1.1338 0.1032 20000 0.9320 1.3364`\
`#> t3         0.8443 0.0913 20000 0.6643 1.0231`\
`#> t4         0.3712 0.0474 20000 0.2781 0.4642`\
`#> t5         0.4463 0.0593 20000 0.3295 0.5625`\
`#> t6         0.3562 0.0431 20000 0.2722 0.4411`\
`#> t7         0.7994 0.0816 20000 0.6410 0.9556`\
`#> t8         0.4877 0.0737 20000 0.3427 0.6316`\
`#> t9         0.5661 0.0708 20000 0.4262 0.7026`\
`#> eta1~~eta2 0.4585 0.0640 20000 0.3339 0.5850`\
`#> eta1~~eta3 0.4705 0.0726 20000 0.3282 0.6125`\
`#> eta2~~eta3 0.2830 0.0692 20000 0.1471 0.4196`\
`#> omega1     0.6253 0.0364 20000 0.5487 0.6907`\
`#> omega2     0.8852 0.0117 20000 0.8599 0.9057`\
`#> omega3     0.6878 0.0312 20000 0.6223 0.7442`

## References

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
