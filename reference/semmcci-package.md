# semmcci: Monte Carlo Confidence Intervals in Structural Equation Modeling

Generates Monte Carlo confidence intervals for free and defined
parameters in models fitted in the structural equation modeling package
`lavaan` (Pesigan & Cheung, 2024). A sampling distribution of the
parameter estimates is generated from the multivariate normal
distribution using the parameter estimates and their sampling covariance
matrix. Confidence limits are the percentiles of the generated
distribution (Preacher & Selig, 2012; MacKinnon et al., 2004).

## Models fitted in `lavaan`

[`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md)
generates Monte Carlo confidence intervals from a fitted `lavaan` model.
[`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md) does
the same for a `lavaan` model fitted to multiply imputed data.
[`MCStd()`](https://jeksterslab.github.io/semmcci/reference/MCStd.md)
generates Monte Carlo confidence intervals for the standardized
estimates from the output of
[`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md) or
[`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md).

## Other models

[`MCGeneric()`](https://jeksterslab.github.io/semmcci/reference/MCGeneric.md)
and
[`MCFunc()`](https://jeksterslab.github.io/semmcci/reference/MCFunc.md)
generate Monte Carlo confidence intervals for functions of parameters
when the estimates and their sampling covariance matrix are supplied.
[`Func()`](https://jeksterslab.github.io/semmcci/reference/Func.md) does
the same for a list of parameters.

## Methods

The [`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`print()`](https://rdrr.io/r/base/print.html) methods are available for
objects of class `semmcci`.

## References

MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence
limits for the indirect effect: Distribution of the product and
resampling methods. *Multivariate Behavioral Research*, *39*(1), 99–128.
[doi:10.1207/s15327906mbr3901_4](https://doi.org/10.1207/s15327906mbr3901_4)

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*.
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4)

Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo
confidence intervals for indirect effects. *Communication Methods and
Measures*, *6*(2), 77–98.
[doi:10.1080/19312458.2012.679848](https://doi.org/10.1080/19312458.2012.679848)

## See also

Useful links:

- <https://github.com/jeksterslab/semmcci>

- <https://jeksterslab.github.io/semmcci/>

- Report bugs at <https://github.com/jeksterslab/semmcci/issues>

## Author

**Maintainer**: Ivan Jacob Agaloos Pesigan <r.jeksterslab@gmail.com>
([ORCID](https://orcid.org/0000-0003-4818-8420)) \[copyright holder\]

Authors:

- Ivan Jacob Agaloos Pesigan <r.jeksterslab@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-4818-8420)) \[copyright holder\]

Other contributors:

- Shu Fai Cheung <shufai.cheung@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-9871-9448)) \[contributor\]
