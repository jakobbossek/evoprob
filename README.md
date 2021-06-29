
# evoprob: Evolve instances for optimization problems

<!-- badges: start -->

[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/evoprob)](http://cran.r-project.org/web/packages/evoprob)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/evoprob)](http://cran.rstudio.com/web/packages/evoprob/index.html)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/evoprob?color=orange)](http://cran.rstudio.com/web/packages/evoprob/index.html)
[![R-CMD-check](https://github.com/jakobbossek/evoprob/workflows/R-CMD-check/badge.svg)](https://github.com/jakobbossek/evoprob/actions)
[![Codecov test
coverage](https://codecov.io/gh/jakobbossek/evoprob/branch/main/graph/badge.svg)](https://codecov.io/gh/jakobbossek/evoprob?branch=main)
<!-- badges: end -->

## What is this all about?

Contains an Evolutionary Algorithm which aims for the generation of a
diverse set of instances for combinatorial optimization problems for
algorithms. Diverse means that the EA strives for an instance set that
covers all possible rankings of the algorithms on the evolved problems
uniformly.

## Installation Instructions

The package will be available at [CRAN](http://cran.r-project.org) when
it is done. Install the developer version using
[devtools](https://github.com/hadley/devtools) package and type the
following command in R:

``` r
devtools::install_github("jakobbossek/evoprob")
```

## Contact

Please address questions and missing features about the **re** to the
author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please
use the [issue tracker](https://github.com/jakobbossek/evoprob/issues)
for this. Pay attention to explain the problem as good as possible. At
its best you provide an example, so I can reproduce your problem
quickly.
