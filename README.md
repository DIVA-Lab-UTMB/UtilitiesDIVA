
<!-- README.md is generated from README.Rmd. Please edit that file -->

# UtilitiesDIVA

<!-- badges: start -->
<!-- badges: end -->

This package contains a collection of functions commonly used in UTMB
DIVA lab, including utility functions for clustering and calculation of
modularity of a bipartite network (a more user-friendly wrapper of
[BipartiteModularityMaximization](https://cran.r-project.org/package=BipartiteModularityMaximization)),
network null models, significance of modularity, and network
visualization functions (a more user-friendly wrapper of
[ExplodeLayout](https://cran.r-project.org/package=ExplodeLayout) ).

## Installation

You can install the development version of UtilitiesDIVA like so:

``` r
install.packages("remotes")
remotes::install_github("DIVA-Lab-UTMB/UtilitiesDIVA")
```

## Example

This is a basic example which shows you how to solve a common problem
(Note that both functions below are based on files with a certain format
and naming convention):

``` r
## Load library
library(UtilitiesDIVA)

## Find clustering, modularity and significance of a bipartite network represented as an incidence matrix stored in "example_data.csv". This function should be run only once for each network.
utility_ModSig(fname = 'example', signif_preserve = 'col_deg', signif_N_sample = 1000)

## Network visualization using ExplodeLayout. Users can run this function multiple times for same network to explore different radius.
utility_EL(fname = 'example', radius = 1.2, plotlabel = 'c')
```

## Documentation

Please read the documentation using `?utility_ModSig`, `?utility_EL` for
more details such as naming convention.
