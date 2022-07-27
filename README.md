
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

## Find clustering and modularity of a bipartite network represented as an incidence matrix stored in "example_data.csv". This function should be run only once for each network.
utility_Mod(
  inputName_data = "example_data.csv",
  outputName_nodelist = "example_nodelist.csv",
  outputName_incidmat = "example_incidmat.csv",
  outputName_modularity = "example_modularity.csv"
)

## Find significance of modularity of a bipartite network represented as an incidence matrix.
utility_ModSig(
  inputName_modularity = "example_modularity.csv",
  inputName_incidmat = "example_incidmat.csv",
  signif_preserve = "col_deg",
  signif_N_sample = 10,
  outputName_Qrandomdist = "example_Qrandomdist.csv",
  outputName_zp_Q = "example_zp_Q.csv"
)

## Network visualization using ExplodeLayout. Users can run this function multiple times for same network to explore different radius.
utility_EL(
  inputName_nodelist = "example_nodelist.csv",
  inputName_incidmat = "example_incidmat.csv",
  radius = 1.5,
  plotlabel = "c",
  outputName_plotpdf = "example_plot.pdf"
)
```

## Documentation

Please read the documentation using `utility_Mod`, `?utility_ModSig`, or
`?utility_EL` for more details.
