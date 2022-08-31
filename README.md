
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
(Note that all functions below are based on files with certain formats):

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

## Get exploded coordinates using ExplodeLayout, specify plotting features, and save new nodelist file and edgelist file with the added node features and edge features.
utility_EL_advanced(
  inputName_nodelist = 'example_nodelist.csv',
  inputName_incidmat = 'example_incidmat.csv',
  radius = 1.5,
  outputName_nodelist = 'example_nodelist_plot.csv',
  outputName_edgelist = 'example_edgelist_plot.csv',
  plotlabel = 'col',
  nodesize_min = 1,
  nodesize_max = 10,
  nodesize_by_degree = 'row',
  nodeborder_row = 0.1,
  nodeborder_col = 2,
  nodelabelsize = 3,
  edgethickness_min = 0.01,
  edgethickness_max = 1,
  edgegrayscale_min = 0.5,
  edgegrayscale_max = 1,
  edgethickness_by_weight = TRUE
)

## Plot a network using nodelist file and edgelist file prepared with plotting features, and save the plot.
utility_EL_plot(
  inputName_nodelist = 'example_nodelist_plot.csv',
  inputName_edgelist = 'example_edgelist_plot.csv',
  outputName = 'example_plot',
  outputType = 'svg'
)
```

## Documentation

Please read the documentation using `?utility_Mod`, `?utility_ModSig`,
`?utility_EL`, `?utility_EL_advanced`, or `?utility_EL_plot` for more
details.
