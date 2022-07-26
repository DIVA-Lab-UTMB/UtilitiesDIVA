% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_ModSig.R
\name{utility_ModSig}
\alias{utility_ModSig}
\title{Find significance of bipartite network with customized null model}
\usage{
utility_ModSig(
  inputName_modularity = "example_modularity.csv",
  inputName_incidmat = "example_incidmat.csv",
  signif_preserve = "col_deg",
  signif_N_sample = 10,
  outputName_Qrandomdist = "example_Qrandomdist.csv",
  outputName_zp_Q = "example_zp_Q.csv"
)
}
\arguments{
\item{inputName_modularity}{name of a csv file saving real modularity Q.}

\item{inputName_incidmat}{name of a csv file saving incidence matrix.}

\item{signif_preserve}{null models to use on incidence matrix. 'density' shuffles entries across whole matrix, 'row_deg' shuffles entries within each row, 'col_deg' shuffles entries within each column.}

\item{signif_N_sample}{number of random networks generated for significance test. Must be greater than 2.}

\item{outputName_Qrandomdist}{name of a csv file to save modularity and number of clusters for each randomly generated network}

\item{outputName_zp_Q}{name of a csv file to save significance result including z-score, p-value and other information.}
}
\value{
significance result including z-score, p-value and other information.


This function does the following things:
1. reads bipartite network data represented as incidence matrix from [inputName_incidmat];
2. reads real modularity from [inputName_modularity];
3. finds significance of modularity using specific null model, saves the modularity distribution of the random networks generated by the null model to [outputName_Qrandomdist], and saves the significance result to [outputName_zp_Q];
}
\description{
Find significance of bipartite network with customized null model
}
