#' Plot network from data file using ExplodeLayout and save the plot to pdf
#'
#' @param inputName_nodelist name of nodelist file
#' @param inputName_incidmat name of incidence matrix file
#' @param radius radius of ExplodeLayout.
#' @param plotlabel whether to plot node labels. '' labels no nodes, 'r' labels row nodes, 'c' labels column nodes, 'rc' labels all nodes.
#' @param outputName_plotpdf name of the pdf to plot.
#'
#' @return the ggplot object
#'
#'
#' This function does the following things:
#' 1. reads nodelist file from [inputName_nodelist];
#' 2. reads incidence matrix file from [inputName_incidmat];
#' 3. visualizes the network using ExplodeLayout according to user specified radius and prints the plot on screen;
#' 4. saves the plot to [outputName_plotpdf].
#'
#' @export
utility_EL = function(inputName_nodelist = 'example_nodelist.csv',
                      inputName_incidmat = 'example_incidmat.csv',
                      radius = 1.5,
                      plotlabel = 'c',
                      outputName_plotpdf = 'example_plot.pdf') {
  nodelist = utils::read.csv(inputName_nodelist)
  incid_mat = utils::read.csv(inputName_incidmat, row.names = 1)
  exploded_nodelist = ExplodeLayout::get_explode_nodelist(nodelist, radius =
                                                            radius) # Explode the coordinates with user specified radius.
  p = ExplodeLayout::plot_binet_ggplot2(exploded_nodelist, incid_mat, plotlabel = plotlabel) # Plot the network.
  print(p) # Print the plotted network on screen. Optional.
  grDevices::pdf(outputName_plotpdf) # Save the plot as pdf.
  print(p)
  grDevices::dev.off()
  return(p)
}
