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



#' Plot network from data file using ExplodeLayout with multiple graphical parameters and save the plot to pdf
#'
#' @param inputName_nodelist name of nodelist file
#' @param inputName_incidmat name of incidence matrix file
#' @param radius radius of ExplodeLayout. Default is 1.5
#' @param outputName_plotpdf name of the pdf to plot.
#' @param plotlabel whether to plot node labels. 'neither' labels no nodes, 'row' labels row nodes, 'col' labels column nodes, 'both' labels all nodes. Default is 'col'.
#' @param nodesize_min minimum size of node. Also used if not plot node size by degree.
#' @param nodesize_max minimum size of node.
#' @param nodesize_by_degree whether to plot node size by degree. Can be 'neither', 'row', 'col', or 'both'. Default is 'row'.
#' @param nodeborder_row stroke of row node. Default is 1.
#' @param nodeborder_col stroke of column node. Default is 2.
#' @param edgethickness_min minimum thickness of an edge. Also used if edge thickness not by weight. Default is 0.01.
#' @param edgethickness_max maximum thickness of an edge.
#' @param edgegrayscale_min darkest color of an edge on grey scale. The smaller the darker. Default is 0.5.
#' @param edgegrayscale_max lightest color of an edge on grey scale. Default is 1.
#' @param edgethickness_by_weight whether to plot edge width by thickness.
#' @param nodelabelsize size of label. Default is 3.
#'
#' @return the ggplot object
#'
#' This function does the following things:
#' 1. reads nodelist file from [inputName_nodelist];
#' 2. reads incidence matrix file from [inputName_incidmat];
#' 3. visualizes the network using ExplodeLayout according to user specified parameters and prints the plot on screen;
#' 4. saves the plot to [outputName_plotpdf].
#'
#' @export
deprecated_utility_EL_advanced = function(inputName_nodelist = 'example_nodelist.csv',
                                          inputName_incidmat = 'example_incidmat.csv',
                                          radius = 1.5,
                                          outputName_plotpdf = 'example_plot.pdf',
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
                                          edgethickness_by_weight = TRUE) {
  nodelist = utils::read.csv(inputName_nodelist)
  incid_mat = utils::read.csv(inputName_incidmat, row.names = 1)
  exploded_coords = ExplodeLayout::explode_coordinates(nodelist, radius =
                                                         radius)
  nodelist_for_plotting = UtilitiesDIVA::get_nodelist_for_plotting(
    nodelist,
    exploded_coords,
    incid_mat,
    plotlabel = plotlabel,
    nodesize_min = nodesize_min,
    nodesize_max = nodesize_max,
    nodesize_by_degree = nodesize_by_degree,
    nodeborder_row = nodeborder_row,
    nodeborder_col = nodeborder_col,
    nodelabelsize = nodelabelsize
  )
  edgelist_for_plotting = UtilitiesDIVA::get_edgelist_for_plotting(
    nodelist_for_plotting,
    incid_mat,
    edgethickness_min = edgethickness_min,
    edgethickness_max = edgethickness_max,
    edgecolor_min = edgegrayscale_min,
    edgecolor_max = edgegrayscale_max,
    edgethickness_by_weight = edgethickness_by_weight
  )
  p = UtilitiesDIVA::plot_binet_ggplot2_from_nodelist_and_edgelist(nodelist_for_plotting, edgelist_for_plotting) # Plot the network.
  print(p) # Print the plotted network on screen. Optional.
  grDevices::pdf(outputName_plotpdf) # Save the plot as pdf.
  print(p)
  grDevices::dev.off()
  return(p)
}


#' Generate nodelist and edgelist for plotting using ExplodeLayout.
#'
#' @param inputName_nodelist a csv file containing 'Label', 'X', 'Y', 'Entity', 'Cluster'. Can be generated using 'utility_Mod'.
#' @param inputName_incidmat a csv file representing incidence matrix of the network. Can be generated using 'utility_Mod'.
#' @param radius radius of ExplodeLayout. Default is 1.5
#' @param outputName_nodelist output name of the enhanced nodelist file for plotting.
#' @param outputName_edgelist output name of the enhanced edgelist file for plotting.
#' @param plotlabel which side to keep node label. Can be 'col', 'row', 'both', 'neither'. Default is 'col'.
#' @param nodesize_min minimum size of a node. Also used if node size is not scaled by degree. Default is 1.
#' @param nodesize_max maximum size of a node. Default is 10
#' @param nodesize_by_degree Scale the degree by 'row', 'col', 'both', or 'neither'. Default is 'row'
#' @param nodeborder_row border thickness of row nodes. Default is 0.1
#' @param nodeborder_col border thickness of column nodes. Default is 2
#' @param nodelabelsize label size of nodes. Default is 3.
#' @param nodelabelcolor label color of nodes. Default is grey20.
#' @param edgethickness_min Also used if edgethickness_by_weight==FALSE. Default is 0.01.
#' @param edgethickness_max Default is 1
#' @param edgegrayscale_min Darkest in gray scale. 0 for black, 1 for white. Default is 0.5
#' @param edgegrayscale_max Brightest in gray scale. Default is 1.
#' @param edgethickness_by_weight if TRUE, scale edge width by edge value. Default is TRUE.
#'
#' @return a list of enhanced nodelist and edgelist for plotting.
#' @export
#'
utility_EL_advanced = function(inputName_nodelist = 'example_nodelist.csv',
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
                               nodelabelcolor = 'grey20',
                               edgethickness_min = 0.01,
                               edgethickness_max = 1,
                               edgegrayscale_min = 0.5,
                               edgegrayscale_max = 1,
                               edgethickness_by_weight = TRUE) {
  nodelist = utils::read.csv(inputName_nodelist)
  incid_mat = utils::read.csv(inputName_incidmat, row.names = 1)
  exploded_coords = ExplodeLayout::explode_coordinates(nodelist, radius =
                                                         radius)
  nodelist_for_plotting = UtilitiesDIVA::get_nodelist_for_plotting(
    nodelist,
    exploded_coords,
    incid_mat,
    plotlabel = plotlabel,
    nodesize_min = nodesize_min,
    nodesize_max = nodesize_max,
    nodesize_by_degree = nodesize_by_degree,
    nodeborder_row = nodeborder_row,
    nodeborder_col = nodeborder_col,
    nodelabelsize = nodelabelsize,
    nodelabelcolor = nodelabelcolor
  )
  edgelist_for_plotting = UtilitiesDIVA::get_edgelist_for_plotting(
    nodelist_for_plotting,
    incid_mat,
    edgethickness_min = edgethickness_min,
    edgethickness_max = edgethickness_max,
    edgecolor_min = edgegrayscale_min,
    edgecolor_max = edgegrayscale_max,
    edgethickness_by_weight = edgethickness_by_weight
  )
  utils::write.csv(nodelist_for_plotting, outputName_nodelist)
  utils::write.csv(edgelist_for_plotting, outputName_edgelist)
  return(
    list(
      'nodelist_for_plotting' = nodelist_for_plotting,
      'edgelist_for_plotting' = edgelist_for_plotting
    )
  )
}

#' Plot a network using nodelist file and edgelist file, and save the plot.
#'
#' @param inputName_nodelist a csv file containing 'Label', 'X', 'Y', 'Entity', 'Cluster', 'Degree', 'Size', 'Shape', 'Border', 'Color', 'Plotlabel', 'Labelsize'. Can be generated using function 'utility_EL_advanced'.
#' @param inputName_edgelist a csv file containing 'nodesR',	'nodesC',	'values',	'x0',	'y0',	'x1',	'y1',	'weight',	'edgewidth',	'color'. Can be generated using function 'utility_EL_advanced'.
#' @param outputName name of output file without extension.
#' @param outputType format to be saved. Currently supports 'svg', 'pdf', 'ps', 'bmp', 'jpg', 'png', 'tif'. Also used as extension of the output file name.
#'
#' @return a ggplot2 plot of the network
#' @export
#'
utility_EL_plot = function(inputName_nodelist = 'example_nodelist_plot.csv',
                           inputName_edgelist = 'example_edgelist_plot.csv',
                           outputName = 'example_plot',
                           outputType = c('svg', 'pdf', 'ps', 'bmp', 'jpg', 'png', 'tif')) {
  print('reading nodelist...')
  nodelist = utils::read.csv(inputName_nodelist)
  print(utils::str(nodelist))
  print('reading edgelist...')
  edgelist = utils::read.csv(inputName_edgelist)
  print(utils::str(edgelist))
  print('plotting...')
  p = UtilitiesDIVA::plot_binet_ggplot2_from_nodelist_and_edgelist(nodelist, edgelist) # Plot the network.
  print(p) # Print the plotted network on screen. Optional.
  outputFullName = sprintf('%s.%s', outputName, outputType)
  print(sprintf('saving to %s ...', outputFullName))
  if (outputType == 'svg') {
    grDevices::svg(outputFullName)
  } else if (outputType == 'pdf') {
    grDevices::pdf(outputFullName)
  } else if (outputType == 'ps') {
    grDevices::cairo_ps(outputFullName)
  } else if (outputType == 'bmp') {
    grDevices::bmp(outputFullName)
  } else if (outputType == 'jpg') {
    grDevices::jpeg(outputFullName)
  } else if (outputType == 'png') {
    grDevices::png(outputFullName)
  } else if (outputType == 'tif') {
    grDevices::tiff(outputFullName)
  } else{
    warning(sprintf('%s is not a supported type.', outputType))
    return(p)
  }
  print(p)
  grDevices::dev.off()
  print(sprintf('%s saved.', outputFullName))
  return(p)
}
