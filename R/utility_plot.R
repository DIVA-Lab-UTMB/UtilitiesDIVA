#' Get node degree from incidence matrix
#'
#' @param incidence_matrix a representation of bipartite network
#'
#' @return a vector representing degree of each node, ordered first by rows from top to bottom, then by columns from left to right.
#' @export
#'
get_node_degree = function(incidence_matrix) {
  return(c(
    apply(incidence_matrix, 1, sum),
    apply(incidence_matrix, 2, sum)
  ))
}

#' Get node size by degree.
#'
#' @param Degree a numeric vector
#' @param Entity indicating whether a node in bipartite network is on the row side (1) or column side (2).
#' @param by Scale the degree by 'row', 'col', 'both', or 'neither'. Default is 'row'
#' @param minsize minimum size of a node. Also used if node size is not scaled by degree.
#' @param maxsize maximum size of a node.
#'
#' @return a vector of node size.
#' @export
#'
get_node_size_by_degree = function(Degree,
                                   Entity,
                                   by = c('row', 'col', 'both', 'neither'),
                                   minsize = 0,
                                   maxsize = 10) {
  nodelist = data.frame(
    'Degree' = Degree,
    'Entity' = Entity,
    'Size' = rep(minsize, length(Degree))
  )
  rowdeg = nodelist[nodelist$Entity == 1, 'Degree']
  coldeg = nodelist[nodelist$Entity == 2, 'Degree']
  if (by == 'row') {
    nodelist[nodelist$Entity == 1, 'Size'] = vector_MinMax(rowdeg, min_val =
                                                             minsize, max_val = maxsize)
  } else if (by == 'col') {
    nodelist[nodelist$Entity == 2, 'Size'] = vector_MinMax(coldeg, min_val =
                                                             minsize, max_val = maxsize)
  } else if (by == 'both') {
    nodelist[, 'Size'] = vector_MinMax(Degree, min_val = minsize, max_val =
                                         maxsize)
  }
  return(nodelist$Size)
}

#' Get node shape based on Entity
#'
#' @param Entity indicating whether a node in bipartite network is on the row side (1) or column side (2).
#' @param rowshape Default is 21 (filled circle)
#' @param colshape Default is 24 (filled triangle point-up)
#'
#' @references http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
#' @return a vector of node shape encoded in base R (pch).
#' @export
get_node_shape = function(Entity,
                          rowshape = 21,
                          colshape = 24) {
  nodeshape = Entity
  nodeshape[Entity == 1] = rowshape
  nodeshape[Entity == 2] = colshape
  return(nodeshape)
}

#' Get node border thickness based on Entity
#'
#' @param Entity indicating whether a node in bipartite network is on the row side (1) or column side (2).
#' @param nodeborder_row Default is 1
#' @param nodeborder_col Default is 2
#'
#' @return a vector of node border thickness used in ggplot2 as stroke.
#' @export
#'
get_node_nodeborder = function(Entity,
                               nodeborder_row = 1,
                               nodeborder_col = 2) {
  nodeborder = Entity
  nodeborder[Entity == 1] = nodeborder_row
  nodeborder[Entity == 2] = nodeborder_col
  return(nodeborder)
}

#' Get node color based on partition
#'
#' @param partition a vector of integer used to color nodes
#' @param type can be 'Cluster', 'Entity', 'Outcome', each with predefined color code
#'
#' @return a vector of node colors
#' @export
#'
get_node_color = function(partition,
                          type = c('Cluster', 'Entity', 'Outcome')) {
  nodecolor = partition
  clu_color_list = c(
    "#C9197A",
    "#94EA18",
    "#0AF3EE",
    "#808080",
    "#E5F115",
    "#8825F9",
    "#CE9A89",
    "#fd991c",
    "black",
    "#6b8e23",
    "#C0C4E0",
    "#669999",
    "blue",
    "pink"
  )
  entity_color_list = c("red", "black")
  outcome_color_list = c("magenta", "sky blue", "orange", "yellow", "brown")
  if (type == 'Cluster') {
    nodecolor = clu_color_list[partition]
  } else if (type == 'Entity') {
    nodecolor = entity_color_list[partition]
  } else if (type == 'Outcome') {
    nodecolor = outcome_color_list[partition]
  }
  return(nodecolor)
}

#' Get node label to plot based on partition
#'
#' @param Label original node labels
#' @param Entity indicating whether a node in bipartite network is on the row side (1) or column side (2).
#' @param by which side to keep. Can be 'col', 'row', 'both', 'neither'
#'
#' @return a vector of node labels where NA means don't plot.
#' @export
#'
get_node_plotlabel = function(Label,
                              Entity,
                              by = c('col', 'row', 'both', 'neither')) {
  nodelabel = Label
  if (by == 'row') {
    nodelabel[Entity == 2] = NA
  } else if (by == 'col') {
    nodelabel[Entity == 1] = NA
  } else if (by == 'neither') {
    nodelabel = rep(NA, length(Label))
  }
  return(nodelabel)
}

#' Get edgelist from incidence matrix. Identical to the function with same name in ExplodeLayout. Duplicated for user's convenience if user doesn't want to load ExplodeLayout.
#'
#' @param incidence_matrix incidence matrix of a bipartite network. Should be a data frame with row names and column names.
#'
#' @return a data frame with 3 columns 'nodesR', 'nodesC', 'values', where each row contains the row node, column node, and value in incidence matrix. Only non-zero values are kept in the edgelist.
#' @export
#'
get_edgelist_from_incidmat = function(incidence_matrix) {
  el = utils::stack(incidence_matrix) # a dataframe of two columns: values, ind
  el$nodesR = rownames(incidence_matrix)
  el$nodesC = as.character(el$ind)
  return(el[el$values != 0, c('nodesR', 'nodesC', 'values')])
}

#' Calculate edge width from edge weight
#'
#' @param edgeweight a vector of values used to get edge width. The larger the edge weight, the larger the edge width. The transformation from edge weight to edge width is linear.
#' @param weighted if true (default), scale edge width between minwidth and maxwidth. If false, use minwidth for all edges.
#' @param minwidth Default is 0.01
#' @param maxwidth Default is 1
#'
#' @return a vector of edge width
#' @export
#'
get_edge_width = function(edgeweight,
                          weighted = T,
                          minwidth = 0.01,
                          maxwidth = 1) {
  edgewidth = rep(minwidth, length(edgeweight))
  if (weighted) {
    edgewidth = vector_MinMax(edgeweight, minwidth, maxwidth)
  }
  return(edgewidth)
}

#' Get edge color (gray scale) from edge weight.
#'
#' @param edgeweight a vector of values used to get edge width. The larger the edge weight, the darker the edge color.
#' @param mincolor darkest gray. 0 for black, 1 for white.
#' @param maxcolor brightest gray. 0 for black, 1 for white.
#'
#' @return a vector of edge color (gray scale)
#' @export
#'
get_edge_color = function(edgeweight,
                          mincolor = 0.5,
                          maxcolor = 1) {
  color_value = vector_MinMax(-edgeweight, mincolor, maxcolor)
  edgecolor = grDevices::gray(color_value)
  return(edgecolor)
}

#' Get nodelist with extra features and plotting information. It overwrites the function with same name in ExplodeLayout for more detailed control.
#'
#' @param nodelist a data frame containing at least 'Label', 'X', 'Y', 'Entity', 'Cluster'.
#' @param new_coordinates a data frame containing at least 'Label', 'newX', 'newY'.
#' @param incidence_matrix a data frame representing incidence matrix of the network. Need to have row names and column names. The label order of the incidence matrix needs to be the same as in nodelist.
#' @param plotlabel which side to keep node label. Can be 'col', 'row', 'both', 'neither'. Default is 'col'.
#' @param nodesize_min minimum size of a node. Also used if node size is not scaled by degree. Default is 1.
#' @param nodesize_max maximum size of a node. Default is 10
#' @param nodesize_by_degree Scale the degree by 'row', 'col', 'both', or 'neither'. Default is 'row'
#' @param nodeborder_row border thickness of row nodes. Default is 0.1
#' @param nodeborder_col border thickness of column nodes. Default is 2
#' @param nodelabelsize label size of nodes. Default is 3.
#' @param nodelabelcolor label color of nodes. Default is grey20.
#'
#' @return a data frame containing 'Label', 'X', 'Y', 'Entity', 'Cluster', 'Degree', 'Size', 'Shape', 'Border', 'Color', 'Plotlabel', 'Labelsize'.

#' @export
#'
get_nodelist_for_plotting = function(nodelist,
                                     new_coordinates,
                                     incidence_matrix,
                                     plotlabel = 'col',
                                     nodesize_min = 1,
                                     nodesize_max = 10,
                                     nodesize_by_degree = 'row',
                                     nodeborder_row = 0.1,
                                     nodeborder_col = 2,
                                     nodelabelsize = 3,
                                     nodelabelcolor = 'grey20') {
  rownames(nodelist) = nodelist$Label
  rownames(new_coordinates) = new_coordinates$Label
  new_coordinates = new_coordinates[rownames(nodelist), ]
  nodelist$X = vector_MinMax(new_coordinates$newX)
  nodelist$Y = vector_MinMax(new_coordinates$newY)
  nodelist$Degree = get_node_degree(incidence_matrix)
  nodelist$Size = get_node_size_by_degree(nodelist$Degree,
                                          nodelist$Entity,
                                          by =
                                            nodesize_by_degree,
                                          nodesize_min,
                                          nodesize_max)
  nodelist$Shape = get_node_shape(nodelist$Entity)
  nodelist$Border = get_node_nodeborder(nodelist$Entity, nodeborder_row, nodeborder_col)
  nodelist$Color = get_node_color(nodelist$Cluster, type = 'Cluster')
  nodelist$Plotlabel = get_node_plotlabel(nodelist$Label, nodelist$Entity, by =
                                            plotlabel)
  nodelist$Labelsize = nodelabelsize
  nodelist$Labelcolor = nodelabelcolor
  return(nodelist[, c(
    'Label',
    'X',
    'Y',
    'Entity',
    'Cluster',
    'Degree',
    'Size',
    'Shape',
    'Border',
    'Color',
    'Plotlabel',
    'Labelsize',
    'Labelcolor'
  )])
}

#' Get edgelist with extra features and plotting information.
#'
#' @param nodelist a data frame containing 'Label','X','Y'.
#' @param incidence_matrix with row names and column names.
#' @param edgethickness_min Also used if edgethickness_by_weight==FALSE. Default is 0.01.
#' @param edgethickness_max Default is 1
#' @param edgecolor_min Darkest in gray scale. 0 for black, 1 for white. Default is 0.5
#' @param edgecolor_max Brightest in gray scale. Default is 1.
#' @param edgethickness_by_weight if TRUE, scale edge width by edge value. Default is TRUE.
#'
#' @return a data frame containing 'nodesR',	'nodesC',	'values',	'x0',	'y0',	'x1',	'y1',	'weight',	'edgewidth',	'color'.

#' @export
#'
get_edgelist_for_plotting = function(nodelist,
                                     incidence_matrix,
                                     edgethickness_min = 0.01,
                                     edgethickness_max = 1,
                                     edgecolor_min = 0.5,
                                     edgecolor_max = 1,
                                     edgethickness_by_weight = TRUE) {
  rownames(nodelist) = nodelist$Label
  el = get_edgelist_from_incidmat(incidence_matrix)
  el$x0 = nodelist[el$nodesR, 'X']
  el$y0 = nodelist[el$nodesR, 'Y']
  el$x1 = nodelist[el$nodesC, 'X']
  el$y1 = nodelist[el$nodesC, 'Y']
  el$weight = el$values
  el = el[order(el$weight),]
  el$edgewidth = get_edge_width(el$weight,
                                edgethickness_by_weight,
                                edgethickness_min,
                                edgethickness_max)
  el$color = get_edge_color(el$weight, edgecolor_min, edgecolor_max)
  return(el)
}

#' Plot a network base on the provided nodelist and edgelist.
#'
#' @param nodelist_for_plotting a data frame containing 'Label', 'X', 'Y', 'Entity', 'Cluster', 'Degree', 'Size', 'Shape', 'Border', 'Color', 'Plotlabel', 'Labelsize'.
#' @param edgelist_for_plotting a data frame containing 'nodesR',	'nodesC',	'values',	'x0',	'y0',	'x1',	'y1',	'weight',	'edgewidth',	'color'.
#'
#' @return a ggplot2 plot of the network
#' @export
#'
plot_binet_ggplot2_from_nodelist_and_edgelist = function(nodelist_for_plotting,
                                                         edgelist_for_plotting) {
  nodelist = nodelist_for_plotting
  el = edgelist_for_plotting
  p = ggplot2::ggplot()
  print('plotting edges...')
  p = p + ggplot2::geom_segment(
    ggplot2::aes(
      x = el$x0,
      y = el$y0,
      xend = el$x1,
      yend = el$y1
    ),
    size = el$edgewidth,
    colour = el$color
  )
  print('plotting nodes...')
  p = p + ggplot2::geom_point(
    ggplot2::aes(nodelist$X, nodelist$Y),
    colour = "black",
    fill = nodelist$Color,
    shape = nodelist$Shape,
    size = nodelist$Size,
    stroke = nodelist$Border
  )
  p = p + ggplot2::theme(panel.background = ggplot2::element_blank()) + ggplot2::theme(legend.position = "none")
  p = p + ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank()
  )
  p = p + ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
  print('plotting labels...')
  p = p + ggrepel::geom_label_repel(
    ggplot2::aes(
      nodelist$X,
      nodelist$Y,
      label = nodelist$Plotlabel,
      alpha = 0.5
      #color = nodelist$Labelcolor
    ),
    color = nodelist$Labelcolor,
    na.rm = TRUE,
    size = nodelist$Labelsize,
    fontface = "bold" ,
    box.padding = ggplot2::unit(0.5, "lines"),
    point.padding = ggplot2::unit(0.5, "lines"),
    segment.color = "#0000FF",
    segment.linetype = 2,
    max.overlaps = 1000,
    label.size = NA
  )
  return(p)
}
