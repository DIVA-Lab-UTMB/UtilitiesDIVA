#' MinMax transformation of a vector
#'
#' @param x a numeric vector of length at least 2 without missing values
#'
#' @return MinMax transformed vector as (x-min(x))/(max(x)-min(x))
#' @export
#'
vector_MinMax = function(x) {
  if (length(x) < 2) {
    warning('length(x)<2')
    return(x)
  } else if (max(x) - min(x) < .Machine$double.eps) {
    warning('max(x)-min(X)<.Machine$double.eps')
    return(x)
  } else{
    return((x - min(x)) / (max(x) - min(x)))
  }
}

#' Center-Scale transformation of a vector
#'
#' @param x a numeric vector of length at least 2 without missing values and are not identical.
#'
#' @return Center-Scale transformed vector as (x-mean(x))/sd(x)
#' @export
#'
vector_CenterScale = function(x) {
  if (length(x) < 2) {
    warning('length(x)<2')
    return(x)
  } else if (stats::sd(x) < .Machine$double.eps) {
    warning('stats::sd(x)<.Machine$double.eps')
    return(x)
  } else{
    return((x - mean(x)) / (stats::sd(x)))
  }
}

#' Shuffle incidence matrix using different null models.
#'
#' @param incid_mat incidence matrix
#' @param preserve 'density' shuffles entries across whole matrix, 'row_deg' shuffles entries within each row, 'col_deg' shuffles entries within each column.
#'
#' @return shuffled incidence matrix
#' @export
#'
shuffle_incid_mat = function(incid_mat,
                             preserve = c('density', 'row_deg', 'col_deg')) {
  # shuffle incidence matrix based on some null model
  if (preserve == 'density') {
    return(matrix(sample(as.vector(incid_mat)), nrow(incid_mat)))
  } else if (preserve == 'row_deg') {
    return(t(apply(incid_mat, 1, sample)))
  } else if (preserve == 'col_deg') {
    return(apply(incid_mat, 2, sample))
  } else {
    stop(paste('Null model', preserve, 'currently not supported.'))
  }
}

#' Get modularity distribution of random networks generated from an original network using specific null model.
#'
#' @param incid_mat incidence matrix of original network.
#' @param N_sample number of samples, default to 1000.
#' @param preserve 'density' shuffles entries across whole matrix, 'row_deg' shuffles entries within each row, 'col_deg' shuffles entries within each column.
#'
#' @return Q a vector of modularity of the randomly generated networks.
#' @return Nclu a vector of number of clusters of the randomly generated networks.
#' @export
#'
simulate_modularity_distribution_for_null_model = function(incid_mat,
                                                           N_sample = 1000,
                                                           preserve = 'col_deg') {
  Q = rep(-1, N_sample)
  Nclu = rep(-1, N_sample)
  for (i in seq(N_sample)) {
    random_incid_mat = shuffle_incid_mat(incid_mat, preserve = preserve)
    random_Q_A = BipartiteModularityMaximization::bipmod(random_incid_mat)
    Q[i] = random_Q_A$MODULARITY
    Nclu[i] = length(unique(random_Q_A$ASSIGN))
    print(sprintf('random net #%d: Q=%f Nclu=%d', i, Q[i], Nclu[i]))
  }
  return(list('Q' = Q, 'Nclu' = Nclu))
}

#' Get significance of a real value over a Gaussian distribution.
#'
#' @param V_real real value.
#' @param V_randomdist a vector of values, assuming they follow a Gaussian distribution.
#'
#' @return V_real original real value
#' @return V_random_mean mean of V_randomdist
#' @return V_random_sd standard deviation of V_randomdist
#' @return z_score z-score of V_real compared to V_randomdist
#' @return p_val two-sided p-value of z-score.
#' @return V_random_length length of V_randomdist
#' @return V_random_min minimum of V_randomdist
#' @return V_random_max maximum of V_randomdist
#' @return V_random_median median of V_randomdist
#' @return V_random_mad median absolute deviation of V_randomdist
#'
#' @export
#'
get_significance_zp = function(V_real, V_randomdist) {
  V_random_mean = mean(V_randomdist)
  V_random_sd = stats::sd(V_randomdist)
  z = (V_real - V_random_mean) / V_random_sd
  p = 2 * stats::pnorm(-abs(z))
  return(
    list(
      'V_real' = V_real,
      'V_random_mean' = V_random_mean,
      'V_random_sd' = V_random_sd,
      'z_score' = z,
      'p_val' = p,
      'V_random_length' = length(V_randomdist),
      'V_random_min' = min(V_randomdist),
      'V_random_max' = max(V_randomdist),
      'V_random_median' = stats::median(V_randomdist),
      'V_random_mad' = stats::mad(V_randomdist)
    )
  )
}

#' Remove isolated (all zero) rows and columns of an incidence matrix.
#'
#' @param incid_mat incidence matrix.
#'
#' @return a new matrix with isolated rows and columns removed.
#' @export
#'
remove_isolatedNodes = function(incid_mat) {
  if (any(is.na(incid_mat))) {
    stop('Incidence matrix contains NA.')
  }
  # if(any(!is.numeric(incid_mat))){
  #   stop('Incidence matrix contains non-numeric values.')
  # }
  if (any(incid_mat < 0)) {
    stop('Incidence matrix contains negative values.')
  }
  non_isolated_rows = apply(incid_mat, 1, function(x)
    any(x != 0))
  non_isolated_cols = apply(incid_mat, 2, function(x)
    any(x != 0))
  return(incid_mat[non_isolated_rows, non_isolated_cols])
}

#' Get nodelist of a bipartite network with 2D coordinates and predefined cluster or partition.
#'
#' @param incid_mat incidence matrix of the bipartite network.
#' @param Cluster a vector of positive integers indicating cluster membership of each node. Here is the order: first row nodes from top to bottom, then column nodes from left to right.
#'
#' @return a dataframe with 5 columns: Label, Cluster, X, Y, Entity. Label is the node name. X and Y are 2D coordinates from FR layout. Entity indicates whether a node is a row node (1) or a column node (2).
#' @export
#'
get_nodelist = function(incid_mat, Cluster) {
  g = igraph::graph_from_incidence_matrix(incid_mat, weighted = TRUE)
  XY = igraph::layout_with_fr(g)
  Label = igraph::V(g)$name
  X = XY[, 1]
  Y = XY[, 2]
  Entity = c(rep(1, nrow(incid_mat)), rep(2, ncol(incid_mat)))
  nl = data.frame(Label, Cluster, X, Y, Entity)
  return(nl)
}


#' A simple pipeline finding modularity (and optionally significance of modularity) and ExplodeLayout of a bipartite network.
#'
#' @param fname stem of file name of input and output files.
#' @param EL_radius radius of ExplodeLayout.
#' @param EL_nodelab whether to plot node labels. '' labels no nodes, 'r' labels row nodes, 'c' labels column nodes, 'rc' labels all nodes.
#' @param signif_preserve null models to use on incidence matrix. 'density' shuffles entries across whole matrix, 'row_deg' shuffles entries within each row, 'col_deg' shuffles entries within each column.
#' @param signif_N_sample number of random networks generated for significance test. The significance test is skipped if signif_N_sample<3.
#'
#' @return No return value. All results are saved in files.
#'
#' This function does the following things:
#' 1. reads bipartite network data represented as incidence matrix from file "[fname]_data.csv";
#' 2. removes empty, a.k.a, isolated rows and columns where all entries are 0, and saves the new matrix to "[fname]_incidmat.csv";
#' 3. finds modularity and optimal clustering of the network, and saves the nodelist with clustering membership to "[fname]_nodelist.csv";
#' 4. optionally, finds significance of modularity using specific null model, saves the modularity distribution of the random networks generated by the null model to "[fname]_Qrandomdist.csv", and saves the significance result to "[fname]_zp_Q.csv";
#' 5. plot the network using ExplodeLayout.
#'
#' @export
pipeline_Mod_ModSig_EL = function(fname = 'example',
                                  EL_radius = 1.2,
                                  EL_nodelab = 'c',
                                  signif_preserve = 'col_deg',
                                  signif_N_sample = 10) {
  print('Get incidence matrix.')
  orig_df = utils::read.csv(paste0(fname, '_data.csv'), row.names = 1) # Read original data frame.
  incid_mat = remove_isolatedNodes(orig_df) # Get incidence matrix after removing isolated rows and columns.
  utils::write.csv(incid_mat, paste0(fname, '_incidmat.csv'), row.names = T) # Save incidence matrix.
  utils::str(incid_mat)


  print(
    'Partition the bipartite network optimizing bipartite modularity using BipartiteModularityMaximization.'
  )
  Q_A = BipartiteModularityMaximization::bipmod(incid_mat) # Get modularity and partition.
  nodelist = get_nodelist(incid_mat, Q_A$ASSIGN) # Get nodelist with coordinates and partition.
  utils::write.csv(nodelist, paste0(fname, '_nodelist.csv'), row.names =
                     F) # Save nodelist.
  utils::str(nodelist)

  if (signif_N_sample >= 3) {
    print(
      'Get significance of real modularity compared to modularity distribution of random networks with user specified null model.'
    )
    Q_real = Q_A$MODULARITY # Modularity of original network.
    Q_randomdist = simulate_modularity_distribution_for_null_model(incid_mat, N_sample = signif_N_sample, preserve = signif_preserve)
    utils::write.csv(Q_randomdist,
                     paste0(fname, '_Qrandomdist.csv'),
                     row.names = F)
    zp = get_significance_zp(Q_real, Q_randomdist$Q)
    utils::write.csv(zp, paste0(fname, '_zp_Q.csv'), row.names = F)
  }


  print('Find new layout using ExplodeLayout.')
  exploded_nodelist = ExplodeLayout::get_explode_nodelist(nodelist, radius =
                                                            EL_radius) # Explode the coordinates with user specified radius.
  p = ExplodeLayout::plot_binet_ggplot2(exploded_nodelist, incid_mat, plotlabel = EL_nodelab) # Plot the network.
  print(p) # Print the plotted network on screen. Optional.
  grDevices::pdf(sprintf('%s_EL_r%.1f.pdf', fname, EL_radius)) # Save the plot as pdf.
  print(p)
  grDevices::dev.off()

}

#' A simple pipeline finding optimal clustering and corresbonding modularity (and optionally significance of modularity).
#'
#' @param fname stem of file name of input and output files.
#' @param signif_preserve null models to use on incidence matrix. 'density' shuffles entries across whole matrix, 'row_deg' shuffles entries within each row, 'col_deg' shuffles entries within each column.
#' @param signif_N_sample number of random networks generated for significance test. The significance test is skipped if signif_N_sample<3.
#'
#' @return No return value. All results are saved in files.
#'
#' This function does the following things:
#' 1. reads bipartite network data represented as incidence matrix from file "[fname]_data.csv";
#' 2. removes empty, a.k.a, isolated rows and columns where all entries are 0, and saves the new matrix to "[fname]_incidmat.csv";
#' 3. finds modularity and optimal clustering of the network, and saves the nodelist with clustering membership to "[fname]_nodelist.csv";
#' 4. optionally, finds significance of modularity using specific null model, saves the modularity distribution of the random networks generated by the null model to "[fname]_Qrandomdist.csv", and saves the significance result to "[fname]_zp_Q.csv";
#'
#' @export
utility_Mod_ModSig = function(fname = 'example',
                              signif_preserve = 'col_deg',
                              signif_N_sample = 10) {
  print('Get incidence matrix.')
  orig_df = utils::read.csv(paste0(fname, '_data.csv'), row.names = 1) # Read original data frame.
  incid_mat = remove_isolatedNodes(orig_df) # Get incidence matrix after removing isolated rows and columns.
  utils::write.csv(incid_mat, paste0(fname, '_incidmat.csv'), row.names = T) # Save incidence matrix.
  utils::str(incid_mat)


  print(
    'Partition the bipartite network optimizing bipartite modularity using BipartiteModularityMaximization.'
  )
  Q_A = BipartiteModularityMaximization::bipmod(incid_mat) # Get modularity and partition.
  nodelist = get_nodelist(incid_mat, Q_A$ASSIGN) # Get nodelist with coordinates and partition.
  utils::write.csv(nodelist, paste0(fname, '_nodelist.csv'), row.names =
                     F) # Save nodelist.
  utils::str(nodelist)

  if (signif_N_sample >= 3) {
    print(
      'Get significance of real modularity compared to modularity distribution of random networks with user specified null model.'
    )
    Q_real = Q_A$MODULARITY # Modularity of original network.
    Q_randomdist = simulate_modularity_distribution_for_null_model(incid_mat, N_sample = signif_N_sample, preserve = signif_preserve)
    utils::write.csv(Q_randomdist,
                     paste0(fname, '_Qrandomdist.csv'),
                     row.names = F)
    zp = get_significance_zp(Q_real, Q_randomdist$Q)
    utils::write.csv(zp, paste0(fname, '_zp_Q.csv'), row.names = F)
  }
}



#' Plot network from data file using ExplodeLayout and save the plot to pdf
#'
#' @param fname stem of file name of input and output files.
#' @param radius radius of ExplodeLayout.
#' @param plotlabel whether to plot node labels. '' labels no nodes, 'r' labels row nodes, 'c' labels column nodes, 'rc' labels all nodes.
#'
#' @return p the ggplot object
#'
#' This function does the following things:
#' 1. reads nodelist file from "[fname]_nodelist.csv";
#' 2. reads incidence matrix file from "[fname]_incidmat.csv";
#' 3. visualizes the network using ExplodeLayout according to user specified radius and prints the plot on screen;
#' 4. saves the plot to "[fname]_EL_r[radius].pdf"
#' @export
#'
deprecated_utility_EL = function(fname, radius, plotlabel) {
  nodelist = utils::read.csv(paste0(fname, '_nodelist.csv'))
  incid_mat = utils::read.csv(paste0(fname, '_incidmat.csv'), row.names = 1)
  exploded_nodelist = ExplodeLayout::get_explode_nodelist(nodelist, radius =
                                                            radius) # Explode the coordinates with user specified radius.
  p = ExplodeLayout::plot_binet_ggplot2(exploded_nodelist, incid_mat, plotlabel = plotlabel) # Plot the network.
  print(p) # Print the plotted network on screen. Optional.
  grDevices::pdf(sprintf('%s_EL_r%.1f.pdf', fname, radius)) # Save the plot as pdf.
  print(p)
  grDevices::dev.off()
  return(p)
}
