#' Find optimal partition (clustering) and corresponding bipartite modularity of a bipartite network represented by incidence matrix.
#'
#' @param inputName_data name of a csv file of incidence matrix of bipartite network where the first column is row name.
#' @param outputName_nodelist name of a csv file to save nodelist.
#' @param outputName_incidmat name of a csv file to save incidence matrix after removing isolated rows and columns.
#' @param outputName_modularity name of a csv file to save modularity Q.
#'
#' @return MODULARITY modulairty
#' @return ASSIGN partition of the network after removing isolated nodes.
#'
#'
#' This function does the following things:
#' 1. reads bipartite network data represented as incidence matrix from [inputName_data];
#' 2. removes empty, a.k.a, isolated rows and columns where all entries are 0, and saves the new matrix to [outputName_incidmat];
#' 3. finds modularity and optimal clustering of the network, and saves the nodelist with clustering membership to [outputName_nodelist];
#' 4. save modularity to [outputName_modularity];
#' 5. return modularity and partition of the network after removing isolated nodes.
#'
#' @export
utility_Mod = function(inputName_data = 'example_data.csv',
                       outputName_nodelist = 'example_nodelist.csv',
                       outputName_incidmat = 'example_incidmat.csv',
                       outputName_modularity = 'example_modularity.csv') {
  print('Get incidence matrix.')
  orig_df = utils::read.csv(inputName_data, row.names = 1) # Read original data frame.
  incid_mat = remove_isolatedNodes(orig_df) # Get incidence matrix after removing isolated rows and columns.
  utils::write.csv(incid_mat, outputName_incidmat, row.names = T) # Save incidence matrix.
  utils::str(incid_mat)


  print(
    'Partition the bipartite network optimizing bipartite modularity using BipartiteModularityMaximization.'
  )
  Q_A = BipartiteModularityMaximization::bipmod(incid_mat) # Get modularity and partition.
  nodelist = get_nodelist(incid_mat, Q_A$ASSIGN) # Get nodelist with coordinates and partition.
  utils::write.csv(nodelist, outputName_nodelist, row.names = F) # Save nodelist.
  utils::str(nodelist)

  Q = Q_A$MODULARITY
  utils::write.csv(data.frame(Q),
                   outputName_modularity,
                   row.names = F)

  return(Q_A)
}
