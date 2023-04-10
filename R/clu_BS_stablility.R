#' Measure clustering stability of columns by bootstrapping rows (sample same number of rows with replacement).
#'
#' @param inputName_nodelist name of nodelist file
#' @param inputName_incidmat name of incidence matrix file
#' @param inputName_modularity name of a csv file saving real modularity Q
#' @param BS_N_sample number of bootstrap samples. Default to 10 as a proof of concept. Should be at least 1000 in practice.
#' @param outputName_bs_randomdist name of a csv file to save modularity, number of clusters, and Rand Index compared to the original column clustering for each bootstrapped network
#' @param outputName_bs_zp name of a csv file to save modularity significance result including z-score, p-value and other information.
#'
#' @return a data frame containing modularity significance result including z-score, p-value and other information.
#' @export
#'
utility_BS_Stability = function(inputName_nodelist = 'example_nodelist.csv' ,
                          inputName_incidmat = 'example_incidmat.csv',
                          inputName_modularity = 'example_modularity.csv',
                          BS_N_sample = 10,
                          outputName_bs_randomdist = 'example_bs_randomdist.csv',
                          outputName_bs_zp = 'example_bs_zp.csv') {

  print('Read nodelist.')
  node_list=utils::read.csv(inputName_nodelist) # Read incidence matrix.
  utils::str(node_list)
  real_col_partition=node_list[node_list$Entity==2,'Cluster']

  print('Read incidence matrix.')
  incid_mat = utils::read.csv(inputName_incidmat, row.names = 1) # Read incidence matrix.
  utils::str(incid_mat)
  nr=nrow(incid_mat)
  nc=ncol(incid_mat)

  print('Read modularity.')
  Q_real = utils::read.csv(inputName_modularity)$Q # Read modularity.
  utils::str(Q_real)

  if (BS_N_sample >= 3) {
    print(
      'Bootstrap rows to test stability of column clustering.'
    )
    bs_Q=rep(-1,BS_N_sample) # bootstrap modularity
    bs_nclu=rep(-1,BS_N_sample) # bootstrap number of cluster
    bs_RI=rep(-1,BS_N_sample) # bootstrap Rand Index
    bs_ARI=rep(-1,BS_N_sample) # bootstrap Adjusted Rand Index
    for(i in seq_len(BS_N_sample)){
      bs_rows=sort(sample(nr,replace=T)) # select rows with replacement
      bs_incid_mat=incid_mat[bs_rows,]
      rownames(bs_incid_mat)=paste0('b',seq(nr))
      bs_modularity_partition=BipartiteModularityMaximization::bipmod(bs_incid_mat)
      bs_Q[i]=bs_modularity_partition$MODULARITY
      bs_nclu[i]=length(unique(bs_modularity_partition$ASSIGN))
      bs_col_partition=utils::tail(bs_modularity_partition$ASSIGN,nc)
      bs_RI[i]=fossil::rand.index(real_col_partition,bs_col_partition)
      bs_ARI[i]=fossil::adj.rand.index(real_col_partition,bs_col_partition)
      print(sprintf('bs:%d Q:%.2f, Nclu:%d, RI:%.2f, ARI:%.2f',i,bs_Q[i],bs_nclu[i],bs_RI[i],bs_ARI[i]))
    }

    BS_randomdist=data.frame(bs_Q,bs_nclu,bs_RI,bs_ARI)

    utils::write.csv(BS_randomdist,
                     outputName_bs_randomdist,
                     row.names = F)
    zp = get_significance_zp(Q_real, bs_Q)
    zp_df=data.frame('info'=names(zp),'value'=unlist(zp))
    utils::write.csv(zp_df, outputName_bs_zp, row.names = F)
  } else {
    stop('BS_N_sample must be greater than 2.')
  }
  return(zp_df)
}
