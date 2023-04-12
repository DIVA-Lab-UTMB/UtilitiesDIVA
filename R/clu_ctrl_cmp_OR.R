#' Cluster Vs Control odds ratio for binary outcome using logistic regression, optionally control for covariates.
#'
#' @param data a dataframe containing cluster, outcome, and optionally covariates.
#' @param cluster_colname name of a column containing clustering information. Number of unique clusters should be at least 2.
#' @param ctrl_label label for control group. Should be one of the labels in cluster column.
#' @param outcome_colname name of a column containing binary outcome.
#' @param covariate_colname a vector of names of columns for covariates. Default is empty.
#' @param weight_colname weight for regression, optional
#'
#' @return a dataframe containing enrichment information
#' @export
#'
clu_ctrl_compare=function(data,cluster_colname,ctrl_label,outcome_colname,covariate_colname=c(),weight_colname=NA){
  uniq_clus=sort(unique(data[,cluster_colname]))
  if(!(ctrl_label %in% uniq_clus)){
    stop('ctrl_label not found in clusters!')
  }
  n_clu=length(uniq_clus)
  # assume uniq_clus from 0 to n_clu-1
  #n_pair=n_clu*(n_clu-1)/2
  # results to save
  clu_ref=c()
  clu_cmp=c()
  flat2x2=matrix(NA,0,4)
  colnames(flat2x2)=c('m00','m01','m10','m11')
  p_chisq=c()
  or_ci=matrix(NA,0,3)
  colnames(or_ci)=c('OR','ORCI025','ORCI975')
  p_val=c()

  for(clu_i in ctrl_label){
    for(clu_j in uniq_clus){
      if(clu_j==ctrl_label){
        next
      }
      clu_ref=c(clu_ref,clu_i)
      clu_cmp=c(clu_cmp,clu_j)

      only_ij_data=data[(data[,cluster_colname] %in% c(clu_i,clu_j)),]
      only_ij_data$x=(only_ij_data[,cluster_colname]==clu_j)+0
      only_ij_data$y=only_ij_data[,outcome_colname]

      flat2x2=rbind(flat2x2,flat_2x2_01table(only_ij_data$x,only_ij_data$y))

      if(1 %in% dim(table(only_ij_data$x,only_ij_data$y))){
        p_chisq=c(p_chisq,NA)
        or_ci=rbind(or_ci,rep(NA,3))
        p_val=c(p_val,NA)
        next
      }
      p_chisq=c(p_chisq,stats::chisq.test(only_ij_data$x,only_ij_data$y)$p.value)

      fmla=stats::as.formula(paste("y ~ ", paste(c('x',covariate_colname), collapse= "+")))
      #print(fmla)
      if(is.na(weight_colname)){
        model=stats::glm(fmla,data=only_ij_data,family=stats::binomial(link='logit'))
      }
      else{
        model=stats::glm(fmla,data=only_ij_data,family=stats::binomial(link='logit'),weights=only_ij_data[,weight_colname])
      }
      #model=stats::glm(fmla,data=only_ij_data,family=stats::binomial(link='logit'))
      # if strange errors, return NA
      this_orci=tryCatch(exp(cbind(OR=stats::coef(model), stats::confint(model)))['x',],
                         error=function(e) rep(NA,3))
      or_ci=rbind(or_ci,this_orci)
      p_val=c(p_val,summary(model)$coefficient['x','Pr(>|z|)'])
    }
  }
  p_bon=stats::p.adjust(p_val,method='bonferroni')
  p_fdr=stats::p.adjust(p_val,method='fdr')
  ans=cbind(clu_ref,clu_cmp,flat2x2,p_chisq,or_ci,p_val,p_bon,p_fdr)
  return(ans)
}

#' Do cluster Vs Control enrichment for a binary outcome, optionally controlling for covariates.
#'
#' @param data a dataframe containing cluster, outcome, and optionally covariates.
#' @param cluster_colname name of a column containing clustering information. Number of unique clusters should be at least 2.
#' @param ctrl_label label for control group. Should be one of the labels in cluster column.
#' @param outcome_colname name of a column containing binary outcome.
#' @param covariate_colname a vector of names of columns for covariates. Default is empty.
#' @param weight_colname weight for regression, optional
#'
#' @return a list of 3 dataframes. 'ccc' is the full enrichment result. 'ccc_sig' only shows significant ones after Bonferroni correction. 'tb' is a simple counting table with proportions of outcome for each cluster.
#' @export
#'
enrich_CluVsCtrl=function(data,cluster_colname,ctrl_label,outcome_colname,covariate_colname=c(),weight_colname=NA){
  ccc=clu_ctrl_compare(data,cluster_colname,ctrl_label,outcome_colname,covariate_colname,weight_colname)
  ccc=as.data.frame.matrix(ccc)
  ccc$OR_text=print_OddsRatio(ccc)
  #stats::write.csv(ccc,sprintf('%s_clu_pairwise_cmp.csv',outname),row.names=F)
  ccc_sig=ccc[ccc$p_bon<0.05,]
  #stats::write.csv(ccc_sig,sprintf('%s_clu_pairwise_cmp_sigOnly.csv',outname),row.names=F)
  tb=stats::addmargins(table(data[,cluster_colname],data[,outcome_colname]))
  tb=as.data.frame.matrix(tb)
  tb$proportion=tb[,'1']/tb[,'Sum']
  tb$cluster=rownames(tb)
  tb=tb[order(tb$proportion),]
  #stats::write.csv(tb[,c('cluster','0','1','Sum','proportion')],sprintf('%s_clu_outcome_proportion.csv',outname),row.names=F)
  return(list('ccc'=ccc,'ccc_sig'=ccc_sig,'tb'=tb))
}
