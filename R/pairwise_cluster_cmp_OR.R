#' Generate a 2x2 table for two binary variables in flat form
#'
#' @param x a binary variable
#' @param y a binary variable of same length
#'
#' @return a vector of 4 numbers counting x==0&y==0, x==0&y==1, x==1&y==0, x==1&y==1
#' @export
#'
flat_2x2_01table=function(x,y){
  if(any(is.na(x))|any(is.na(y))){
    stop('any(is.na(x))|any(is.na(y))')
  }
  if(length(x)!=length(y)){
    stop('length(x)!=length(y)')
  }
  if(!all(c(x,y) %in% c(0,1))){
    stop('!all(c(x,y) %in% c(0,1))')
  }
  m00=sum(x==0&y==0)
  m01=sum(x==0&y==1)
  m10=sum(x==1&y==0)
  m11=sum(x==1&y==1)
  return(c(m00,m01,m10,m11))
}

#' Cluster pairwise odds ratio for binary outcome using logistic regression, optionally control for covariates.
#'
#' @param data a dataframe containing cluster, outcome, and optionally covariates.
#' @param cluster_colname name of a column containing clustering information. Number of unique clusters should be at least 2.
#' @param outcome_colname name of a column containing binary outcome.
#' @param covariate_colname a vector of names of columns for covariates. Default is empty.
#' @param weight_colname weight for regression, optional
#'
#' @return a dataframe containing enrichment information
#' @export
#'
pairwise_clu_compare=function(data,cluster_colname,outcome_colname,covariate_colname=c(),weight_colname=NA){
  uniq_clus=sort(unique(data[,cluster_colname]))
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

  for(clu_i in uniq_clus){
    for(clu_j in uniq_clus){
      if(clu_i>=clu_j){
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

#' Add text behind p-value.
#'
#' @param p p-value
#'
#' @return string describing whether p<.001, p<.01, or p<.05.
#' @export
#'
psig=function(p){
  if(is.na(p)){
    return('')
  } else if(p<0.001){
    return('<.001')
  } else if(p<0.01){
    return('<.01')
  } else if(p<0.05){
    return('<.05')
  } else{
    return('')
  }
}

#' Generate strings summarizing odds ratio, confidence interval, and p-value.
#'
#' @param pcc odds ratio dataframe generated from function 'pairwise_clu_compare'
#'
#' @return a vector of strings summarizing odds ratio, confidence interval, and p-value.
#' @export
#'
print_OddsRatio=function(pcc){
  ans=rep(NA,nrow(pcc))
  for(i in seq(nrow(pcc))){
    ans[i]=sprintf('OR=%.2g, CI=%.2g-%.2g, P-corr=%.2g %s',pcc[i,'OR'],pcc[i,'ORCI025'],pcc[i,'ORCI975'],pcc[i,'p_bon'],psig(pcc[i,'p_bon']))
  }
  return(ans)
}

#' Do cluster pairwise enrichment for a binary outcome, optionally controlling for covariates.
#'
#' @param data a dataframe containing cluster, outcome, and optionally covariates.
#' @param cluster_colname name of a column containing clustering information. Number of unique clusters should be at least 2.
#' @param outcome_colname name of a column containing binary outcome.
#' @param covariate_colname a vector of names of columns for covariates. Default is empty.
#' @param weight_colname weight for regression, optional
#'
#' @return a list of 3 dataframes. 'pcc' is the full enrichment result. 'pcc_sig' only shows significant ones after Bonferroni correction. 'tb' is a simple counting table with proportions of outcome for each cluster.
#' @export
#'
enrich=function(data,cluster_colname,outcome_colname,covariate_colname=c(),weight_colname=NA){
  pcc=pairwise_clu_compare(data,cluster_colname,outcome_colname,covariate_colname,weight_colname)
  pcc=as.data.frame.matrix(pcc)
  pcc$OR_text=print_OddsRatio(pcc)
  #stats::write.csv(pcc,sprintf('%s_clu_pairwise_cmp.csv',outname),row.names=F)
  pcc_sig=pcc[pcc$p_bon<0.05,]
  #stats::write.csv(pcc_sig,sprintf('%s_clu_pairwise_cmp_sigOnly.csv',outname),row.names=F)
  tb=stats::addmargins(table(data[,cluster_colname],data[,outcome_colname]))
  tb=as.data.frame.matrix(tb)
  tb$proportion=tb[,'1']/tb[,'Sum']
  tb$cluster=rownames(tb)
  tb=tb[order(tb$proportion),]
  #stats::write.csv(tb[,c('cluster','0','1','Sum','proportion')],sprintf('%s_clu_outcome_proportion.csv',outname),row.names=F)
  return(list('pcc'=pcc,'pcc_sig'=pcc_sig,'tb'=tb))
}


#' Plot odds ratio of each cluster compared to control (cluster 0).
#'
#' @param pcc odds ratio dataframe generated from function 'pairwise_clu_compare' or 'enrich'.
#'
#' @return a plot of odds ratio compared to control (cluster 0) with confidence interval as error bar.
#' @export
#'
plotOddsRatio_CaseVsCtrl=function(pcc){
  pcc_CaseVsCtrl=pcc[pcc$clu_ref==0,]
  #svg(sprintf('%s_CluVsCtrlOR.svg',outname))
  plot(pcc_CaseVsCtrl$clu_cmp,pcc_CaseVsCtrl$OR,xlab='cluster',ylab='odds ratio (cluster vs control)', ylim=c(min(0,pcc_CaseVsCtrl$ORCI025),max(pcc_CaseVsCtrl$ORCI975)))
  graphics::arrows(x0 = pcc_CaseVsCtrl$clu_cmp,y0 = pcc_CaseVsCtrl$ORCI025,x1 = pcc_CaseVsCtrl$clu_cmp,y1 = pcc_CaseVsCtrl$ORCI975,code=3,angle=90,length=0.1)
  graphics::abline(h=1,lty=2)
  #dev.off()
}
