#' Univariable feature selection using logistic regression.
#'
#' @param data dataframe containing independent variables and outcome for feature selection
#' @param independent_variables names of independent variables
#' @param covariates optional
#' @param outcome name of outcome
#'
#' @return dataframe containing univariable feature selection results in details
#' @export
#'
FeatureSelection_UniVar_LogReg = function(data,
                                          independent_variables,
                                          covariates=c(),
                                          outcome) {
  rows_nomissing = apply(data[, c(independent_variables, covariates, outcome)], 1, function(x) ! any(is.na(x)))
  print(table(rows_nomissing))
  subdata = data[rows_nomissing,]
  LOR_mean = c()
  LOR_se = c()
  ORP = c()
  for (feature in independent_variables) {
    fmla = stats::as.formula(paste0(outcome, '~', paste(
      c(feature, covariates), collapse = '+'
    )))
    model = stats::glm(fmla, subdata, family = stats::binomial(link = 'logit'))
    s = summary(model)
    LOR_mean = c(LOR_mean, s$coefficient[feature, 'Estimate'])
    LOR_se = c(LOR_se, s$coefficient[feature, 'Std. Error'])
    ORP = c(ORP, s$coefficient[feature, 'Pr(>|z|)'])
  }
  OR = exp(LOR_mean)
  ORCI025 = exp(LOR_mean - 1.96 * LOR_se)
  ORCI975 = exp(LOR_mean + 1.96 * LOR_se)
  pBon = stats::p.adjust(ORP, method = 'bonferroni')
  pFDR = stats::p.adjust(ORP, method = 'fdr')
  df = data.frame('features' = independent_variables,
                  OR,
                  ORCI025,
                  ORCI975,
                  'p' = ORP,
                  pBon,
                  pFDR)
  return(df)
}

#' Summarize univariable feature selection results (Bonferroni corrected p-values) for multiple outcomes.
#'
#' @param data dataframe containing independent variables and outcome for feature selection
#' @param independent_variables names of independent variables
#' @param covariates optional, same for all independent variables.
#' @param outcomes names of outcomes
#'
#' @return dataframe containing Bonferroni corrected p-values from univariable feature selection for each outcome.
#' @export
#'
FeatureSelection_UniVar_LogReg_MultiOutcomes = function(data,
                                                        independent_variables,
                                                        covariates=c(),
                                                        outcomes) {
  df = data.frame('features' = independent_variables)
  for (outcome in outcomes) {
    u=FeatureSelection_UniVar_LogReg(data, independent_variables, covariates, outcome)
    print(u)
    df[, outcome] = u$pBon
  }
  return(df)
}
