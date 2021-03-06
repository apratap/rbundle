
#ref taken from https://github.com/th1vairam/pcbc_c4_analysis/blob/master/code/R/lib/getAssociationStatistics.R
getAssociationStatistics <- function(COVARIATES,FactorCovariates,ContCovariates,PVAL=0.05){
  
  getFactorAssociationStatistics <- function(factorNames,COVARIATES, na.action='remove'){
    if (na.action == "remove")
      COVARIATES = na.omit(COVARIATES[,factorNames])
    fac1 = as.factor(COVARIATES[,1])
    fac2 = as.factor(COVARIATES[,2])
    
    stats = assocstats(xtabs(~fac1+fac2))
    
    return(c(Estimate=stats$cramer,Pval=stats$chisq_tests['Pearson','P(> X^2)']))
  }
  
  # Find association between factor covariates
  if (length(FactorCovariates) != 0){
    COVARIATES.FACTOR.CORRELATION = apply(expand.grid(FactorCovariates,FactorCovariates),1,getFactorAssociationStatistics,COVARIATES[,FactorCovariates])
    COVARIATES.FACTOR.CORRELATION.ESTIMATE <- matrix(COVARIATES.FACTOR.CORRELATION['Estimate',],nrow=length(FactorCovariates),ncol=length(FactorCovariates))
    COVARIATES.FACTOR.CORRELATION.PVAL <- matrix(COVARIATES.FACTOR.CORRELATION['Pval',],nrow=length(FactorCovariates),ncol=length(FactorCovariates))
    
    colnames(COVARIATES.FACTOR.CORRELATION.ESTIMATE) <- FactorCovariates
    rownames(COVARIATES.FACTOR.CORRELATION.ESTIMATE) <- FactorCovariates
    
    colnames(COVARIATES.FACTOR.CORRELATION.PVAL) <- FactorCovariates
    rownames(COVARIATES.FACTOR.CORRELATION.PVAL) <- FactorCovariates
  } else{
    COVARIATES.FACTOR.CORRELATION.ESTIMATE <- NULL
    COVARIATES.FACTOR.CORRELATION.PVAL <- NULL
  }
  
  # Find correlation between continuous covariates
  if (length(ContCovariates) != 0){
    tmp <- apply(COVARIATES[,ContCovariates],2,as.numeric)
    COVARIATES.CONT.CORRELATION = corr.test(tmp,use = 'pairwise.complete.obs')
    COVARIATES.CONT.CORRELATION.ESTIMATE = COVARIATES.CONT.CORRELATION$r
    COVARIATES.CONT.CORRELATION.PVAL = COVARIATES.CONT.CORRELATION$p
  } else {
    COVARIATES.CONT.CORRELATION.ESTIMATE <- NULL
    COVARIATES.CONT.CORRELATION.PVAL <- NULL
  }
  
  # Find ICC between factor and continuous covariates
  getFactorContAssociationStatistics <- function(factorContNames,COVARIATES, na.action='remove'){
    if (na.action == "remove")
      COVARIATES = na.omit(COVARIATES[,factorContNames])
    
    stats = ICC(COVARIATES)
    
    return(c(Estimate = stats$results['Single_raters_absolute','ICC'],
             Pval = stats$results['Single_raters_absolute','p']))
  }
  
  # Find association between factor and continuous covariates
  if (length(FactorCovariates) != 0  & length(ContCovariates) != 0){
    COVARIATES.FACTORCONT.CORRELATION = apply(expand.grid(FactorCovariates,ContCovariates),1,getFactorContAssociationStatistics,COVARIATES[,c(FactorCovariates,ContCovariates)])
    COVARIATES.FACTORCONT.CORRELATION.ESTIMATE <- matrix(COVARIATES.FACTORCONT.CORRELATION['Estimate',],nrow=length(FactorCovariates),ncol=length(ContCovariates))
    COVARIATES.FACTORCONT.CORRELATION.PVAL <- matrix(COVARIATES.FACTORCONT.CORRELATION['Pval',],nrow=length(FactorCovariates),ncol=length(ContCovariates))
    
    colnames(COVARIATES.FACTORCONT.CORRELATION.ESTIMATE) <- ContCovariates
    rownames(COVARIATES.FACTORCONT.CORRELATION.ESTIMATE) <- FactorCovariates
    
    colnames(COVARIATES.FACTORCONT.CORRELATION.PVAL) <- ContCovariates
    rownames(COVARIATES.FACTORCONT.CORRELATION.PVAL) <- FactorCovariates
  } else {
    COVARIATES.FACTORCONT.CORRELATION.ESTIMATE <- NULL
    COVARIATES.FACTORCONT.CORRELATION.PVAL<- NULL
  }
  
  # Combine all estimates that are significant
  if (length(FactorCovariates) != 0 & length(ContCovariates) == 0){
    COVARIATES.CORRELATION.ESTIMATE = COVARIATES.FACTOR.CORRELATION.ESTIMATE    
    COVARIATES.CORRELATION.PVAL = COVARIATES.FACTOR.CORRELATION.PVAL
  }else if (length(FactorCovariates) == 0 & length(ContCovariates) != 0){
    COVARIATES.CORRELATION.ESTIMATE = COVARIATES.CONT.CORRELATION.ESTIMATE
    COVARIATES.CORRELATION.PVAL = COVARIATES.CONT.CORRELATION.PVAL
  }else if (length(FactorCovariates) != 0 & length(ContCovariates) != 0){
    COVARIATES.CORRELATION.ESTIMATE = rbind(cbind(COVARIATES.FACTOR.CORRELATION.ESTIMATE,COVARIATES.FACTORCONT.CORRELATION.ESTIMATE),
                                            cbind(t(COVARIATES.FACTORCONT.CORRELATION.ESTIMATE),COVARIATES.CONT.CORRELATION.ESTIMATE))
    
    COVARIATES.CORRELATION.PVAL = rbind(cbind(COVARIATES.FACTOR.CORRELATION.PVAL,COVARIATES.FACTORCONT.CORRELATION.PVAL),
                                        cbind(t(COVARIATES.FACTORCONT.CORRELATION.PVAL),COVARIATES.CONT.CORRELATION.PVAL))
  }else{
    COVARIATES.CORRELATION.ESTIMATE = NULL    
    COVARIATES.CORRELATION.PVAL = NULL
  }
  
  # plot heatmap
  tmp <- COVARIATES.CORRELATION.ESTIMATE
  tmp[COVARIATES.CORRELATION.PVAL>PVAL] <- 0
  p <- ggheatmap(abs(tmp),hm.colours=brewer.pal(9,'Reds'))
  
  return(list(ESTIMATE = COVARIATES.CORRELATION.ESTIMATE, PVAL = COVARIATES.CORRELATION.PVAL, plot=p))  
}