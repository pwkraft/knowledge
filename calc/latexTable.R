latexTable <- function(x, caption=NULL, label=NULL, align=NULL, digits=3
                       , varlabs=NULL, mlabs=NULL, ...){
  ############################################################################################
  ### Function to print model results in latex table
  ### replaces stargazer for unsupported models
  ## x: model list
  ## caption: see xtable for details
  ## label: see xtable for details
  ## align: see xtable for details
  ## digits: nteger indicating the number of decimal places to be used
  ## varlabs: list of variable names and respective labels (also determines order in table)
  ## mlabs: labels for each model
  ## ...: further arguments passed to print.xtable (e.g. file etc.)
  ############################################################################################
  
  ## save model in list
  if(class(x)!="list") x <- list(x)
  tbl <- data.frame(vars = names(coef(x[[1]])))
  
  for(m in 1:length(x)){
    ## extract varnames, coefs and se
    vars <- names(coef(x[[m]]))
    coefs <- round(coef(x[[m]]), digits)
    se <- paste0("(",round(sqrt(diag(vcov(x[[m]]))), digits),")")
    tmp <- data.frame(vars,coefs,se)
    colnames(tmp) <- c("vars",paste0(c("coefs","se"),m))
    
    ## merge model results
    tbl <- merge(tbl,tmp,by="vars",all=T)
  }
  
  ## arrange variables
  if(!is.null(varlabs)){
    if(nrow(tbl)!=length(varlabs)) stop("Variable lables do not have correct length")
    rownames(tbl) <- as.character(tbl$vars)
    tbl <- tbl[names(varlabs),]
    tbl$vars <- unlist(varlabs)
  }
  
  ## prepare full variable names for output
  out <- data.frame(vars = as.vector(t(cbind(as.character(tbl$vars),""))))
  
  ## coefs and se in single column for each model
  for(m in 1:length(x)){
    out <- cbind(out, as.vector(t(tbl[,grep(m,colnames(tbl))])))
  }
  
  ## model names
  if(!is.null(mlabs)){
    if(length(mlabs)!=(ncol(out)-1)) stop("Model labels do not have correct length")
    colnames(out) <- c("Variable", mlabs)
  } else {
    colnames(out) <- c("Variable", paste0("(",1:length(x),")"))
  }
  
  ## convert table to character
  out <- apply(out, 2, as.character)
  
  ## add number of observations
  if(class(x[[1]])=="vglm"){
    out <- rbind(out, c("N",sapply(x, function(x) length(residuals(x))/2)))
  } else {
    out <- rbind(out, c("N",sapply(x, function(x) length(residuals(x)))))
  }
  
  ## add LogLik or R-Squared
  if(class(x[[1]])=="lm"){
    out <- rbind(out, c("R-squared (adj.)",sapply(x, function(x) 
      round(summary(x)$adj.r.squared, digits))))
  } else {
    out <- rbind(out, c("Log-Likelihood",sapply(x, function(x) 
      round(logLik(x), 0))))
  }
  
  ## adjust align for excluded rownames
  if(!is.null(align)) align <- paste0("l",align)
  
  ## export table
  print(xtable(out, caption=caption, label=label, align=align), include.rownames=F
        , hline.after=c(-1,0,nrow(out)-2,nrow(out)), ...)
}
