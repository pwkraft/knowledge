### function to simulate expected values/first differences (replaces Zelig)
#' built: 2016-08-27, Patrick Kraft
#' @importFrom MASS mvrnorm
#' @importFrom sandwich vcovHC
#' @param models: list of model results (lm, glm, or vglm/tobit)
#' @param iv: data frame containing the values for comparison (only 2 rows, selected variables)
#' @param robust: logical, should robust standard errors be used
#' @param nsim: number of simulations
#' @return data.frame: contains expected values, confidence intervals, variable names
#' @export

sim <- function(models, iv, robust=F, ci=c(0.025,0.975), nsim = 1000){
  
  ## prepare output object, convert input to model list
  out <- NULL
  if(class(models)[1] != "list") models <- list(models)
  
  for(i in 1:length(models)){
    ## simulate betas from sampling distribution
    if(robust == T){
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), sandwich::vcovHC(models[[i]]))
    } else {
      betas <- MASS::mvrnorm(nsim, coef(models[[i]]), vcov(models[[i]]))
    }
    
    ## extract variable names
    vars <- names(coef(models[[i]]))
    int <- grep("[^)]:", vars)
    varsInt <- strsplit(vars[int], ":")
    
    ## generate matrix of covariates
    X <- matrix(1, nrow=length(vars), ncol=nrow(iv))
    X[vars %in% names(iv),] <- t(iv[vars[vars %in% names(iv)]])
    if(class(models[[i]])[1]=="lm"){
      means <- apply(models[[i]]$model[vars[-c(1,which(vars %in% names(iv)),int)]]
                     , 2, mean, na.rm=T)
    } else if(class(models[[i]])[1] == "glm"){
      means <- apply(models[[i]]$model[vars[-c(1,which(vars %in% names(iv)),int)]]
                     , 2, mean, na.rm=T)
    } else if(class(models[[i]])[1] == "vglm" & models[[i]]@family@vfamily == "tobit"){
      means <- apply(models[[i]]@x[,vars[-c(1,2,which(vars %in% names(iv)),int)]]
                     , 2, mean, na.rm=T)
    } else stop("Model type not supported")
    X[vars %in% names(means),] <- means
    
    ## calculate interaction effects
    if(length(varsInt)>0){
      for(j in 1:length(varsInt)){
        X[int[j],] <- apply(X[vars %in% varsInt[[j]],],2,prod)
      }
    }
    
    ## calculate expected values
    if(class(models[[i]])[1]=="lm"){
      evs <- betas %*% X
    } else if(class(models[[i]])[1] == "glm"){
      if(models[[i]]$family$link == "logit"){
        evs <- 1/(1+exp(-betas %*% X))
      } else if(models[[i]]$family$link == "probit"){
        evs <- pnorm(betas %*% X)
      } else stop("Model type not supported")
    } else if(class(models[[i]])[1] == "vglm" & models[[i]]@family@vfamily == "tobit"){
      ## IDEA: decompose effect of tobit in dP(Y>0) and dY|Y>0
      ## based on predicted values (rather than EVs)
      ## note that betas[,2] is log(Sigma) estimate
      ## CHECK CALCULATIONS!
      if(unique(models[[i]]@misc$Upper)!=Inf) stop("Upper limit not supported")
      if(unique(models[[i]]@misc$Lower)!=0) warning("Limit != 0 not testes yet!")
      loLim <- unique(models[[i]]@misc$Lower)[1,1]
      
      ## expected values for z>0
      evsTemp <- betas[,-2] %*% X[-2,]
      evs <- evsTemp + exp(betas[,2]) * dnorm(evsTemp/exp(betas[,2])) / pnorm(evsTemp/exp(betas[,2]))
      
      ## probability of z>0
      pvs <- array(dim = c(nsim,ncol(X),nsim))
      for(j in 1:nrow(pvs)){
        pvs[j,,] <- matrix(rnorm(nsim*ncol(X), mean = evsTemp[j,], sd = exp(betas[j,2]))
                           , ncol = nsim)
      }
      prob <- apply(pvs, 2, function(x) apply(x, 1, function(x) mean(x>loLim)))
    } else stop("Model type not supported")
    
    skip <- F
    
    if(nrow(iv)==2){
      ## calculate first differences
      evs <- evs[,2] - evs[,1]
      if(class(models[[i]])[1] == "vglm"){
        if(models[[i]]@family@vfamily == "tobit")
          prob <- prob[,2] - prob[,1]
      }
    } else if(nrow(iv)==4) {
      ## calculate difference-in-difference
      evs <- (evs[,2] - evs[,1]) - (evs[,4] - evs[,3])
      if(class(models[[i]])[1] == "vglm"){
        if(models[[i]]@family@vfamily == "tobit")
          prob <- (prob[,2] - prob[,1]) - (prob[,4] - prob[,3])
      }
    } else {
      ## compute predicted values for each step
      warning("Check number of scenarios - STILL TESTING")
      if(class(models[[i]])[1] != "vglm"){
        res <- data.frame(mean = apply(evs, 2, mean)
                          , cilo = apply(evs, 2, quantile, ci[1])
                          , cihi = apply(evs, 2, quantile, ci[2])
                          , dv = as.factor(colnames(models[[i]]$model)[1])
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , ivval = iv[,1])
      } else if(models[[i]]@family@vfamily == "tobit"){
        res <- data.frame(mean = c(apply(prob, 2, mean), apply(evs, 2, mean))
                          , cilo = c(apply(prob, 2, quantile, ci[1]), apply(evs, 2, quantile, ci[1]))
                          , cihi = c(apply(prob, 2, quantile, ci[2]), apply(evs, 2, quantile, ci[2]))
                          , dv = as.factor(sub("(.*) \\~.*", "\\1", models[[i]]@call[2]))
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , ivval = iv[,1]
                          , value = factor(rep(c("Probability P(y>0)","Expected Value E(y|y>0)"), each = nrow(iv))
                                           , levels = c("Probability P(y>0)","Expected Value E(y|y>0)")))
      } else stop("Check model type")
      out <- rbind(out, res)
      skip <- T
    }
    
    ## warning for Inf/-Inf in single iterations
    if(Inf %in% evs|-Inf %in% evs){
      warning(paste0("Inf/-Inf in ",length(evs[evs==Inf])+length(evs[evs==-Inf])," evs iteration(s)"))
      evs[evs==Inf|evs==-Inf] <- NA
    }
    
    if(!skip){
      ## generate output table
      if(class(models[[i]])[1] != "vglm"){
        res <- data.frame(mean = mean(evs)
                          , cilo = quantile(evs, ci[1])
                          , cihi = quantile(evs, ci[2])
                          , dv = as.factor(colnames(models[[i]]$model)[1])
                          , iv = as.factor(paste(colnames(iv), collapse = "_")))
      } else {
        res <- data.frame(mean = c(mean(prob, na.rm = T), mean(evs, na.rm = T))
                          , cilo = c(quantile(prob, ci[1], na.rm = T),quantile(evs, ci[1], na.rm = T))
                          , cihi = c(quantile(prob, ci[2], na.rm = T), quantile(evs, ci[2], na.rm = T))
                          , dv = as.factor(sub("(.*) \\~.*", "\\1", models[[i]]@call[2]))
                          , iv = as.factor(paste(colnames(iv), collapse = "_"))
                          , value = factor(c("Probability P(y>0)","Expected Value E(y|y>0)")
                                           , levels = c("Probability P(y>0)","Expected Value E(y|y>0)")))
      }
      out <- rbind(out, res)
    }
  }
  
  ## return output table
  rownames(out) <- NULL
  return(out)
}


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
