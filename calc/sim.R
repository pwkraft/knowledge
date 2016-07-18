### Function to calculate expected values/first differences (replaces Zelig call)
library(MASS)

sim <- function(models, iv, robust=F, ci=c(0.025,0.975)){
  ## function to calculate first differences in predicted probabilities for probit model
  ## models: list of glm probit models
  ## iv: data frame containing the values for comparison (only 2 rows, selected variables)
  
  ## prepare output object, convert input to model list
  out <- NULL
  if(class(models)[1] != "list") models <- list(models)
  
  for(i in 1:length(models)){
    ## simulate betas from sampling distribution
    if(robust == T){
      betas <- mvrnorm(1000, coef(models[[i]]), vcovHC(models[[i]]))
    } else {
      betas <- mvrnorm(1000, coef(models[[i]]), vcov(models[[i]]))
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
      means <- apply(models[[i]]$data[vars[-c(1,which(vars %in% names(iv)),int)]]
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
      ## CONTINUE HERE, check whether this is correct
      ## IDEA: each original evs column as a new matrix where each column is an individual
      ## simulation with the given mean of the row of the original matrix.
      ## then decompose effect of tobit in dP(Y>0) and dY|Y>0
      evs <- matrix(rnorm(nrow(betas)*100, betas[,-2] %*% X[-2,]
                          , sd = exp(betas[,2])), ncol = 2)
      evs2 <- apply(evs, 2, function(x) x)
      
      unique(models[[i]]@misc$Lower)
    } else stop("Model type not supported")
    
    if(nrow(iv)==2){
      ## calculate first differences
      evs <- evs[,2] - evs[,1]
    } else if(nrow(iv)==4) {
      ## calculate difference-in-difference
      evs <- (evs[,2] - evs[,1]) - (evs[,4] - evs[,3])
    } else {
      warning("Check number of scenarios")
    }
    
    ## generate output table
    res <- data.frame(mean = mean(evs)
                      , cilo = quantile(evs, ci[1])
                      , cihi = quantile(evs, ci[2])
                      , dv = as.factor(colnames(models[[i]]$model)[1])
                      , iv = as.factor(paste(colnames(iv), collapse = "_")))
    out <- rbind(out, res)
  }
  
  ## return output table
  rownames(out) <- NULL
  return(out)
}