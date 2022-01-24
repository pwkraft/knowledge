###########################################################################################
## Project:  Measuring Political Sophistication using Open-ended Responses
## File:     func.R
## Overview: functions used in prep_anes.R, analyses_anes.R, ...
## Author:   Patrick Kraft
###########################################################################################


## function to compute -1/+1 SD range around mean
sdrange <- function(x){
  mean(x, na.rm = T) + c(-1,1) * sd(x, na.rm = T)
}

### compute shannon entropy
shannon <- function(x){
  -sum(log(x^x)/log(length(x)))
}

## quickly declare missing values
na_in <- function(x, y) {
  x[x %in% y] <- NA
  x
}

### Compute number of topics based on stm results
#' built: 2018-01-11, Patrick Kraft
#' updated: 2021-06-18, Patrick Kraft
#' @import stm
#' @param x: stm model result
#' @return data.frame: estimated number of topics and distinctiveness of word choice
#' @export

ntopics <- function(x, docs){
  if(class(x)!="STM") stop("x must be an STM object")

  ## P(t|X): probability of topic t given covariates X [nobs,ntopics]
  pt_x <- x$theta

  ## P(w|t): probability of word w given topic t [ntopics,nwords]
  pw_t <- exp(x$beta$logbeta[[1]])
  # logbeta: list containing the log of the word probabilities for each topic

  ## P(w|X) = sum_j(P(w|t_j)P(t_j|X)): probability of word w across all topics [nobs,nwords]
  pw_x <- pt_x %*% pw_t

  ## P(t_i|w,X) = P(w|t_i)P(t_i|X)/P(w|X): probability of topic t given word w and covariates X
  nobs <- nrow(pt_x)
  nwords <- ncol(pw_t)
  ntopics <- ncol(pt_x)
  pt_wx <- array(NA, c(nobs, nwords, ntopics))

  cat("\nComputing P(t_i|w,X):\n")
  pb <- txtProgressBar(min = 1, max = nwords, style = 3)
  for(w in 1:nwords){
    for(t in 1:ntopics){
      pt_wx[,w,t] <- pw_t[t,w] * pt_x[,t] / pw_x[,w]
    }
    setTxtProgressBar(pb, w)
  }
  close(pb)

  ## compute sophistication components
  cat("\nComputing sophistication components:\n")
  ntopics <- rep(NA, nobs)
  pb <- txtProgressBar(min = 1, max = nobs, style = 3)
  for(n in 1:nobs){
    maxtopic_wx <- apply(pt_wx[n,,],1,which.max) # which topic has the highest probability for each word (given X)
    ntopics[n] <- length(unique(maxtopic_wx[docs$documents[[n]][1,]])) # number of topics in response
    setTxtProgressBar(pb, n)
  }
  cat("\n\n")

  ## return rescaled measure
  return(ntopics/max(ntopics))
}


### Compute discursive sophistication components based on stm results
#' built: 2018-01-11, Patrick Kraft
#' @import stm
#' @param x: stm model result
#' @return data.frame: estimated number of topics and distinctiveness of word choice
#' @export

sophistication <- function(x, docs){
  if(class(x)!="STM") stop("x must be an STM object")

  ## P(t|X): probability of topic t given covariates X [nobs,ntopics]
  pt_x <- x$theta

  ## P(w|t): probability of word w given topic t [ntopics,nwords]
  pw_t <- exp(x$beta$logbeta[[1]])
  # logbeta: list containing the log of the word probabilities for each topic

  ## P(w|X) = sum_j(P(w|t_j)P(t_j|X)): probability of word w across all topics [nobs,nwords]
  pw_x <- pt_x %*% pw_t

  ## P(t_i|w,X) = P(w|t_i)P(t_i|X)/P(w|X): probability of topic t given word w and covariates X
  nobs <- nrow(pt_x)
  nwords <- ncol(pw_t)
  ntopics <- ncol(pt_x)
  pt_wx <- array(NA, c(nobs, nwords, ntopics))

  cat("\nComputing P(t_i|w,X):\n")
  pb <- txtProgressBar(min = 1, max = nwords, style = 3)
  for(w in 1:nwords){
    for(t in 1:ntopics){
      pt_wx[,w,t] <- pw_t[t,w] * pt_x[,t] / pw_x[,w]
    }
    setTxtProgressBar(pb, w)
  }
  close(pb)

  ## compute sophistication components
  cat("\nComputing sophistication components:\n")
  know <- data.frame(ntopics = rep(NA, nobs), distinct = rep(NA, nobs))
  pb <- txtProgressBar(min = 1, max = nobs, style = 3)
  for(n in 1:nobs){
    maxtopic_wx <- apply(pt_wx[n,,],1,which.max) # which topic has the highest probability for each word (given X)
    wordprob_t <- diag(pw_t[maxtopic_wx,]) # what is the probability of the word given the assigned topic

    know$ntopics[n] <- length(unique(maxtopic_wx[docs$documents[[n]][1,]])) # number of topics in response
    know$distinct[n] <- sum(wordprob_t[docs$documents[[n]][1,]] * docs$documents[[n]][2,]) # sum of word probabilities
    setTxtProgressBar(pb, n)
  }
  cat("\n\n")

  ## rescaling
  know$ntopics <- know$ntopics/max(know$ntopics)
  know$distinct <- know$distinct/max(know$distinct)
  # scale minimum to zero?
  # know$distinct <- (know$distinct-min(know$distinct))/(max(know$distinct)-min(know$distinct))

  know
}

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
                       , varlabs=NULL, mlabs=NULL, cluster=NULL,...){
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
    if(!is.null(cluster)){
      se <- paste0("(",round(sqrt(diag(vcovCL(x[[m]], cluster=cluster))), digits),")")
    } else {
      se <- paste0("(",round(sqrt(diag(vcov(x[[m]]))), digits),")")
    }
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


robustSoph <- function(data, k, k_original, label, removestopwords = TRUE,
                       stem = TRUE, thresh = 10, lang = "english",
                       meta = c("age","educ_cont","pid_cont","educ_pid","female"),
                       seed = 12345){
  ### function to estimate stm and compute discursive sophistication w/ varying model specifications
  # data: original dataset containing the following variables:
  #       - polknow_text_mean: original discursive sophistication measure used in main analyses
  #       - resp: merged open-ended responses (minor pre-processing applied)
  #       - ditem: opinionation component of discursive sophistication
  #       - all variables listed in [meta]
  # k: number of topics used for stm estimation
  # k_original: number of topics in original stm
  # label: label for dataset
  # removestopwords: (logical) whether stop words should be removed
  # thresh: lower threshold in prepDocuments for minimum number of docs each term has to appear in
  # stem: (logical) whether or not to stem words in textProcessor
  # lang: language used in textProcessor
  # meta: variable names (in data) that are used as prevalence covariates in stm estimation
  # seed: seed used for stm estimation
  ###

  ### garbage collection
  gc()

  ## remove missings on metadata
  data <- data[apply(!is.na(data[,meta]),1,prod)==1,]

  ## process for stm
  processed <- textProcessor(data$resp, metadata = data[,meta], stem = stem, language = lang,
                             removestopwords = removestopwords,
                             customstopwords = c("dont", "hes", "shes", "that", "etc")
  )
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = thresh)

  ## remove discarded observations from data
  if(length(processed$docs.removed)>0) data <- data[-processed$docs.removed,]
  if(length(out$docs.removed)>0) data <- data[-out$docs.removed,]

  ## stm fit
  stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
                 , K=k, init.type = "Spectral", seed=seed)

  ## compute number of considerations
  data$size <- ntopics(stm_fit, out)
  data$polknow_text_rep <- with(data, size + range + constraint)/3

  ## compute discursive_sophistication
  out <- tibble(datalab = label,
                k_original = k_original, k = k,
                removestopwords = removestopwords,
                stem = stem, thresh = thresh,
                polknow_text_mean = data$polknow_text_mean,
                polknow_text_rep = data$polknow_text_rep)
  out
}
