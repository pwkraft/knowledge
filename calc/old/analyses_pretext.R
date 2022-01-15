### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## preText analysis of ANES data (based on data created in prep.R)


rm(list = ls())
gc()

library(dplyr)
library(purrr)
library(quanteda)
library(stm)
library(ggplot2)
library(gridExtra)
library(preText)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

source("func.R")
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))

## load data and stm results
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")
load("out/yougov.Rdata")
load("out/swiss.Rdata")

## select raw documents
data <- list()
data$opend2012 <- apply(anes2012opend[anes2012opend$caseid %in% data2012$caseid[1:500], -1]
                        , 1, paste, collapse=' ')
data$opend2016 <- apply(anes2016opend[anes2016opend$V160001 %in% data2016$caseid[1:500], -1]
                        , 1, paste, collapse=' ')
data$opend_yougov <- apply(opend[opend$caseid %in% data_yg$caseid[1:500], -1]
                           , 1, paste, collapse=' ')
data$opend_german <- apply(cbind(opend_german$string1[1:500],opend_german$string2[1:500])
                           , 1, paste, collapse=' ')
data$opend_french <- apply(cbind(opend_french$string1[1:500],opend_french$string2[1:500])
                           , 1, paste, collapse=' ')
data$opend_italian <- apply(cbind(opend_italian$string1[1:500],opend_italian$string2[1:500])
                            , 1, paste, collapse=' ')

## preprocess data and run preText
res <- data %>% 
  map(factorial_preprocessing, use_ngrams = FALSE, parallel = TRUE, cores = 7) %>%
  map(preText, parallel = TRUE, cores = 7)

## remove intermediate dfms
file.remove(dir()[grep("intermediate_dfm_\\d+\\.Rdata", dir())])

## generate preText score plot
res %>% map(preText_score_plot)

## plot regression results
extractData <- function(x){
  out <- x[[2]]
  out$xmean <- x[[3]]$x
  out$ylab <- factor(out$y, labels = c("Lowercase","Remove Infrequent Terms","Remove Numbers"
                                       ,"Remove Punctuation","Remove Stopwords","Stemming"))
  out
}

plot_df <- res %>% 
  map(regression_coefficient_plot, remove_intercept = TRUE) %>% 
  map("data") %>% map_dfr(extractData) %>% 
  mutate(data = rep(names(res), each=nrow(.)/length(res))) %>%
  mutate(datalab = factor(data, levels = c("opend2012","opend2016","opend_yougov"
                                           ,"opend_german","opend_french","opend_italian")
       , labels = c("2012 ANES","2016 ANES","2015 YouGov"
                    ,"Swiss (German)","Swiss (French)","Swiss (Italian)")))

ggplot(plot_df, aes(x = xmean, xmin = x, xmax = xend, y = ylab)) +
  geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
  facet_wrap(~datalab, ncol=2) + labs(y=NULL, x = "Regression Coefficient") + plot_default
ggsave("../fig/pretext.pdf",width = 6, height = 4)



###################
### Replicate measure with varying model specifications + pre-processing choices
###################


robustSoph <- function(data, k, k_original, label
                       , thresh = 10, stem = TRUE, lang = "english"
                       , meta = c("age","educ_cont","pid_cont","educ_pid","female")
                       , seed = 12345){
  ### function to estimate stm and compute discursive sophistication w/ varying model specifications
  # data: original dataset containing the following variables:
  #       - polknow_text_mean: original discursive sophistication measure used in main analyses
  #       - resp: merged open-ended responses (minor pre-processing applied)
  #       - ditem: opinionation component of discursive sophistication
  #       - all variables listed in [meta]
  # k: number of topics used for stm estimation
  # k_original: number of topics in original stm
  # label: label for dataset
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
  processed <- textProcessor(data$resp, metadata = data[,meta], stem = stem, language = lang
                             , customstopwords = c("dont", "hes", "shes", "that", "etc")
                             )
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = thresh)
  
  ## remove discarded observations from data
  if(length(processed$docs.removed)>0) data <- data[-processed$docs.removed,]
  if(length(out$docs.removed)>0) data <- data[-out$docs.removed,]
  
  ## stm fit
  stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
                 , K=k, init.type = "Spectral", seed=seed)
  
  ## compute sophistication components
  know <- sophistication(stm_fit, out)
  know$polknow_text_mean_rep <- (know$ntopics + know$distinct + data$ditem)/3
  
  ## compute discursive_sophistication
  out <- tibble(polknow_text_mean = data$polknow_text_mean,
                polknow_text_mean_rep = know$polknow_text_mean_rep,
                k = paste0("Fewer Topics (k = ",k,")"),
                k_original = paste0("k = ",k_original,collapse = ""),
                datalab = label, thresh = thresh, stem = stem)
  out
}

## clear workspace
try(rm(anes2012, anes2012opend, anes2012spell, anes2016, anes2016opend, anes2016spell
       , data, extractData, hetreg_summary2012, hetreg_summary2016, latexTable, meta, meta2012
       , meta2016, opend, out, out_yougov, out2012, out2016, plot_df, processed, processed_yougov
       , processed2012, processed2016, res, swiss, yougov))
gc()

## compare discursive sophistication for different model specifications (save intermediate steps)
plot_df <- bind_rows(
  robustSoph(data2012, 20, stm_fit2012$settings$dim$K, "2012 ANES")
  , robustSoph(data2016, 20, stm_fit2016$settings$dim$K, "2016 ANES")
  , robustSoph(data_yg, 20, stm_fit$settings$dim$K, "2015 YouGov"))
save(plot_df, file = "tmp/tmp01.Rdata")

plot_df <- plot_df %>% bind_rows(
  robustSoph(opend_german, 20, stm_fit_german$settings$dim$K, "Swiss (German)"
             , lang = "german", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
  , robustSoph(opend_french, 20, stm_fit_french$settings$dim$K, "Swiss (French)"
               , lang = "french", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
  , robustSoph(opend_italian, 20, stm_fit_italian$settings$dim$K, "Swiss (Italian)"
               , lang = "italian", meta = c("age", "edu", "ideol", "edu_ideol", "female")))
save(plot_df, file = "tmp/tmp02.Rdata")  
  
plot_df <- plot_df %>% bind_rows(
  robustSoph(data2012, 20, stm_fit2012$settings$dim$K, "2012 ANES", thresh=1)
  , robustSoph(data2016, 20, stm_fit2016$settings$dim$K, "2016 ANES", thresh=1)
  , robustSoph(data_yg, 20, stm_fit$settings$dim$K, "2015 YouGov", thresh=1))
save(plot_df, file = "tmp/tmp03.Rdata")  

plot_df <- plot_df %>% bind_rows(
  robustSoph(opend_german, 20, stm_fit_german$settings$dim$K, "Swiss (German)", thresh=1
             , lang = "german", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
  , robustSoph(opend_french, 20, stm_fit_french$settings$dim$K, "Swiss (French)", thresh=1
               , lang = "french", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
  , robustSoph(opend_italian, 20, stm_fit_italian$settings$dim$K, "Swiss (Italian)", thresh=1
               , lang = "italian", meta = c("age", "edu", "ideol", "edu_ideol", "female")))
save(plot_df, file = "tmp/tmp04.Rdata")  

plot_df <- plot_df %>% bind_rows(
  robustSoph(data2012, 20, stm_fit2012$settings$dim$K, "2012 ANES", stem = FALSE)
  , robustSoph(data2016, 20, stm_fit2016$settings$dim$K, "2016 ANES", stem = FALSE)
  , robustSoph(data_yg, 20, stm_fit$settings$dim$K, "2015 YouGov", stem = FALSE))
save(plot_df, file = "tmp/tmp05.Rdata")

plot_df <- plot_df %>% bind_rows(
  robustSoph(opend_german, 20, stm_fit_german$settings$dim$K, "Swiss (German)", stem = FALSE
             , lang = "german", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
  , robustSoph(opend_french, 20, stm_fit_french$settings$dim$K, "Swiss (French)", stem = FALSE
               , lang = "french", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
  , robustSoph(opend_italian, 20, stm_fit_italian$settings$dim$K, "Swiss (Italian)", stem = FALSE
               , lang = "italian", meta = c("age", "edu", "ideol", "edu_ideol", "female")))
save(plot_df, file = "tmp/tmp06.Rdata")

## prepare data for plotting
plot_df <- plot_df %>% 
  mutate(datalab = factor(datalab, levels = c("2012 ANES","2016 ANES","2015 YouGov"
                                              ,"Swiss (French)","Swiss (German)","Swiss (Italian)"))
         , condition = factor(stem + thresh, levels = c("11","2","10")
                              , labels = c("","+ Include Infrequent Terms"
                                           ,"+ No Stemming"))
         , combined = paste(k, condition, sep="\n"))

## compute correlations for subgroups
plot_cor <- plot_df %>%
  group_by(datalab, k_original, combined) %>%
  summarize(cor = paste0("r = ",round(cor(polknow_text_mean, polknow_text_mean_rep), 2))) %>%
  mutate(polknow_text_mean = .9, polknow_text_mean_rep = .1)

## generate plot
ggplot(plot_df, aes(y=polknow_text_mean, x=polknow_text_mean_rep)) + 
  geom_point(alpha=.05) + geom_smooth(method="lm") + facet_grid(datalab+k_original~combined) + 
  geom_text(data=plot_cor, aes(label=cor), size=2) + xlim(0,1) + ylim(0,1) + 
  labs(y = "Discursive Sophistication (Preferred Specification)",
       x = "Discursive Sophistication (Alternative Specifications)") + 
  plot_default
ggsave("../fig/pretext_robustness.pdf", width=6, height=8)

