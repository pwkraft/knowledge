# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Load required packages and custom functions
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Required packages -------------------------------------------------------

## general purpose
library(tidyverse)
library(here)
library(broom)
library(marginaleffects)

## text analysis
library(discursive)
library(SnowballC)
library(cld2)
library(stm)
library(preText)

## plots / tables
library(ggpubr)
library(GGally)
library(cowplot)
library(ggridges)
library(gridExtra)
library(ggbeeswarm)
library(stargazer)
library(xtable)



# Custom functions --------------------------------------------------------

## Plot defaults
plot_default <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill="white"))

## Declare multiple missing values
na_in <- function(x, y) {
  x[x %in% y] <- NA
  x
}

## Compute data summary
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-1.96*sd(x)/sqrt(length(x))
  ymax <- m+1.96*sd(x)/sqrt(length(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

## Extract model coefficients with 90% & 95% CIs
map_tidy <- function(x, iv = "female"){
  left_join(
    x %>%
      map_dfr(~tidy(., conf.int = T), .id = "model"),
    x %>%
      map_dfr(~tidy(., conf.int = T, conf.level = 0.90), .id = "model") %>%
      transmute(model = model, term = term, conf.low90 = conf.low, conf.high90 = conf.high)
  ) %>%
    filter(term == iv)
}

## Remove empty open-ended responses, fix spelling
oe_clean <- function(x, spell = TRUE, tolower = FALSE){
  if(tolower){
    x <- tolower(x)
    oe_na <- unique(tolower(oe_na))
  }

  x <- gsub("(^\\s+|\\s+$)","", x)
  x[is.na(x)] <- ""
  x[x %in% oe_na] <- ""
  x <- gsub("//", " ", x, fixed = T) %>%
    gsub("\\", " ", ., fixed = T) %>%
    gsub("...", " ", ., fixed = T) %>%
    gsub("/", " ", ., fixed = T) %>%
    gsub("\\s+", " ", .) %>%
    gsub("(^\\s+|\\s+$)", "", .)

  if(spell){
    tmp <- tempfile(fileext = ".csv")
    write.table(x, file = tmp, sep = ",", col.names = F, row.names = F)
    spell <- aspell(tmp) %>%
      filter(Suggestions != "NULL")
    for(i in 1:nrow(spell)){
      x[spell$Line[i]] <- gsub(spell$Original[i],
                               unlist(spell$Suggestions[i])[1],
                               x[spell$Line[i]])
    }
    unlink(tmp)
  }
  x
}

## Select & combine sample of open-ended responses (for preText)
oe_sample <- function(x, n = 500) {
  x %>%
    filter(wc != 0) %>%
    slice_sample(n = n) %>%
    select(starts_with("oe_")) %>%
    apply(1, paste, collapse = " ")
}

## Compare variance & non-response in ideological placements
ideo_compare <- function(yname, xname, data, quantiles = c(.25, .75)) {
  y <- data.frame(data)[, yname]
  x <- data.frame(data)[, xname]
  x_lim <- quantile(x, probs = quantiles, na.rm = T)
  y_lo <- y[x <= x_lim[1]]
  y_hi <- y[x > x_lim[2]]
  y_lo_na <- as.numeric(is.na(y_lo))
  y_hi_na <- as.numeric(is.na(y_hi))

  dplyr::tibble(
    dv = yname,
    iv = xname,
    x_lo = x_lim[1],
    x_hi = x_lim[2],
    n_lo = length(y_lo),
    n_hi = length(y_hi),
    sd_lo = sd(y_lo, na.rm = T),
    sd_hi = sd(y_hi, na.rm = T),
    sd_pval = var.test(y_lo, y_hi)$p.value,
    na_lo = mean(y_lo_na, na.rm = T),
    na_hi = mean(y_hi_na, na.rm = T),
    na_pval = t.test(y_lo_na, y_hi_na)$p.value
  ) %>%
    mutate(
      sd_stars = case_when(
        sd_pval < .001 ~ "***",
        sd_pval < .01 ~ "**",
        sd_pval < .05 ~ "*",
        sd_pval >= .05 ~ "ns"
      ),
      na_stars = case_when(
        na_pval < .001 ~ "***",
        na_pval < .01 ~ "**",
        na_pval < .05 ~ "*",
        na_pval >= .05 ~ "ns"
      )
    )
}

## Wrapper to compute discursive sophistication w/ varying model specifications
robust_discursive <- function(data, datalab,
                              K = 25, stem = TRUE, removestopwords = TRUE,
                              language = "english", dictionary = dict_constraint$en,
                              meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                              seed = 12345, verbose = FALSE){
  cat("\nProcessing ", datalab,
      ": K = ", K,
      ", stem = ", stem,
      ", removestopwords = ", removestopwords,
      sep = "")
  discursive(data = data,
             openends = colnames(data)[grep("oe_", colnames(data))],
             meta = meta,
             args_textProcessor = list(stem = stem,
                                       removestopwords = removestopwords,
                                       language = language,
                                       verbose = verbose),
             args_prepDocuments = list(lower.thresh = 10,
                                       verbose = verbose),
             args_stm = list(K = K,
                             seed = seed,
                             verbose = verbose),
             keep_stm = FALSE,
             dictionary = dictionary) %>%
    discard(is.null) %>%
    flatten() %>%
    as_tibble() %>%
    transmute(original = data$discursive,
              replication = discursive,
              K = K,
              stem = stem,
              removestopwords = removestopwords,
              datalab = datalab) %>%
    na.omit()
}
