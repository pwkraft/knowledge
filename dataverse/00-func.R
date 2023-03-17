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
