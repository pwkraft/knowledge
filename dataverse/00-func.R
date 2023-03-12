# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Load required packages and custom functions
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Required packages -------------------------------------------------------

## general purpose
library(tidyverse)
library(here)

## text analysis
library(discursive)
library(SnowballC)
library(stm)

## plots / tables
library(gridExtra)
library(xtable)





# Custom functions --------------------------------------------------------

## Plot defaults
plot_default <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill="white"))


## Remove empty open-ended responses, fix spelling

oe_clean <- function(x, spell = TRUE){
  x <- gsub("(^\\s+|\\s+$)","", x)
  x[x %in% oe_na] <- ""
  x <- gsub("//"," ", x , fixed = T) %>%
    gsub("\\s+"," ", .) %>%
    gsub("(^\\s+|\\s+$)","", .)

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
