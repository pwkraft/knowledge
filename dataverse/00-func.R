# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Load required packages and custom functions
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Required packages -------------------------------------------------------

## general purpose
library(tidyverse)
library(GGally)
library(here)

## text analysis
library(discursive)
library(SnowballC)
library(cld2)
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

## quickly declare missing values
na_in <- function(x, y) {
  x[x %in% y] <- NA
  x
}

## Remove empty open-ended responses, fix spelling
oe_clean <- function(x, spell = TRUE, tolower = FALSE){
  if(tolower){
    x <- tolower(x)
    oe_na <- unique(tolower(oe_na))
  }

  x <- gsub("(^\\s+|\\s+$)","", x)
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
