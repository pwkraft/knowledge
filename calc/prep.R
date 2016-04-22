### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## This file prepares the survey data of the 2008 and 2012 ANES for subsequent analyses



### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(readstata13)
raw2012 <- read.dta13("../data/anes_timeseries_2012.dta", convert.factors = F)
raw2008 <- read.dta13("../data/anes_timeseries_2008.dta", convert.factors = F)



### define auxiliary functions

zero_one <- function(x){
    ## transforms a variable to 0-1 range
    x <- (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
    return(x)
}



### 2012 data

## respondent id
anes2012 <- data.frame(id=raw2012$caseid)

## ideology (factor/dummies)
anes2012$ideol <- factor(recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                  , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numerdic(anes2012$libcpre_self=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$libcpre_self=="Conservative")

## ideology (continuous)
anes2012$ideol_ct <- zero_one(recode(raw2012$libcpre_self, "lo:0=NA"))

## strength of ideology
anes2012$ideol_str <- zero_one(abs(recode(raw2012[,libcpre_self], "lo:0=NA") - 4))
anes2012$ideol_str_c <- anes2012$ideol_str - mean(anes2012$ideol_str, na.rm = T)

## religiosity (church attendance)
anes2012$relig <- - recode(raw2012$relig_churchoft, "lo:0 = NA") + 5
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 5

## education (bachelor degree)
anes2012$educ <- as.numeric(raw2012$dem_edugroup_x >= 4)
anes2012$educ[raw2012$raw2012$dem_edugroup_x < 0] <- NA

## age
anes2012$age <- recode(raw2012$dem_age_r_x, "c(-2,-9,-8) = NA")

## sex
anes2012$female <- raw2012$gender_respondent_x - 1

## race
anes2012$black <- as.numeric(recode(raw2012dem_raceeth_x, "lo:0 = NA") == 2)

## spanish speaking respondent

## political knowledge (open factual knowledge questions)

## political knowledge (closes factua knowledge questions)

## political knowledge (interviewer evaluation)

## idea: compare influence of OE measure vs. factual measure on interviewer evaluation?
## measures that we need from the text data: length of response, lexical diversity, readability

### recode 2008 data


