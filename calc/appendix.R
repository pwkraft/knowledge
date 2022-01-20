# ========================================================================= #
# Women Also Know Stuff
# - Code includes additional analyses for the online appendix
# - Author: Patrick Kraft
# - Date: 01/20/22
# ========================================================================= #



# Preamble ----------------------------------------------------------------

## Packages ----

library(tidyverse)
library(quanteda)
library(nFactors)
library(gridExtra)
library(ggridges)
library(GGally)
library(xtable)
library(stm)
library(broom)
library(marginaleffects)
library(stargazer)


## Load data ----

rm(list = ls())
load(here::here("calc/out/anes2012.Rdata"))
load(here::here("calc/out/anes2016.Rdata"))
load(here::here("calc/out/anes2020.Rdata"))
load(here::here("calc/out/swiss.Rdata"))
load(here::here("calc/out/yougov.Rdata"))
load(here::here("calc/out/cces.Rdata"))
source("calc/func.R")


## Plot defaults ----
plot_default <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill="white"))



# Word count plot ---------------------------------------------------------

wc_mean <- tibble(
  study = factor(1:6, labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                                 "2015 YouGov", "Swiss Survey")),
  wc = c(mean(data_cces$wc), mean(data2020$wc), mean(data2016$wc), mean(data2012$wc),
         mean(data_yg$wc), mean(c(opend_french$wc, opend_german$wc, opend_italian$wc)))
)

bind_rows(
  transmute(data_cces, wc = wc, study = 1),
  transmute(data2020, wc = wc, study = 2),
  transmute(data2016, wc = wc, study = 3),
  transmute(data2012, wc = wc, study = 4),
  transmute(data_yg, wc = wc, study = 5),
  transmute(opend_french, wc = wc, study = 6),
  transmute(opend_german, wc = wc, study = 6),
  transmute(opend_italian, wc = wc, study = 6)
) %>% mutate(
  study = factor(study,
                 labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                            "2015 YouGov", "Swiss Survey"))) %>%
  ggplot(aes(wc)) + geom_histogram(fill = "grey") + theme_classic(base_size = 8) +
  theme(panel.border = element_rect(fill=NA)) +
  facet_wrap(.~study, ncol = 2, scales = "free") +
  geom_vline(aes(xintercept = wc), colour="red", linetype = "longdash",data = wc_mean) +
  ylab("Number of Respondents") + xlab("Word Count")

ggsave("fig/wc.png", width = 3, height = 2)

