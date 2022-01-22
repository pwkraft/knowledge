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
ggsave("fig/wc.png", width = 6, height = 5)



# Plot non-response by gender ---------------------------------------------

bind_rows(
  transmute(cces, noresp = wc == 0, female = female, study = 1),
  transmute(anes2020, noresp = wc == 0, female = female, study = 2),
  transmute(anes2016, noresp = wc == 0, female = female, study = 3),
  transmute(anes2012, noresp = wc == 0, female = female, study = 4),
  transmute(yougov, noresp = wc == 0, female = female, study = 5),
  transmute(swiss, noresp = wc == 0, female = female, study = 6)
) %>% mutate(
  study = factor(study,
                 labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                            "2015 YouGov", "Swiss Survey")),
  Gender = factor(female, labels = c("Male","Female"))) %>%
  na.omit() %>%
  group_by(study, Gender) %>%
  summarize(avg = mean(noresp),
            sd = sd(noresp),
            n = n(),
            cilo = avg - 1.96*sd/sqrt(n),
            cihi = avg + 1.96*sd/sqrt(n)) %>%
  ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi)) + plot_default +
  geom_bar(stat="identity", fill="grey") + geom_errorbar(width=.1) +
  facet_wrap(~study, ncol = 2) + ylab("Average Values") + xlab(NULL) +
  guides(fill="none") + scale_fill_brewer(palette="Paired")
ggsave("fig/noresponse.png", width = 6, height = 5)

transmute(cces, noresp = wc == 0, female = female) %>%
  t.test(noresp ~ female, data = .)



# STM topic proportions ---------------------------------------------------

pdf("fig/stm_prop.pdf", width=12, height=10)
par(mfrow = c(2,4), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit_cces, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2018 CES (k = ", stm_fit_cces$settings$dim$K,")", collapse = ""))
plot(stm_fit2020, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2020 ANES (k = ", stm_fit2020$settings$dim$K,")", collapse = ""))
plot(stm_fit2016, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2016 ANES (k = ", stm_fit2016$settings$dim$K,")", collapse = ""))
plot(stm_fit2012, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2012 ANES (k = ", stm_fit2012$settings$dim$K,")", collapse = ""))
plot(stm_fit_yg, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2015 YouGov (k = ", stm_fit_yg$settings$dim$K,")", collapse = ""))
plot(stm_fit_french, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - French (k = ", stm_fit_french$settings$dim$K,")", collapse = ""))
plot(stm_fit_german, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - German (k = ", stm_fit_german$settings$dim$K,")", collapse = ""))
plot(stm_fit_italian, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - Italian (k = ", stm_fit_italian$settings$dim$K,")", collapse = ""))
dev.off()




