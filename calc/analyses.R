# ========================================================================= #
# Women Also Know Stuff
# - Code includes all analyses for updated paper + presentation
# - Author: Patrick Kraft
# - Date: 01/12/22
# ========================================================================= #



# Preamble ----------------------------------------------------------------

## Packages ----

library(tidyverse)
library(quanteda)
library(nFactors)
library(grid)
library(gridExtra)
library(cowplot)
library(ggridges)
library(GGally)
library(ggpubr)
library(ggbeeswarm)
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


## Total number of obs across studies ----
nrow(data_cces) + nrow(data_yg) +
  nrow(data2012) + nrow(data2016) + nrow(data2020) +
  nrow(opend_german) + nrow(opend_french) + nrow(opend_italian)



# Yearly citation count ---------------------------------------------------

read_csv("calc/in/PoPCites_summary.csv") %>%
  ggplot(aes(x = Year, y = Citations)) +
  geom_line() +
  ylab("Citations per Year") +
  xlab("") +
  theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill=NA)) +
  scale_x_continuous(breaks=seq(1995,2020,5)) +
  ylim(0,75)
ggsave("fig/cites.pdf", width=3.5, height=2.2)
ggsave("fig/cites.png", width=3.5, height=2.2)



# Factor analysis of sophistication components ----------------------------

tibble(Variable = c("Size","Range", "Constraint"),
       `2018 CES` = factanal(dplyr::select(data_cces, size, range, constraint),
                             1, rotation = "varimax")$loadings[,1],
       `2020 ANES` = factanal(dplyr::select(data2020, size, range, constraint),
                              1, rotation = "varimax")$loadings[,1],
       `2016 ANES` = factanal(dplyr::select(data2016, size, range, constraint),
                              1, rotation = "varimax")$loadings[,1],
       `2012 ANES` = factanal(dplyr::select(data2012, size, range, constraint),
                              1, rotation = "varimax")$loadings[,1]) %>%
  xtable(caption = "Factor Loadings of Discursive Sophistication Components",
         label = "app:factload", digits = 3, align = "llcccc") %>%
  print(file = "tab/factload.tex",
        caption.placement = "bottom",
        include.rownames = FALSE)

### Determine the number of factors
nfactors <- function(x) {
  ev <- eigen(cor(x, use = "complete.obs")) # get eigenvalues
  ap <- parallel(subject=nrow(x),var=ncol(x),
                 rep=100,cent=.05)
  nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
  plotnScree(nS)
}
nfactors(dplyr::select(data_cces, size, range, constraint))
nfactors(dplyr::select(data2020, size, range, constraint))
nfactors(dplyr::select(data2016, size, range, constraint))
nfactors(dplyr::select(data2012, size, range, constraint))



# Correlation matrices ----------------------------------------------------

## 2018 CES
data_cces %>% transmute(
  v1 = polknow_text_scale,
  v2 = polknow_factual_scale) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge")
  ) + plot_default
ggsave("fig/cces2018_corplot.png",width=3.2, height=3.2)

## 2020 ANES
data2020 %>% transmute(
  v1 = polknow_text_scale,
  v2 = polknow_factual_scale) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge")
  ) + plot_default
ggsave("fig/anes2020_corplot.png",width=3.2, height=3.2)

## 2016 ANES
data2016 %>% transmute(
  v1 = polknow_text_scale,
  v2 = polknow_factual_scale,
  v3 = polknow_evalpre) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge",
                           "Interviewer\nEvaluation")
  ) + plot_default
ggsave("fig/anes2016_corplot.png",width=3.2, height=3.2)

## 2012 ANES
data2012 %>% transmute(
  v1 = polknow_text_scale,
  v2 = polknow_factual_scale,
  v3 = polknow_evalpre) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge",
                           "Interviewer\nEvaluation")
          ) + plot_default
ggsave("fig/anes2012_corplot.png",width=3.2, height=3.2)



# Sample responses --------------------------------------------------------

data_cces %>%
  filter(wc > (median(wc) - 100) & wc < (median(wc) + 100),
         polknow_old == 1) %>%
  filter((polknow_text_scale < quantile(polknow_text_scale,.25) & female == 0) |
           (polknow_text_scale > quantile(polknow_text_scale,.75) & female == 1)) %>%
  arrange(polknow_text_scale) %>%
  select(caseid, female, polknow_old, polknow_text_scale) %>%
  #left_join(opend) %>%
  left_join(haven::read_sav("/data/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
              dplyr::select(caseid, UWM309, UWM310, UWM312, UWM313, UWM315,
                            UWM316, UWM318, UWM319, UWM321, UWM322)) %>%
  write_csv(file="calc/tmp/select_cces.csv")



# Validation: competence and engagement -----------------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("polknow_text_scale", "polknow_factual_scale",
         "female", "age", "black", "educ", "faminc", "relig"
         )

m1 <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(ivs, response = "vote"), data = ., family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "polint_att"), data = .)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_int"), data = .)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_ext"), data = .))
)

m1 %>%
  map_dfr(~tidy(comparisons(
    ., variables = list(polknow_text_scale = c(-1,1),
                        polknow_factual_scale = c(-1,1)))),
    .id = "model") %>%
  as_tibble() %>%
  mutate(
    study = factor(rep(rep(c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES"), each = 2), 4),
                   levels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES")),
    dv = recode_factor(rep(dvs, each = 8),
                       `vote` = "Turnout",
                       `polint_att` = "Political Interest",
                       `effic_int` = "Internal Efficacy",
                       `effic_ext` = "External Efficacy"),
    term = recode_factor(term,
                         `polknow_factual_scale` = "Factual\nKnowledge",
                         `polknow_text_scale` = "Discursive\nSophistication")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(study~dv) +
  xlab("Estimated Effect of Discursive Sophistication and Factual Knowledge\n(for an increase from 1 SD below mean to 1 SD above mean)") +
  ylab("Independent Variable") + plot_default
ggsave("fig/knoweff.pdf", width=6.5, height=4)


# Validation: interaction b/w measures and no controls --------------------

ivs <- c("polknow_text_scale * polknow_factual_scale",
         "female", "age", "black", "educ", "faminc", "relig")
m1int <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(ivs, response = "vote"), data = ., family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "polint_att"), data = .)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_int"), data = .)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_ext"), data = .))
)

ivs <- c("polknow_text_scale", "polknow_factual_scale")
m1noc <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(ivs, response = "vote"), data = ., family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "polint_att"), data = .)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_int"), data = .)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_ext"), data = .))
)

c(m1noc[1], m1[1], m1int[1],
  m1noc[5], m1[5], m1int[5],
  m1noc[9], m1[9], m1int[9],
  m1noc[13], m1[13], m1int[13]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2018 CES. Standard errors in parentheses. Estimates in model
          (2), (5), (8), and (11) are used for Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(3,3,3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "scriptsize",
            out = "tab/knoweff2018cces.tex", label = "tab:knoweff2018cces")

c(m1noc[2], m1[2], m1int[2],
  m1noc[6], m1[6], m1int[6],
  m1noc[10], m1[10], m1int[10],
  m1noc[14], m1[14], m1int[14]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2020 ANES. Standard errors in parentheses. Estimates in model
          (2), (5), (8), and (11) are used for Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(3,3,3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "scriptsize",
            out = "tab/knoweff2020anes.tex", label = "tab:knoweff2020anes")

c(m1noc[3], m1[3], m1int[3],
  m1noc[7], m1[7], m1int[7],
  m1noc[11], m1[11], m1int[11],
  m1noc[15], m1[15], m1int[15]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2016 ANES. Standard errors in parentheses. Estimates in model
          (2), (5), (8), and (11) are used for Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(3,3,3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "scriptsize",
            out = "tab/knoweff2016anes.tex", label = "tab:knoweff2016anes")

c(m1noc[4], m1[4], m1int[4],
  m1noc[8], m1[8], m1int[8],
  m1noc[12], m1[12], m1int[12],
  m1noc[16], m1[16], m1int[16]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2012 ANES. Standard errors in parentheses. Estimates in model
          (2), (5), (8), and (11) are used for Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(3,3,3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "scriptsize",
            out = "tab/knoweff2012anes.tex", label = "tab:knoweff2012anes")


# Validation: interaction by gender ---------------------------------------

ivs <- c("polknow_text_scale * polknow_factual_scale",
         "educ", "age", "black", "faminc", "relig")
c(map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(ivs, response = "vote"), family=binomial("logit"),
           data = ., subset = female == 1)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "polint_att"),
          data = ., subset = female == 1)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_int"),
          data = ., subset = female == 1)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_ext"),
          data = ., subset = female == 1))) %>%
  map(summary)

c(map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(ivs, response = "vote"), family=binomial("logit"),
           data = ., subset = female == 0)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "polint_att"),
          data = ., subset = female == 0)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_int"),
          data = ., subset = female == 0)),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(ivs, response = "effic_ext"),
          data = ., subset = female == 0))) %>%
  map(summary)


# Validation: information retrieval ---------------------------------------

m2 <- list(
  lm(know_dis ~ polknow_text_scale + polknow_factual_scale, data = data_yg),
  lm(know_dis ~ polknow_text_scale + polknow_factual_scale + female + age + black + educ + faminc + relig, data = data_yg),
  lm(know_dis ~ polknow_text_scale * polknow_factual_scale + female + age + black + educ + faminc + relig, data = data_yg)
)

bind_rows(
  plot_cap(m2[[2]], condition = c("polknow_text_scale"), draw = F) %>%
    transmute(iv = "polknow_text_scale", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
  plot_cap(m2[[2]], condition = c("polknow_factual_scale"), draw = F) %>%
    transmute(iv = "polknow_factual_scale", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
) %>%
  as_tibble() %>%
  mutate(Variable = recode_factor(iv,
                                  `polknow_text_scale` = "Discursive Sophistication",
                                  `polknow_factual_scale` = "Factual Knowledge")) %>%
  ggplot(aes(x=ivval, y=mean, ymin=cilo,ymax=cihi, lty=Variable, fill=Variable)) + plot_default +
  geom_ribbon(alpha=0.4, lwd=.1) + geom_line() +
  ylab("Information Retrieval") + xlab("Value of Independent Variable")
ggsave("fig/yg_disease.pdf", width=4, height=2)

stargazer(m2, type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
          title="Effects of sophistication on information retrieval in the 2015 YouGov study.
          Standard errors in parentheses. Estimates are used for Figure \\ref{fig:yg_disease}
          in the main text.",
          column.labels = "Information Retrieval",
          column.separate = 3,
          order = c(1,2,9,3:8,10),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "Age", "Black", "College Degree",
                               "Household Income","Church Attendance","Constant"),
          keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
          out = "tab/yg_disease.tex", label = "tab:yg_disease")



# Validation: manual coding -----------------------------------------------

opend_cor <- tibble(
  cor = c(cor(opend_german$polknow_text_scale, opend_german$loj),
          cor(opend_french$polknow_text_scale, opend_french$loj),
          cor(opend_italian$polknow_text_scale, opend_italian$loj)),
  language = c("German", "French", "Italian"),
  polknow_text_scale = -1,
  loj = 4) %>%
  mutate(cor = paste0("r = ",round(cor, 2)))

rbind(data.frame(opend_german, language = "German"),
      data.frame(opend_french, language = "French"),
      data.frame(opend_italian, language = "Italian")) %>%
  ggplot(aes(x=polknow_text_scale, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges.pdf",width=6,height=2)

rbind(data.frame(opend_german, language = "German"),
      data.frame(opend_french, language = "French"),
      data.frame(opend_italian, language = "Italian")) %>%
  ggplot(aes(x=polknow_text_scale, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_empty +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges0.pdf",width=6,height=2)

rbind(data.frame(opend_german, language = "German"),
      data.frame(opend_french, language = "French"),
      data.frame(opend_italian, language = "Italian")) %>%
  mutate(polknow_text_scale = ifelse(language == "German", NA, polknow_text_scale),
         polknow_text_scale = ifelse(language == "Italian", NA, polknow_text_scale)) %>%
  ggplot(aes(x=polknow_text_scale, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data = filter(opend_cor, language == "French"),
            aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges1.pdf",width=6,height=2)

rbind(data.frame(opend_german, language = "German"),
      data.frame(opend_french, language = "French"),
      data.frame(opend_italian, language = "Italian")) %>%
  mutate(polknow_text_scale = ifelse(language == "Italian", NA, polknow_text_scale)) %>%
  ggplot(aes(x=polknow_text_scale, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data = filter(opend_cor, language != "Italian"),
            aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges2.pdf",width=6,height=2)

rbind(data.frame(opend_german, language = "German"),
      data.frame(opend_french, language = "French"),
      data.frame(opend_italian, language = "Italian")) %>%
  ggplot(aes(x=polknow_text_scale, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges3.pdf",width=6,height=2)



# Gender gap: levels w/o controls -----------------------------------------

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-1.96*sd(x)/sqrt(length(x))
  ymax <- m+1.96*sd(x)/sqrt(length(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

grid.arrange(
  data_cces %>%
    select(polknow_text_scale,
           polknow_factual_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `polknow_factual_scale` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2018 CES"),
  data_yg %>%
    select(polknow_text_scale,
           polknow_factual_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `polknow_factual_scale` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2015 YouGov"),
  cowplot::get_legend(tibble(
    `Measurement Type` = factor(1:2, labels = c("Open-ended", "Conventional"))) %>%
      ggplot(aes(x = `Measurement Type`, fill = `Measurement Type`)) +
      geom_bar() +
      scale_fill_brewer(palette="Paired")),

  data2020 %>%
    select(polknow_text_scale,
           polknow_factual_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `polknow_factual_scale` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2020 ANES"),
  data2016 %>%
    select(polknow_text_scale,
           polknow_factual_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `polknow_factual_scale` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2016 ANES"),
  data2012 %>%
    select(polknow_text_scale,
           polknow_factual_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `polknow_factual_scale` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2012 ANES"),

  opend_french %>%
    select(polknow_text_scale,
           loj_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `loj_scale` = "Level of\nJustification")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col="Open-ended")) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 1.5,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    labs(title="Swiss Survey",
         subtitle="French Respondents",
         y=NULL, x=NULL),
  opend_german %>%
    select(polknow_text_scale,
           loj_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `loj_scale` = "Level of\nJustification")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col="Open-ended")) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 1.5,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    labs(title=" ",
         subtitle="German Respondents",
         y=NULL, x=NULL),
  opend_italian %>%
    select(polknow_text_scale,
           loj_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_scale` = "Discursive\nSophistication",
                                    `loj_scale` = "Level of\nJustification")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col="Open-ended")) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) +
    guides(col="none") + scale_color_brewer(palette="Paired") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 1.5,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    labs(title="",
         subtitle="Italian Respondents",
         y=NULL, x=NULL),
  ncol=3) %>%
  ggsave("fig/meandiff.pdf", plot = ., width=6.5, height=8)


# Gender gap: differences w/ controls -------------------------------------

ivs <- c("female", "age", "black", "educ", "faminc", "relig")
m3text <- list(data_cces, data2020, data2016, data2012, data_yg) %>%
  map(~lm(reformulate(ivs, response = "polknow_text_scale"),
          data = ., subset = !is.na(polknow_factual_scale)))
m3factual <- list(data_cces, data2020, data2016, data2012, data_yg) %>%
  map(~lm(reformulate(ivs, response = "polknow_factual_scale"),
          data = ., subset = !is.na(polknow_text_scale)))

ivs <- c("female", "age", "educ")
m3swiss <- list(opend_french, opend_german, opend_italian) %>%
  map(~lm(reformulate(ivs, response = "polknow_text_scale"), data = .))

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

bind_rows(
  map_tidy(m3text) %>%
    mutate(study = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
           dv = "Discursive Sophistication"),
  map_tidy(m3factual) %>%
    mutate(study = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
           dv = "Factual Knowledge"),
  map_tidy(m3swiss) %>%
    mutate(study = c("Swiss Survey\n(French)", "Swiss Survey\n(German)", "Swiss Survey\n(Italian)"),
           dv = "Discursive Sophistication")
) %>%
  mutate(
    study = factor(
      study,
      levels = rev(c("2015 YouGov", "2020 ANES", "2012 ANES", "2018 CES", "2016 ANES",
                     "Swiss Survey\n(Italian)", "Swiss Survey\n(French)", "Swiss Survey\n(German)"))
    ),
    dv = factor(
      dv,
      levels = c("Discursive Sophistication", "Factual Knowledge"))
  ) %>%
  ggplot(aes(y=study, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_vline(xintercept = -0.2, color="grey", lty="dashed") +
  geom_vline(xintercept = 0.2, color="grey", lty="dashed") +
  geom_point() + geom_errorbarh(height=0) +
  geom_errorbarh(aes(xmin=conf.low90, xmax=conf.high90), height=.2) +
  facet_wrap(.~dv) + xlim(-.75, .25) +
  xlab("Estimated Gender Gap") + ylab("Dataset") + plot_default
ggsave("fig/determinants.pdf",width=5,height=3)

stargazer(c(m3text, m3swiss), type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Discursive Sophistication",
          title="Effects of gender on discursive sophistication in the CES, ANES, and YouGov study.
          Estimates are used for Figure \\ref{fig:determinants} in the main text.",
          column.labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov",
                            "French", "German", "Italian"),
          covariate.labels = c("Female", "Age", "Black", "College Degree",
                               "Household Income", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "tiny",
          out = "tab/determinants_text.tex", label = "tab:determinants_text")

stargazer(m3factual, type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Factual Knowledge",
          title="Effects of gender on factual knowledge in the CES, ANES, and YouGov study.
          Estimates are used for Figure \\ref{fig:determinants} in the main text.",
          column.labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
          covariate.labels = c("Female", "Age", "Black", "College Degree",
                               "Household Income", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "scriptsize",
          out = "tab/determinants_factual.tex", label = "tab:determinants_factual")


# Explaining the gender gap -----------------------------------------------

## estimate topic prevalence effects
prep2012 <- estimateEffect(~ female + age + educ_cont + pid_cont + educ_pid,
                           stm_fit2012, meta = out2012$meta, uncertainty = "Global")
prep2016 <- estimateEffect(~ female + age + educ_cont + pid_cont + educ_pid,
                           stm_fit2016, meta = out2016$meta, uncertainty = "Global")
prep2020 <- estimateEffect(~ female + age + educ_cont + pid_cont + educ_pid,
                           stm_fit2020, meta = out2020$meta, uncertainty = "Global")

## select topics with largest gender effects
tmp2012 <- tibble(estimate = sapply(summary(prep2012)$tables,
                                    function(x) x["female","Estimate"]),
                  topics = prep2012$topics) %>% arrange(estimate)
topics2012 <- c(head(tmp2012$topics, 5), tail(tmp2012$topics, 5))
tmp2016 <- tibble(estimate = sapply(summary(prep2016)$tables,
                                    function(x) x["female","Estimate"]),
                  topics = prep2016$topics) %>% arrange(estimate)
topics2016 <- c(head(tmp2016$topics, 5), tail(tmp2016$topics, 5))
tmp2020 <- tibble(estimate = sapply(summary(prep2020)$tables,
                                    function(x) x["female","Estimate"]),
                  topics = prep2020$topics) %>% arrange(estimate)
topics2020 <- c(head(tmp2020$topics, 5), tail(tmp2020$topics, 5))

## plot gender differences in topic proportions
png("fig/stm_gender.png", height=5.5, width=6.5, units = "in", res = 400)
par(mfrow=c(3,1), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2012, covariate = "female", topics = topics2012, model = stm_fit2012,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2012 ANES")
plot.estimateEffect(prep2016, covariate = "female", topics = topics2016, model = stm_fit2016,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2016 ANES")
plot.estimateEffect(prep2020, covariate = "female", topics = topics2020, model = stm_fit2020,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2020 ANES")
dev.off()

png("fig/stm_gender1.png", height=3.5, width=7.5, units = "in", res = 400)
par(mfrow=c(1,1), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2020, covariate = "female", topics = topics2020, model = stm_fit2020,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2020 ANES")
dev.off()

png("fig/stm_gender0.png", height=3.5, width=7.5, units = "in", res = 400)
par(mfrow=c(1,1), mar=c(2.2,0.5,2.2,0.5))
plot(x=c(-.05,.02), y = 1:2,
     type = "n", xlim = c(-.05,.02), main = "2020 ANES",
     yaxt='n', xlab = "", ylab = "")
abline(v = 0, lty = "dashed")
dev.off()



