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
  v1 = polknow_text_mean,
  v2 = polknow_factual) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge")
  ) + plot_default
ggsave("fig/cces2018_corplot.png",width=3.2, height=3.2)

## 2020 ANES
data2020 %>% transmute(
  v1 = polknow_text_mean,
  v2 = polknow_factual) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge")
  ) + plot_default
ggsave("fig/anes2020_corplot.png",width=3.2, height=3.2)

## 2016 ANES
data2016 %>% transmute(
  v1 = polknow_text_mean,
  v2 = polknow_factual,
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
  v1 = polknow_text_mean,
  v2 = polknow_factual,
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
  filter(wc > (median(wc) - 25) & wc < (median(wc) + 25),
         polknow_factual == 1) %>%
  filter((polknow_text_mean < quantile(polknow_text_mean,.25) & female == 0) |
           (polknow_text_mean > quantile(polknow_text_mean,.75) & female == 1)) %>%
  arrange(polknow_text_mean) %>%
  select(caseid, female, polknow_factual, polknow_text_mean) %>%
  #left_join(opend) %>%
  left_join(haven::read_sav("/data/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
              dplyr::select(caseid, UWM309, UWM310, UWM312, UWM313, UWM315,
                            UWM316, UWM318, UWM319, UWM321, UWM322)) %>%
  write_csv(file="calc/tmp/select_cces.csv")



# Validation: competence and engagement -----------------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("female", "educ", "faminc", "age", "black", "relig")

m1text <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(c("polknow_text_mean", ivs), response = "vote"),
           data = ., subset = !is.na(polknow_factual), family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_text_mean", ivs), response = "polint_att"),
          data = ., subset = !is.na(polknow_factual))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_text_mean", ivs), response = "effic_int"),
          data = ., subset = !is.na(polknow_factual))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_text_mean", ivs), response = "effic_ext"),
          data = ., subset = !is.na(polknow_factual))))

m1factual <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(c("polknow_factual", ivs), response = "vote"),
           data = ., subset = !is.na(polknow_text_mean), family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_factual", ivs), response = "polint_att"),
          data = ., subset = !is.na(polknow_text_mean))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_factual", ivs), response = "effic_int"),
          data = ., subset = !is.na(polknow_text_mean))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_factual", ivs), response = "effic_ext"),
          data = ., subset = !is.na(polknow_text_mean))))

c(m1text, m1factual) %>%
  map_dfr(~summary(marginaleffects(.)), .id = "model") %>%
  as_tibble() %>%
  filter(term %in% c("polknow_text_mean", "polknow_factual")) %>%
  mutate(
    study = factor(rep(c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES"), 8),
                   levels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES")),
    dv = recode_factor(rep(rep(dvs, each = 4), 2),
                       `vote` = "Turnout",
                       `polint_att` = "Political Interest",
                       `effic_int` = "Internal Efficacy",
                       `effic_ext` = "External Efficacy"),
    term = recode_factor(term,
                         `polknow_factual` = "Factual\nKnowledge",
                         `polknow_text_mean` = "Discursive\nSophistication")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(study~dv) +
  xlab("Average Marginal Effect") + ylab("Independent Variable") + plot_default
ggsave("fig/knoweff.pdf", width=6.5, height=4)

c(m1text[1], m1factual[1],
  m1text[5], m1factual[5],
  m1text[9], m1factual[9],
  m1text[13], m1factual[13]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2018 CES. Standard errors in parentheses. Estimates are used for
          Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(2,2,2,2),
            covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                                 "College Degree","Household Income","Age",
                                 "Black","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "ll"), font.size = "footnotesize",
            out = "tab/knoweff2018cces.tex", label = "tab:knoweff2018cces")

c(m1text[2], m1factual[2],
  m1text[6], m1factual[6],
  m1text[10], m1factual[10],
  m1text[14], m1factual[14]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2020 ANES. Standard errors in parentheses. Estimates are used for
          Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(2,2,2,2),
            covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                                 "College Degree","Household Income","Age",
                                 "Black","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "ll"), font.size = "footnotesize",
            out = "tab/knoweff2020anes.tex", label = "tab:knoweff2020anes")

c(m1text[3], m1factual[3],
  m1text[7], m1factual[7],
  m1text[11], m1factual[11],
  m1text[15], m1factual[15]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2016 ANES. Standard errors in parentheses. Estimates are used for
          Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(2,2,2,2),
            covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                                 "College Degree","Household Income","Age",
                                 "Black","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "ll"), font.size = "footnotesize",
            out = "tab/knoweff2016anes.tex", label = "tab:knoweff2016anes")

c(m1text[4], m1factual[4],
  m1text[8], m1factual[8],
  m1text[12], m1factual[12],
  m1text[16], m1factual[16]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2012 ANES. Standard errors in parentheses. Estimates are used for
          Figure \\ref{fig:knoweff} in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(2,2,2,2),
            covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                                 "College Degree","Household Income","Age",
                                 "Black","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "ll"), font.size = "footnotesize",
            out = "tab/knoweff2012anes.tex", label = "tab:knoweff2012anes")



# Validation: information retrieval ---------------------------------------

m2 <- list(
  lm(know_dis ~ polknow_text_mean + female + educ + faminc + age + black + relig, data = data_yg),
  lm(know_dis ~ polknow_factual + female + educ + faminc + age + black + relig, data = data_yg))

rbind(sim(m2[[1]], iv=data.frame(polknow_text_mean=seq(min(data_yg$polknow_text_mean),
                                                       max(data_yg$polknow_text_mean),
                                                       length=10))),
      sim(m2[[2]], iv=data.frame(polknow_factual=seq(0,1,length=10)))) %>%
  as_tibble() %>%
  mutate(Variable = recode_factor(iv,
                                  `polknow_text_mean` = "Discursive Sophistication",
                                  `polknow_factual` = "Factual Knowledge")) %>%
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
          column.separate = 2,
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Household Income","Age",
                               "Black","Church Attendance","Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "tab/yg_disease.tex", label = "tab:yg_disease")



# Validation: manual coding -----------------------------------------------

opend_cor <- tibble(
  cor = c(cor(opend_german$polknow_text_mean, opend_german$loj),
          cor(opend_french$polknow_text_mean, opend_french$loj),
          cor(opend_italian$polknow_text_mean, opend_italian$loj)),
  language = c("German", "French", "Italian"),
  polknow_text_mean = 0.05,
  loj = 4) %>%
  mutate(cor = paste0("r = ",round(cor, 2)))

rbind(data.frame(opend_german, language = "German")
      , data.frame(opend_french, language = "French")
      , data.frame(opend_italian, language = "Italian")) %>%
  ggplot(aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges.pdf",width=6,height=2)



# Gender gap: levels w/o controls -----------------------------------------

grid.arrange(
  data_cces %>%
    select(polknow_text_mean,
           polknow_factual,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `polknow_factual` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill=Variable)) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
    geom_point(aes(y=1), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    ggtitle("2018 CES"),
  data_yg %>%
    select(polknow_text_mean,
           polknow_factual,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `polknow_factual` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill=Variable)) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
    geom_point(aes(y=1), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    ggtitle("2015 YouGov"),
  data2020 %>%
    select(polknow_text_mean,
           polknow_factual,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `polknow_factual` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill=Variable)) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
    geom_point(aes(y=1), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    ggtitle("2020 ANES"),
  opend_german %>%
    select(polknow_text_mean,
           loj,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `loj` = "Level of\nJustification")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n),
              max = max(value)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill="Variable")) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable, scale="free_y") +
    geom_point(aes(y=max), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    labs(title="Swiss Survey",
         subtitle="German Respondents",
         y="Average Values", x=NULL),
  data2016 %>%
    select(polknow_text_mean,
           polknow_factual,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `polknow_factual` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill=Variable)) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
    geom_point(aes(y=1), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    ggtitle("2016 ANES"),
  opend_french %>%
    select(polknow_text_mean,
           loj,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `loj` = "Level of\nJustification")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n),
              max = max(value)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill="Variable")) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable, scale="free_y") +
    geom_point(aes(y=max), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    labs(title=" ",
         subtitle="French Respondents",
         y="Average Values", x=NULL),
  data2012 %>%
    select(polknow_text_mean,
           polknow_factual,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `polknow_factual` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill=Variable)) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
    geom_point(aes(y=1), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    ggtitle("2012 ANES"),
  opend_italian %>%
    select(polknow_text_mean,
           loj,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `polknow_text_mean` = "Discursive\nSophistication",
                                    `loj` = "Level of\nJustification")) %>%
    na.omit() %>%
    group_by(Variable, Gender) %>%
    summarize(avg = mean(value),
              sd = sd(value),
              n = n(),
              cilo = avg - 1.96*sd/sqrt(n),
              cihi = avg + 1.96*sd/sqrt(n),
              max = max(value)) %>%
    ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi, fill="Variable")) + plot_default +
    geom_bar(stat="identity") + geom_errorbar(width=.25) +
    facet_wrap(~Variable, scale="free_y") +
    geom_point(aes(y=max), col="white") +
    guides(fill="none") + scale_fill_brewer(palette="Paired") +
    labs(title="",
         subtitle="Italian Respondents",
         y="Average Values", x=NULL),
  ncol=2) %>%
  ggsave("fig/meandiff.pdf", plot = ., width=6.5, height=8)



# Gender gap: differences w/ controls -------------------------------------

ivs <- c("female", "educ", "faminc", "age", "black", "relig")

m3text <- list(data_cces, data2020, data2016, data2012, data_yg) %>%
  map(~lm(reformulate(ivs, response = "polknow_text_mean"),
          data = ., subset = !is.na(polknow_factual)))
m3factual <- list(data_cces, data2020, data2016, data2012, data_yg) %>%
  map(~lm(reformulate(ivs, response = "polknow_factual"),
          data = ., subset = !is.na(polknow_text_mean)))

c(m3text, m3factual) %>%
  map_dfr(~tidy(., conf.int = T), .id = "model") %>%
  filter(term == "female") %>%
  mutate(
    study = factor(rep(c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"), 2),
                   levels = c("2015 YouGov", "2020 ANES", "2018 CES", "2012 ANES", "2016 ANES")),
    dv = recode_factor(rep(c("polknow_text_mean","polknow_factual"), each = 5),
                       `polknow_text_mean` = "Discursive Sophistication",
                       `polknow_factual` = "Factual Knowledge")) %>%
  ggplot(aes(y=study, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(.~dv) +
  xlab("Estimated Gender Gap") + ylab("Dataset") + plot_default
ggsave("fig/determinants.pdf",width=5,height=2)

stargazer(m3text, type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Discursive Sophistication",
          title="Effects of gender on discursive sophistication in the CES, ANES, and YouGov study.
          Estimates are used for Figure \\ref{fig:determinants} in the main text.",
          column.labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
          covariate.labels = c("Female", "College Degree", "Household Income",
                               "Age", "Black", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "tab/determinants_text.tex", label = "tab:determinants_text")

stargazer(m3factual, type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Factual Knowledge",
          title="Effects of gender on factual knowledge in the CES, ANES, and YouGov study.
          Estimates are used for Figure \\ref{fig:determinants} in the main text.",
          column.labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
          covariate.labels = c("Female", "College Degree", "Household Income",
                               "Age", "Black", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "tab/determinants_factual.tex", label = "tab:determinants_factual")


# Explaining the gender gap -----------------------------------------------

## estimate topic prevalence effects
prep2012 <- estimateEffect(~ age + female + educ_cont + pid_cont + educ_pid,
                           stm_fit2012, meta = out2012$meta, uncertainty = "Global")
prep2016 <- estimateEffect(~ age + female + educ_cont + pid_cont + educ_pid,
                           stm_fit2016, meta = out2016$meta, uncertainty = "Global")
prep2020 <- estimateEffect(~ age + female + educ_cont + pid_cont + educ_pid,
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
                    labeltype = "prob", n=5, verbose.labels = F, width=50,
                    main = "2012 ANES")
plot.estimateEffect(prep2016, covariate = "female", topics = topics2016, model = stm_fit2016,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "prob", n=5, verbose.labels = F, width=50,
                    main = "2016 ANES")
plot.estimateEffect(prep2020, covariate = "female", topics = topics2020, model = stm_fit2020,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "prob", n=5, verbose.labels = F, width=50,
                    main = "2020 ANES")
dev.off()


