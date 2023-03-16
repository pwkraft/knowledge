# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Main analyses reported in manuscript (+ appendix tables)
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Load raw data, packages, and custom functions ---------------------------

source(here::here("00-func.R"))
load(here("data/processed.Rdata"))


# Table 1: Factor Loadings of Discursive Sophistication Components --------

tibble(Variable = c("Size","Range", "Constraint"),
       `2018 CES` = factanal(na.omit(select(ces2018, size, range, constraint)),
                             1, rotation = "varimax")$loadings[,1],
       `2020 ANES` = factanal(na.omit(select(anes2020, size, range, constraint)),
                              1, rotation = "varimax")$loadings[,1],
       `2016 ANES` = factanal(na.omit(select(anes2016, size, range, constraint)),
                              1, rotation = "varimax")$loadings[,1],
       `2012 ANES` = factanal(na.omit(select(anes2012, size, range, constraint)),
                              1, rotation = "varimax")$loadings[,1]) %>%
  xtable(caption = "Factor Loadings of Discursive Sophistication Components",
         label = "tab:factload", digits = 3, align = "llcccc") %>%
  print(file = here("out/tab1-factload.tex"),
        caption.placement = "bottom",
        include.rownames = FALSE)


# Figure 1: Correlation matrix of discursive sophistication and co --------

## 2018 CES
ces2018 %>% transmute(
  v1 = discursive,
  v2 = polknow) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge")
  ) + plot_default
ggsave(here("out/fig1a-corplot_cces2018.png"),width=3.2, height=3.2)

## 2020 ANES
anes2020 %>% transmute(
  v1 = discursive,
  v2 = polknow) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge")
  ) + plot_default
ggsave(here("out/fig1b-corplot_anes2020.png"),width=3.2, height=3.2)

## 2016 ANES
anes2016 %>% transmute(
  v1 = discursive,
  v2 = polknow,
  v3 = polknow_evalpre) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge",
                           "Interviewer\nEvaluation")
  ) + plot_default
ggsave(here("out/fig1c-corplot_anes2016.png"),width=3.2, height=3.2)

## 2012 ANES
anes2012 %>% transmute(
  v1 = discursive,
  v2 = polknow,
  v3 = polknow_evalpre) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha =.05, size=.2)),
          axisLabels="none",
          columnLabels = c("Discursive\nSophistication",
                           "Factual\nKnowledge",
                           "Interviewer\nEvaluation")
  ) + plot_default
ggsave(here("out/fig1d-corplot_anes2012.png"),width=3.2, height=3.2)


# Sample responses --------------------------------------------------------

ces2018 %>%
  filter(wc > (median(wc) - 100) & wc < (median(wc) + 100),
         polknow_factual == 0.6) %>%
  filter((discursive < quantile(discursive,.25) & female == 0) |
           (discursive > quantile(discursive,.75) & female == 1)) %>%
  arrange(discursive) %>%
  select(caseid, female, polknow_old, polknow_factual, discursive) %>%
  #left_join(opend) %>%
  left_join(haven::read_sav("/data/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
              select(caseid, UWM309, UWM310, UWM312, UWM313, UWM315,
                            UWM316, UWM318, UWM319, UWM321, UWM322)) %>%
  write_csv(file="calc/tmp/select_cces.csv")

# Directly identify sample responses in revised submission
ces2018 %>% filter(caseid %in% c(418389435, 412523485)) %>%
  select(caseid, female, polknow_old, polknow_factual, discursive) %>%
  left_join(haven::read_sav("/data/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
              select(caseid, UWM309, UWM310, UWM312, UWM313, UWM315,
                            UWM316, UWM318, UWM319, UWM321, UWM322))# %>% View()

# Old code to identify sample responses in original submission (using old factual scale):
ces2018 %>% filter(caseid %in% c(412380137, 414596729)) %>%
  select(caseid, female, polknow_old, polknow_factual, discursive) %>%
  left_join(haven::read_sav("/data/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
              select(caseid, UWM309, UWM310, UWM312, UWM313, UWM315,
                            UWM316, UWM318, UWM319, UWM321, UWM322))# %>% View()



# Validation: competence and engagement -----------------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("discursive", "polknow",
         "female", "age", "black", "educ", "faminc", "relig"
)

m1 <- c(
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~glm(reformulate(ivs, response = "vote"), data = ., family=binomial("logit"))),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "polint_att"), data = .)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_int"), data = .)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_ext"), data = .))
)

m1 %>%
  map_dfr(~tidy(comparisons(
    ., variables = list(discursive = c(-1,1),
                        polknow = c(-1,1)))),
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
                         `discursive` = "Discursive\nSophistication",
                         `polknow` = "Factual\nKnowledge")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high,
             col = term, shape = term)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(study~dv) +
  xlab("Estimated Effect of Discursive Sophistication and Factual Knowledge\n(for an increase from 1 SD below mean to 1 SD above mean)") +
  scale_color_brewer(palette = "Dark2") +
  ylab("Independent Variable") + plot_default +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev)
ggsave("out/fig-knoweff.pdf", width=6.5, height=4)


# Validation: interaction b/w measures and no controls --------------------

ivs <- c("discursive * polknow",
         "female", "age", "black", "educ", "faminc", "relig")
m1int <- c(
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~glm(reformulate(ivs, response = "vote"), data = ., family=binomial("logit"))),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "polint_att"), data = .)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_int"), data = .)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_ext"), data = .))
)

ivs <- c("discursive", "polknow")
m1noc <- c(
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~glm(reformulate(ivs, response = "vote"), data = ., family=binomial("logit"))),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "polint_att"), data = .)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_int"), data = .)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_ext"), data = .))
)

c(m1noc[1], m1[1], m1int[1],
  m1noc[5], m1[5], m1int[5]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout and political interest
            in the 2018 CES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Turnout","Political Interest"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2018cces1.tex", label = "tab:knoweff2018cces1")

c(m1noc[9], m1[9], m1int[9],
  m1noc[13], m1[13], m1int[13]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout, political interest, internal efficacy,
          and external efficacy in the 2018 CES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Internal Efficacy","External Efficacy"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2018cces2.tex", label = "tab:knoweff2018cces2")

c(m1noc[2], m1[2], m1int[2],
  m1noc[6], m1[6], m1int[6]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout and political interest
            in the 2020 ANES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Turnout","Political Interest"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2020anes1.tex", label = "tab:knoweff2020anes1")

c(m1noc[10], m1[10], m1int[10],
  m1noc[14], m1[14], m1int[14]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on internal and external efficacy
            in the 2020 ANES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Turnout","Political Interest","Internal Efficacy","External Efficacy"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2020anes2.tex", label = "tab:knoweff2020anes2")

c(m1noc[3], m1[3], m1int[3],
  m1noc[7], m1[7], m1int[7]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout and political interest
            in the 2016 ANES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Turnout","Political Interest"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2016anes1.tex", label = "tab:knoweff2016anes1")

c(m1noc[11], m1[11], m1int[11],
  m1noc[15], m1[15], m1int[15]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on internal and external efficacy
            in the 2016 ANES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Internal Efficacy","External Efficacy"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2016anes2.tex", label = "tab:knoweff2016anes2")

c(m1noc[4], m1[4], m1int[4],
  m1noc[8], m1[8], m1int[8]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on turnout and political interest
          in the 2012 ANES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Turnout","Political Interest"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2012anes1.tex", label = "tab:knoweff2012anes1")

c(m1noc[12], m1[12], m1int[12],
  m1noc[16], m1[16], m1int[16]) %>%
  stargazer(type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
            model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
            title=c("Effects of sophistication on internal and external efficacy
            in the 2012 ANES. Standard errors in parentheses. Estimates of model
          (2) and (5) are used for Figure 2 in the main text."),
            column.labels = c("Internal Efficacy","External Efficacy"),
            column.separate = c(3,3),
            order = c(1,2,9,3:8,10),
            covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                                 "Female", "Age", "Black", "College Degree",
                                 "Household Income","Church Attendance","Constant"),
            keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
            out = "out/tab-knoweff2012anes2.tex", label = "tab:knoweff2012anes2")


# Validation: interaction by gender ---------------------------------------

ivs <- c("discursive * polknow",
         "educ", "age", "black", "faminc", "relig")
c(map(list(ces2018, anes2020, anes2016, anes2012),
      ~glm(reformulate(ivs, response = "vote"), family=binomial("logit"),
           data = ., subset = female == 1)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "polint_att"),
          data = ., subset = female == 1)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_int"),
          data = ., subset = female == 1)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_ext"),
          data = ., subset = female == 1))) %>%
  map(summary)

c(map(list(ces2018, anes2020, anes2016, anes2012),
      ~glm(reformulate(ivs, response = "vote"), family=binomial("logit"),
           data = ., subset = female == 0)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "polint_att"),
          data = ., subset = female == 0)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_int"),
          data = ., subset = female == 0)),
  map(list(ces2018, anes2020, anes2016, anes2012),
      ~lm(reformulate(ivs, response = "effic_ext"),
          data = ., subset = female == 0))) %>%
  map(summary)


# Validation: information retrieval ---------------------------------------

m2 <- list(
  lm(know_dis ~ discursive + polknow, data = yg2015),
  lm(know_dis ~ discursive + polknow + female + age + black + educ + faminc + relig, data = yg2015),
  lm(know_dis ~ discursive * polknow + female + age + black + educ + faminc + relig, data = yg2015)
)

bind_rows(
  plot_cap(m2[[2]], condition = c("discursive"), draw = F) %>%
    transmute(iv = "discursive", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
  plot_cap(m2[[2]], condition = c("polknow"), draw = F) %>%
    transmute(iv = "polknow", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
) %>%
  as_tibble() %>%
  mutate(Variable = recode_factor(iv,
                                  `discursive` = "Discursive Sophistication",
                                  `polknow` = "Factual Knowledge")) %>%
  ggplot(aes(x=ivval, y=mean, ymin=cilo,ymax=cihi, lty=Variable, fill=Variable)) + plot_default +
  geom_ribbon(alpha=0.6, lwd=.1) + geom_line() +
  ylab("Information Retrieval") + xlab("Value of Independent Variable") +
  scale_fill_brewer(palette="Dark2") # old version = "Paired"
ggsave("out/fig-yg_disease.pdf", width=4, height=2)

stargazer(m2, type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
          title="Linear regressions predicting information retrieval in the 2015 YouGov study.
          Standard errors in parentheses. Estimates of model (2) are used for Figure 3
          in the main text.",
          column.labels = "Information Retrieval",
          column.separate = 3,
          order = c(1,2,9,3:8,10),
          covariate.labels = c("Discursive Soph.","Factual Knowledge", "Disc. X Factual",
                               "Female", "Age", "Black", "College Degree",
                               "Household Income","Church Attendance","Constant"),
          keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
          out = "out/tab-yg_disease.tex", label = "tab:yg_disease")



# Validation: manual coding -----------------------------------------------

opend_cor <- tibble(
  cor = c(cor(swiss2012_de$discursive, swiss2012_de$loj),
          cor(swiss2012_fr$discursive, swiss2012_fr$loj),
          cor(swiss2012_it$discursive, swiss2012_it$loj)),
  language = c("German", "French", "Italian"),
  discursive = -1,
  loj = 4) %>%
  mutate(cor = paste0("r = ",round(cor, 2)))

rbind(data.frame(swiss2012_de, language = "German"),
      data.frame(swiss2012_fr, language = "French"),
      data.frame(swiss2012_it, language = "Italian")) %>%
  ggplot(aes(x=discursive, y=as.factor(loj))) +
  geom_density_ridges(aes(fill="1"), scale = 4, alpha=.6) + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication") +
  scale_fill_brewer(palette="Dark2") + theme(legend.position = "none")
ggsave("out/fig-swiss_ggridges.pdf",width=6,height=2)

rbind(data.frame(swiss2012_de, language = "German"),
      data.frame(swiss2012_fr, language = "French"),
      data.frame(swiss2012_it, language = "Italian")) %>%
  ggplot(aes(x=discursive, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_empty +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("out/fig-swiss_ggridges0.pdf",width=6,height=2)

rbind(data.frame(swiss2012_de, language = "German"),
      data.frame(swiss2012_fr, language = "French"),
      data.frame(swiss2012_it, language = "Italian")) %>%
  mutate(discursive = ifelse(language == "German", NA, discursive),
         discursive = ifelse(language == "Italian", NA, discursive)) %>%
  ggplot(aes(x=discursive, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data = filter(opend_cor, language == "French"),
            aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("out/fig-swiss_ggridges1.pdf",width=6,height=2)

rbind(data.frame(swiss2012_de, language = "German"),
      data.frame(swiss2012_fr, language = "French"),
      data.frame(swiss2012_it, language = "Italian")) %>%
  mutate(discursive = ifelse(language == "Italian", NA, discursive)) %>%
  ggplot(aes(x=discursive, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data = filter(opend_cor, language != "Italian"),
            aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("out/fig-swiss_ggridges2.pdf",width=6,height=2)

rbind(data.frame(swiss2012_de, language = "German"),
      data.frame(swiss2012_fr, language = "French"),
      data.frame(swiss2012_it, language = "Italian")) %>%
  ggplot(aes(x=discursive, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.1,.9)) +
  facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("out/fig-swiss_ggridges3.pdf",width=6,height=2)



# Gender gap: levels w/o controls -----------------------------------------

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-1.96*sd(x)/sqrt(length(x))
  ymax <- m+1.96*sd(x)/sqrt(length(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

grid.arrange(
  ces2018 %>%
    select(discursive,
           polknow,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `polknow` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2018 CES"),
  yg2015 %>%
    select(discursive,
           polknow,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `polknow` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
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
      scale_fill_brewer(palette="Dark2")),

  anes2020 %>%
    select(discursive,
           polknow,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `polknow` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2020 ANES"),
  anes2016 %>%
    select(discursive,
           polknow,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `polknow` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2016 ANES"),
  anes2012 %>%
    select(discursive,
           polknow,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `polknow` = "Factual\nKnowledge")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col=Variable)) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) + ylab(NULL) + xlab(NULL) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 2,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    ggtitle("2012 ANES"),

  swiss2012_fr %>%
    select(discursive,
           loj_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `loj_scale` = "Level of\nJustification")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col="Open-ended")) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 1.5,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    labs(title="Swiss Survey",
         subtitle="French Respondents",
         y=NULL, x=NULL),
  swiss2012_de %>%
    select(discursive,
           loj_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `loj_scale` = "Level of\nJustification")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col="Open-ended")) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 1.5,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    labs(title=" ",
         subtitle="German Respondents",
         y=NULL, x=NULL),
  swiss2012_it %>%
    select(discursive,
           loj_scale,
           female) %>%
    pivot_longer(-female) %>%
    mutate(Gender = factor(female, labels = c("Male","Female")),
           Variable = recode_factor(name,
                                    `discursive` = "Discursive\nSophistication",
                                    `loj_scale` = "Level of\nJustification")) %>%
    na.omit() %>%
    ggplot(aes(y=value, x=Gender)) + plot_default +
    geom_quasirandom(aes(col="Open-ended")) +
    stat_summary(fun.data=data_summary, geom="errorbar", width=.5) +
    facet_wrap(~Variable) +
    guides(col="none") + scale_color_brewer(palette="Dark2") +
    stat_compare_means(aes(label = ..p.signif..),
                       method = "t.test", label.y = 1.5,
                       comparisons = list(c("Male", "Female")),
                       symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                                          symbols = c("***", "**", "*", "ns"))) +
    labs(title="",
         subtitle="Italian Respondents",
         y=NULL, x=NULL),
  ncol=3) %>%
  ggsave("out/fig-meandiff.png", plot = ., width=6.5, height=8)


# Gender gap: differences w/ controls -------------------------------------

ivs <- c("female", "age", "black", "educ", "faminc", "relig")
m3text <- list(ces2018, anes2020, anes2016, anes2012, yg2015) %>%
  map(~lm(reformulate(ivs, response = "discursive"),
          data = ., subset = !is.na(polknow)))
m3factual <- list(ces2018, anes2020, anes2016, anes2012, yg2015) %>%
  map(~lm(reformulate(ivs, response = "polknow"),
          data = ., subset = !is.na(discursive)))

ivs <- c("female", "age", "educ")
m3swiss <- list(swiss2012_fr, swiss2012_de, swiss2012_it) %>%
  map(~lm(reformulate(ivs, response = "discursive"), data = .))

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
  ggplot(aes(y=study, x=estimate, xmin=conf.low, xmax=conf.high,
             col = dv, shape = dv)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_vline(xintercept = c(-0.2,0.2), color="grey", lty="dashed") +
  geom_point() + geom_errorbarh(height=0) +
  geom_errorbarh(aes(xmin=conf.low90, xmax=conf.high90), height=.2) +
  facet_wrap(.~dv) + xlim(-.75, .25) +
  xlab("Estimated Gender Gap") + ylab("Dataset") + plot_default +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
ggsave("out/fig-determinants.pdf",width=5,height=3)

stargazer(m3text, type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Discursive Sophistication",
          title="Linear regressions predicting discursive sophistication in the CES, ANES, and YouGov study.
          Estimates are used for Figure 6 in the main text.",
          column.labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
          covariate.labels = c("Female", "Age", "Black", "College Degree",
                               "Household Income", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "out/tab-determinants_text.tex", label = "tab:determinants_text")

stargazer(m3swiss, type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Discursive Sophistication",
          title="Linear regressions predicting discursive sophistication in the Swiss referendum study.
          Estimates are used for Figure 6 in the main text.",
          column.labels = c("French", "German", "Italian"),
          covariate.labels = c("Female", "Age", "College Degree", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "out/tab-determinants_swiss.tex", label = "tab:determinants_swiss")

stargazer(m3factual, type="text", align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Factual Knowledge",
          title="Linear regressions predicting  factual knowledge in the CES, ANES, and YouGov study.
          Estimates are used for Figure 6 in the main text.",
          column.labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov"),
          covariate.labels = c("Female", "Age", "Black", "College Degree",
                               "Household Income", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "out/tab-determinants_factual.tex", label = "tab:determinants_factual")


# Explaining the gender gap -----------------------------------------------

## estimate topic prevalence effects
prep2012 <- estimateEffect(~ female + age + educ_cont + pid_cont + educ_pid,
                           stm_fit2012, meta = anes2012disc$prepDocuments$meta, uncertainty = "Global")
prep2016 <- estimateEffect(~ female + age + educ_cont + pid_cont + educ_pid,
                           stm_fit2016, meta = anes2016disc$prepDocuments$meta, uncertainty = "Global")
prep2020 <- estimateEffect(~ female + age + educ_cont + pid_cont + educ_pid,
                           stm_fit2020, meta = anes2020disc$prepDocuments$meta, uncertainty = "Global")

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
png("out/fig-stm_gender.png", height=5.5, width=6.5, units = "in", res = 400)
par(mfrow=c(3,1), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2012, covariate = "female", topics = topics2012, model = anes2012disc$stm,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2012 ANES")
plot.estimateEffect(prep2016, covariate = "female", topics = topics2016, model = anes2016disc$stm,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2016 ANES")
plot.estimateEffect(prep2020, covariate = "female", topics = topics2020, model = anes2020disc$stm,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2020 ANES")
dev.off()

png("out/fig-stm_gender1.png", height=3.5, width=7.5, units = "in", res = 400)
par(mfrow=c(1,1), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2020, covariate = "female", topics = topics2020, model = anes2020disc$stm,
                    xlim = c(-.05,.02), method = "difference", cov.value1 = 1, cov.value2 = 0,
                    labeltype = "frex", n=5, verbose.labels = F, width=50,
                    main = "2020 ANES")
dev.off()

png("out/fig-stm_gender0.png", height=3.5, width=7.5, units = "in", res = 400)
par(mfrow=c(1,1), mar=c(2.2,0.5,2.2,0.5))
plot(x=c(-.05,.02), y = 1:2,
     type = "n", xlim = c(-.05,.02), main = "2020 ANES",
     yaxt='n', xlab = "", ylab = "")
abline(v = 0, lty = "dashed")
dev.off()
