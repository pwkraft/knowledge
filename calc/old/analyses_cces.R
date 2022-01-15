# =================================
# Asking the Right Questions
# - Main analyses for first draft
# - Author: Patrick Kraft
# - Date: 02/14/19
# =================================

## packages
library(tidyverse)
library(haven)
library(stargazer)
library(survey)
library(broom)
library(marginaleffects)
library(margins)

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))

## recode CCES data
source(here::here("code/arq_recode.R"))

## set survey design
cces_svy <- svydesign(id = ~1, weights = cces$weight, data = cces)


# Figure 2: Gender difference in average proportions of correct re --------

## Variable names
varnames_know <- c("pk_house", "pk_senate", "pk_ideol",
                   "know_guns", "know_trade", "know_daca", 
                   "know_health", "know_abortion")
varnames_controls <- c("age", "college", "black", "married")

## Estimate models
m1bivariate <- map(varnames_know, ~reformulate("female", response = .)) %>% 
  map(~svyglm(., family = binomial(logit), design = cces_svy))
m1controls <- map(varnames_know, ~reformulate(c("female", varnames_controls), response = .)) %>% 
  map(~svyglm(., family = binomial(logit), design = cces_svy))

## Plot average marginal effects
c(m1bivariate, m1controls) %>% 
  map_dfr(~summary(marginaleffects(.)), .id = "model") %>% 
  as_tibble() %>% 
  filter(term == "femaleFemale") %>% 
  mutate(dv = rep(varnames_know, 2),
         Controls = rep(c("No","Yes"), each = 8),
         Question = factor(
           dv, 
           levels = rev(varnames_know),
           labels = rev(c("Majority in the House",
                          "Majority in the Senate", 
                          "Ideological Placement", 
                          "Gun Legislation",
                          "Trade Policy", "DACA",
                          "Health Care", "Abortion"))),
         Group = recode_factor(
           dv, 
           pk_house = "Traditional Items", 
           pk_senate = "Traditional Items", 
           pk_ideol = "Traditional Items",
           know_guns = "Male-dominated Policy Items", 
           know_daca = "Neutral", 
           know_abortion = "Female-dominated Policy Items", 
           know_health = "Female-dominated Policy Items", 
           know_trade = "Male-dominated Policy Items")) %>% 
  ggplot(aes(x = estimate, y = Question,
             xmin = conf.low, xmax = conf.high, 
             shape = Controls, col = Controls)) +
  plot_default + 
  geom_vline(xintercept = 0, col = "gray") +
  geom_pointrange(position = position_dodge(width = -.5)) + 
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Group, ncol = 1, scales = "free_y") +
  labs(
    y = "", 
    x = "Gender Differences in Probability of Correct Responses"
  )
ggsave("fig/knowdiff_oview_new.png", width = 7, height = 5)

## Table for appendix  
m1controls %>% 
  stargazer(., type = 'text', no.space = T,
            style = "default", font.size = "tiny",
            star.cutoffs = c(.05,.01,.001), header = F,
            title = "Determinants of correct responses on knowledge items (survey-weighted logit).",
            notes = "Estimates used for Figure 2 in the main text.",
            column.labels = c("Conventional","Male-Dominated","Neutral","Female-Dominated"),
            column.separate = c(3,2,1,2),
            dep.var.caption = "",
            dep.var.labels = c("House","Senate",
                               "Placement","Guns",
                               "Trade","DACA",
                               "Health","Abortion"),
            keep.stat = c("n","aic"),
            covariate.labels = c("Female","Age","College",
                                 "Black","Married","Constant"),
            out = "tab/determinants.tex")


# Figure 3: Differences in knowledge levels -------------------------------

summary(svyglm(pk_combined ~ female + age + college + black + married, design = cces_svy))
summary(svyglm(know ~ female + age + college + black + married, design = cces_svy))

svyttest(pk_combined~female, design = cces_svy)$p.value

yvals <- c(.92, .35)

df_pval <- tibble(
  label = c("p < 0.001", paste("p =",round(svyttest(know~female, design = cces_svy)$p.value, 3))),
  x = "Male",
  y = yvals + .05,
  Knowledge = factor(c("Conventional", "Balanced"), 
                     levels = c("Conventional", "Balanced"))
)

df_segm <- tibble(
  x = "Male",
  y = yvals,
  xend = "Female",
  yend = yvals,
  Knowledge = factor(c("Conventional", "Balanced"), 
                     levels = c("Conventional", "Balanced"))
)

df_vert <- tibble(
  x = rep(c("Male", "Female"), 2),
  y = rep(yvals, each = 2),
  xend = rep(c("Male", "Female"), 2),
  yend = rep(yvals, each = 2) - .03,
  Knowledge = factor(rep(c("Conventional", "Balanced"), each = 2), 
                     levels = c("Conventional", "Balanced"))
)

cces %>%
  select(female, know, pk_combined) %>%
  gather(item, value, -female) %>%
  group_by(female, item) %>%
  summarize(mean = mean(value, na.rm = T), 
            cilo = cilo(value), 
            cihi = cihi(value)) %>% 
  mutate(Knowledge = recode_factor(item,
                                   `pk_combined` = "Conventional",
                                   `know` = "Balanced")) %>% 
  ggplot(aes(x = female, fill = female, y = mean, ymin = cilo, ymax = cihi)) +
  plot_default + 
  geom_col() + 
  geom_linerange() +
  facet_wrap(~Knowledge) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Average Knowledge",
       x = "") +
  ylim(0,1) +
  geom_segment(data = df_segm, 
               aes(x = x, y = y, yend = yend, xend = xend),
               inherit.aes=FALSE) +
  geom_segment(data = df_vert, 
               aes(x = x, y = y, yend = yend, xend = xend),
               inherit.aes=FALSE) +
  geom_text(data = df_pval,
            aes(x = x, y = y, label = label),
            inherit.aes=FALSE, 
            position = position_nudge(x = .5),
            cex = 2)
ggsave("fig/knowdiff_overall.png", width = 3, height = 2)


# Figure 5: Political knowledge as a predictor of turnout, engagem --------

## Estimate models
m2 <- list(
  glm(vote_post ~ pk_combined + age + college + black + married,
         family = binomial(logit), data = cces, subset = female == "Female"),
  glm(vote_post ~ pk_combined + age + college + black + married,
         family = binomial(logit), data = cces, subset = female == "Male"),
  glm(vote_post ~ know + age + college + black + married,
         family = binomial(logit), data = cces, subset = female == "Female"),
  glm(vote_post ~ know + age + college + black + married,
         family = binomial(logit), data = cces, subset = female == "Male"),
  
  lm(polatt ~ pk_combined + age + college + black + married,
         data = cces, subset = female == "Female"),
  lm(polatt ~ pk_combined + age + college + black + married,
         data = cces, subset = female == "Male"),
  lm(polatt ~ know + age + college + black + married,
         data = cces, subset = female == "Female"),
  lm(polatt ~ know + age + college + black + married,
         data = cces, subset = female == "Male"),
  
  lm(campint ~ pk_combined + age + college + black + married,
         data = cces, subset = female == "Female"),
  lm(campint ~ pk_combined + age + college + black + married,
         data = cces, subset = female == "Male"),
  lm(campint ~ know + age + college + black + married,
         data = cces, subset = female == "Female"),
  lm(campint ~ know + age + college + black + married,
         data = cces, subset = female == "Male"),
  
  lm(effic_int ~ pk_combined + age + college + black + married,
         data = cces, subset = female == "Female"),
  lm(effic_int ~ pk_combined + age + college + black + married,
         data = cces, subset = female == "Male"),
  lm(effic_int ~ know + age + college + black + married,
         data = cces, subset = female == "Female"),
  lm(effic_int ~ know + age + college + black + married,
         data = cces, subset = female == "Male")
)

m2pk <- list(
  svyglm(vote_post ~ pk_combined + age + college + black + married,
         family = binomial(logit), design = cces_svy, subset = female == "Female"),
  svyglm(vote_post ~ pk_combined + age + college + black + married,
         family = binomial(logit), design = cces_svy, subset = female == "Male"),
  
  svyglm(polatt ~ pk_combined + age + college + black + married,
         design = cces_svy, subset = female == "Female"),
  svyglm(polatt ~ pk_combined + age + college + black + married,
         design = cces_svy, subset = female == "Male"),
  
  svyglm(campint ~ pk_combined + age + college + black + married,
         design = cces_svy, subset = female == "Female"),
  svyglm(campint ~ pk_combined + age + college + black + married,
         design = cces_svy, subset = female == "Male"),
  
  svyglm(effic_int ~ pk_combined + age + college + black + married,
         design = cces_svy, subset = female == "Female"),
  svyglm(effic_int ~ pk_combined + age + college + black + married,
         design = cces_svy, subset = female == "Male")
)

m2know <- list(
  svyglm(vote_post ~ know + age + college + black + married,
         family = binomial(logit), design = cces_svy, subset = female == "Female"),
  svyglm(vote_post ~ know + age + college + black + married,
         family = binomial(logit), design = cces_svy, subset = female == "Male"),
  
  svyglm(polatt ~ know + age + college + black + married,
         design = cces_svy, subset = female == "Female"),
  svyglm(polatt ~ know + age + college + black + married,
         design = cces_svy, subset = female == "Male"),
  
  svyglm(campint ~ know + age + college + black + married,
         design = cces_svy, subset = female == "Female"),
  svyglm(campint ~ know + age + college + black + married,
         design = cces_svy, subset = female == "Male"),
  
  svyglm(effic_int ~ know + age + college + black + married,
         design = cces_svy, subset = female == "Female"),
  svyglm(effic_int ~ know + age + college + black + married,
         design = cces_svy, subset = female == "Male")
)

## Plot average marginal effects
c(m2pk, m2know) %>% 
  map_dfr(~summary(marginaleffects(.)), .id = "model") %>% 
  as_tibble() %>% 
  filter(term %in% c("pk_combined", "know")) %>% 
  mutate(Gender = rep(c("Female", "Male"), 8),
         Gender = factor(Gender, levels = c("Male","Female")),
         Outcome = rep(rep(c("Turnout","Attention\nto Politics",
                             "Campaign\nInterest","Internal\nEfficacy"), each = 2), 2),
         Outcome = factor(Outcome, levels = rev(c("Turnout","Attention\nto Politics",
                                                  "Campaign\nInterest","Internal\nEfficacy"))),
         Knowledge = rep(c("Conventional", "Balanced"), each = 8),
         Knowledge = factor(Knowledge, levels = c("Conventional","Balanced"))) %>% 
  ggplot(aes(x = estimate, y = Outcome,
             xmin = conf.low, xmax = conf.high, 
             shape = Gender, col = Gender)) +
  plot_default + 
  geom_vline(xintercept = 0, col = "gray") +
  geom_pointrange(position = position_dodge(width = -.5)) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~Knowledge, ncol = 2) +
  labs(
    y = "", 
    x = "Average Marginal Effect of Political Knowledge"
  )
ggsave("fig/know_predictor.png", width = 7, height = 3)

## Plot average predictions
bind_rows(
  m2pk %>% 
    map_dfr(~cplot(., "pk_combined", draw = FALSE), .id = "Model") %>% 
    mutate(Gender = rep(rep(c("Female", "Male"), each = 25), 4),
           Outcome = rep(c("Turnout","Attention\nto Politics",
                           "Campaign\nInterest","Internal\nEfficacy"), 
                         each = 50),
           Knowledge = "Conventional"),
  m2know %>% 
    map_dfr(~cplot(., "know", draw = FALSE), .id = "Model") %>% 
    mutate(Gender = rep(rep(c("Female", "Male"), each = 25), 4),
           Outcome = rep(c("Turnout","Attention\nto Politics",
                           "Campaign\nInterest","Internal\nEfficacy"), 
                         each = 50),
           Knowledge = "Balanced")
) %>% 
  mutate(Gender = factor(Gender, levels = c("Male","Female")),
         Outcome = factor(Outcome, levels = c("Turnout","Attention\nto Politics",
                                              "Campaign\nInterest","Internal\nEfficacy")),
         Knowledge = factor(Knowledge, levels = c("Conventional","Balanced"))) %>% 
  ggplot(aes(x = xvals, y = yvals, ymax = upper, ymin = lower, lty = Gender, fill = Gender)) +
  plot_default +
  geom_ribbon(alpha = .5) +
  geom_line() +
  scale_fill_brewer(palette = "Paired") +
  facet_grid(Knowledge ~ Outcome) +
  labs(
    y = "Expected Value", 
    x = "Political Knowledge"
  )
ggsave("fig/know_averages.png", width = 7, height = 3)

## Table for appendix  
m2pk %>%
  stargazer(., type = 'text', no.space = T,
            style = "default", font.size = "tiny",
            star.cutoffs = c(.05,.01,.001), header = F,
            title = "Conventional political knowledge as a predictor of turnout, engagement, and efficacy",
            notes = "Estimates used for Figure 4 and Figure 5 in the main text.",
            column.labels = rep(c("Female","Male"), 4),
            dep.var.caption = "",
            dep.var.labels = c("Turnout","Att. to Politics",
                               "Campaign Interest","Internal Efficacy"),
            keep.stat = c("n","rsq","adj.rsq","aic"),
            covariate.labels = c("Conventional Knowledge",
                                 "Age","College","Black","Married","Constant"),
            out = "tab/predictor_conventional.tex")

m2know %>%
  stargazer(., type = 'text', no.space = T,
            style = "default", font.size = "tiny",
            star.cutoffs = c(.05,.01,.001), header = F,
            title = "Gender-balanced political knowledge as a predictor of turnout, engagement, and efficacy",
            notes = "Estimates used for Figure 4 and Figure 5 in the main text.",
            column.labels = rep(c("Female","Male"), 4),
            dep.var.caption = "",
            dep.var.labels = c("Turnout","Att. to Politics",
                               "Campaign Interest","Internal Efficacy"),
            keep.stat = c("n","rsq","adj.rsq","aic"),
            covariate.labels = c("Balanced Knowledge",
                                 "Age","College","Black","Married","Constant"),
            out = "tab/predictor_balanced.tex")


