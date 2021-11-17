# ========================================================================= #
# Measuring Political Sophistication using Open-ended Responses
# Patrick Kraft
# ========================================================================= #
# - prepares the survey data of the 2020 ANES for subsequent analyses
# - prepares open-ended responses (selecting variables, spell checksing etc.)
# - fits structural topic model for open-ended responses
# - computes discursive sophistication measure



# Packages, data, and custom functions ------------------------------------

library(car)
library(tidyverse)
library(haven)
library(stm)
library(quanteda)
library(quanteda.dictionaries)
library(here)

raw2020 <- read_dta("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_stata_20210324.dta")
raw2020_oe <- read_csv("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_redacted_openends.csv")
load("~/Dropbox/Uni/Data/LIWC/liwc2015.Rdata")

source("calc/func.R")



# Recode main survey data -------------------------------------------------

na_in <- function(x, y) {
  x[x %in% y] <- NA
  x
}

anes2020 <- raw2020 %>% transmute(

  ## respondent id
  caseid = V200001,

  ## interview mode (1=Video, 2=Phone, 3=Web)
  mode = as_factor(V200002),

  ## political knowledge (office recognition, post-election)
  polknow_office = ((na_in(V202138y, -7:-1) == 1) +
                      (na_in(V202139y1, -7:-1) == 1) +
                      (na_in(V202140y1, -7:-1) == 1) +
                      (na_in(V202141y1, -7:-1) == 1) +
                      (na_in(V202142y1, -7:-1) == 1))/5,

  ## political knowledge (factual knowledge questions, pre-election
  polknow_factual = ((na_in(V201644, -5) == 6) +
                       (na_in(V201645, -5) == 1) +
                       (na_in(V201646, -5) == 1) +
                       (na_in(V201647, -5) == 2))/4,

  ## political knowledge (majorities in congress, post-election)
  polknow_majority = ((Recode(knowl_housemaj, "c(-6,-7)=NA")==2)
                                              + (Recode(knowl_senmaj, "c(-6,-7)=NA")==1))/2)

  ## political knowledge (interviewer evaluation, only in F2F part!)
  polknow_evalpre = (5 - Recode(iwrobspre_levinfo, "lo:-1=NA"))/4
  polknow_evalpost = (5 - Recode(iwrobspost_levinfo, "lo:-1=NA"))/4
  polknow_eval = (polknow_evalpre + polknow_evalpost)/2

  ## intelligence (interviewer evaluation, only in F2F part!)
  intpre = (5 - Recode(iwrobspre_intell, "lo:-1=NA"))/4
  intpost = (5 - Recode(iwrobspost_intell, "lo:-1=NA"))/4
  int = (intpre + intpost)/2

  ## education (bachelor degree)
  educ = as.numeric(dem_edugroup_x >= 4)
  educ[dem_edugroup_x < 0] = NA

  ## education (continuous)
  educ_cont = (Recode(dem_edugroup_x, "lo:0=NA") - 1)/4

  ## political media exposure
  polmedia = Recode(prmedia_wkinews, "lo:-4=NA; -1=0")
                            + Recode(prmedia_wktvnws, "lo:-4=NA; -1=0")
                            + Recode(prmedia_wkpaprnws, "lo:-4=NA; -1=0")
                            + Recode(prmedia_wkrdnws, "lo:-4=NA; -1=0")) / 28

  ## political discussion
  poldisc = Recode(discuss_discpstwk, "lo:-1 = NA")/7
  poldisc[discuss_disc==2] = 0

  ## political interest (pay attention to politics)
  polint_att = (5 - Recode(interest_attention, "lo:-1 = NA"))/4

  ## political interest (following campaign)
  polint_cam = (3 - Recode(interest_following, "lo:-1 = NA"))/2

  ## overall political interest
  polint = with(anes2012, (polint_att + polint_cam)/2)

  ## internal efficacy
  effic_int = Recode(effic_complicstd, "lo:-8=NA; -1=1") - 1
                             + Recode(effic_complicrev, "lo:-8=NA; -1=1") - 1
                             - Recode(effic_undstd, "lo:-8=NA; -1=5") + 5
                             - Recode(effic_undrev, "lo:-8=NA; -1=5") + 5
  ) / 8

  ## external efficacy
  effic_ext = Recode(effic_carestd, "lo:-8=NA; -1=1") - 1
                             - Recode(effic_carerev, "lo:-8=NA; -1=5") + 5
                             + Recode(effic_saystd, "lo:-8=NA; -1=1") - 1
                             - Recode(effic_sayrev, "lo:-8=NA; -1=5") + 5
  ) / 8

  ## overall efficacy
  effic = with(anes2012, (effic_int + effic_ext)/2)

  ## voted in previous election
  pastvote = Recode(interest_voted2008, "c(2,5)=0; lo:-1=NA")

  ## voted in current election
  vote = Recode(rvote2012_x, "2=0; lo:-1=NA")

  ## participated in protest march / rally
  protest = Recode(dhsinvolv_march, "c(2,5)=0; lo:-1=NA")

  ## letter to congressman/senator
  letter = Recode(dhsinvolv_contact1, "2=0; lo:-1=NA")

  ## signed a petition
  petition = as.numeric((Recode(dhsinvolv_netpetition, "c(2,5)=0; lo:-1=NA") +
                                     Recode(dhsinvolv_petition, "c(2,5)=0; lo:-1=NA")) > 0)

  ## wear a campaign button
  button = Recode(mobilpo_sign, "c(2,5)=0; lo:-1=NA")

  ## additive index non-conventional participation
  part = with(anes2012, protest + petition + button + letter)

  ## vote choice (pre-election) (1=dpc, 2=rpc)
  vc_pre = Recode(prevote_intpreswho,"lo:-1=NA; 1=1; 2=2; else=3")

  ## vote choice (post-election)
  vc_post = Recode(postvote_presvtwho,"lo:-1=NA; 1=1; 2=2; else=3")
  vote_rep = Recode(vc_post, "3=NA") - 1

  ## vote change (pre-post)
  vc_change = vc_pre == vc_post
  #vc_change[prevote_presvt == 1] = 1
  vc_change[is.na(vc_pre) & !is.na(vc_post)] = 0
  vc_change[!is.na(vc_pre) & is.na(vc_post)] = 0

  ## party/candidate placements
  ideol_ego = (Recode(libcpre_self, "lo:0=NA") - 4)/3
  spsrvpr_ego = (Recode(spsrvpr_ssself, "lo:0=NA") - 4)/3
  defsppr_ego = (Recode(defsppr_self, "lo:0=NA") - 4)/3
  inspre_ego = (Recode(inspre_self, "lo:0=NA") - 4)/3
  guarpr_ego = (Recode(guarpr_self, "lo:0=NA") - 4)/3
  aidblack_ego = (Recode(aidblack_self, "lo:0=NA") - 4)/3
  envjob_ego = (Recode(envjob_self, "lo:0=NA") - 4)/3

  ideol_dpc = (Recode(libcpre_dpc, "lo:0=NA") - 4)/3
  ideol_rpc = (Recode(libcpre_rpc, "lo:0=NA") - 4)/3
  spsrvpr_dpc = (Recode(spsrvpr_ssdpc, "lo:0=NA") - 4)/3
  spsrvpr_rpc = (Recode(spsrvpr_ssrpc, "lo:0=NA") - 4)/3
  defsppr_dpc = (Recode(defsppr_dpc, "lo:0=NA") - 4)/3
  defsppr_rpc = (Recode(defsppr_rpc, "lo:0=NA") - 4)/3
  inspre_dpc = (Recode(inspre_dpc, "lo:0=NA") - 4)/3
  inspre_rpc = (Recode(inspre_rpc, "lo:0=NA") - 4)/3
  guarpr_dpc = (Recode(guarpr_dpc, "lo:0=NA") - 4)/3
  guarpr_rpc = (Recode(guarpr_rpc, "lo:0=NA") - 4)/3
  aidblack_dpc = (Recode(aidblack_dpc, "lo:0=NA") - 4)/3
  aidblack_rpc = (Recode(aidblack_rpc, "lo:0=NA") - 4)/3
  envjob_dpc = (Recode(envjob_dpc, "lo:0=NA") - 4)/3
  envjob_rpc = (Recode(envjob_rpc, "lo:0=NA") - 4)/3

  ## ideology (factor/dummies)
  ideol = factor(Recode(libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                           , labels = c("Liberal","Moderate","Conservative"))
  ideol_lib = as.numeric(ideol=="Liberal")
  ideol_con = as.numeric(ideol=="Conservative")

  ## ideology (continuous, -1 to 1)
  ideol_ct = (Recode(libcpre_self, "lo:0=NA") - 4)/3

  ## strength of ideology
  ideol_str = abs(ideol_ct)

  ## party identification (factor/dummies)
  pid = factor(Recode(pid_x
                                , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                         , labels = c("Democrat","Independent","Republican"))
  pid_dem = as.numeric(pid=="Democrat")
  pid_rep = as.numeric(pid=="Republican")

  ## pid continuous
  pid_cont = (Recode(pid_x, "lo:0=NA") - 4)/3

  ## strength of partisanship
  pid_str = abs(pid_cont)

  ## interaction: pid * education
  educ_pid = educ_cont * pid_cont

  ## religiosity (church attendance)
  relig = (5 - Recode(relig_churchoft, "lo:0 = NA"))/5
  relig[relig_church != 1] = 0
  relig[relig_churchwk == 2] = 1

  ## age
  age = Recode(dem_age_r_x, "c(-2,-9,-8) = NA")

  ## log(age)
  lage = log(age)

  ## sex
  female = gender_respondent_x - 1

  ## race
  black = as.numeric(Recode(dem_raceeth_x, "lo:0 = NA") == 2)

  ## income
  faminc = (Recode(incgroup_prepost_x, "lo:0 = NA") -1)/27

  ## gender of inerviewer
  iwrmale = Recode(iwrdesc_pre_gender, "lo:0=NA; 2=0")

  ## spanish speaking respondent
  spanish = as.numeric(profile_spanishsurv == 1 |
                                   admin_pre_lang_start == 2 |
                                   admin_post_lang_start == 2)

  ## wordsum literacy test
  wordsum = (wordsum_setb == 5) + (wordsum_setd == 3)
                           + (wordsum_sete == 1) + (wordsum_setf == 3)
                           + (wordsum_setg == 5) + (wordsum_seth == 4)
                           + (wordsum_setj == 1) + (wordsum_setk == 1)
                           + (wordsum_setl == 4) + (wordsum_seto == 2))/10

  ## gender of interviewe
  iwr_female = Recode(iwrdesc_pre_gender,"lo:-1=NA")-1

  ## Pro-redistribution attitude: (new scale: 0-1)
  ## Services and spending tradeoff placement (1-7, max = increase spending)
  ## Standard of living (1-7, max = gov't should let each person get ahead on their own)
  redist = (Recode(spsrvpr_ssself, "lo:0 = NA") - Recode(guarpr_self, "lo:0 = NA") + 6)/12

  ## Support tax increases (new scale: 0-1)
  ## favor tax on millionaires
  ## raising personal inc tax for over 250K inc to reduce deficit
  tax = ((-Recode(milln_milltax_x, "lo:0 = NA") + 7)/3 + Recode(budget_rdef250k, "lo:0 = NA; 1=2; 2=0; 3=1"))/4

  ## Personality characteristics: Extraversion
  extraversion = Recode(tipi_extra, "lo:0 = NA")
  reserved = Recode(tipi_resv, "lo:0 = NA")
)

