
#  Environment       =======================================================================================

rm(list=ls())

setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Dropbox/GrahamSvolik/"))

library(tidyverse)
library(reshape2)
library(estimatr)
library(coefplot)
library(stargazer)
source("scripts/helpers.R")

w1_dem = read_csv("data/data_howDemocratic.csv")
dat_full = read_csv("data/data_experiment.csv")

Nsim = 5000
fresh = T

""

#  Datasets    =======================================================================================

dat_full = dat_full %>%
  mutate(
    Type = relevel(factor(Type), "D+ vs. D+")
  )

#  default dataset: excluding matchups with negative valence candidates
dat_cand  = dat_full %>% filter(!(c_dem_type == "v" | o_dem_type =="v"))


""

#  Section 3.1: Policy distance    =======================================================================================
#     Overall ATE    =======================================================================================

##    TEXT:  This change amounts to a 11.74\% decline in candidate~1's vote share 
##           when he adopts an undemocratic position, with the 95\% confidence interval of  ...

# function to compute the ATE, for block bootstrapping
fn_ATE_scalar = function(x) lm(c_win ~ Type, data = x, weights = weight)$coefficients[2]
fn_ATE_scalar(dat_cand)

# block bootstrapped estimate
vec_ATE_ovr = block_boot_scalar(dat_cand, fn_ATE_scalar, n = Nsim,
                                needed = c("id", "matchNum", "candNum", "c_win", "Type", "weight"), cand1_only = T)

# clustered SE estimates
tab_ATE_ovr_lm = lm_robust(c_win ~ diff_dem_type_u, data = dat_cand, weights = weight, clusters = id) %>% tidy
#   note: The regression estimates are more precise than the bootstrapped estimates because 
#         when there are two observations per candidate, the D+ vs. D+ mean is exactly 0.5. 
#         The bootstrap function conservatively uses only one candidate per choice, which 
#         introduces variability into the control mean intercept. If in the bootstrap you 
#         change cand1_only to FALSE, the estimates will be very similar.

# combine and save
tab_ATE_ovr = cbind(
  statistic = names(vec_ATE_ovr),
  bootstrap = vec_ATE_ovr,
  regression = c(tab_ATE_ovr_lm[2,c(2,3,6,7)], rep(NA, 4))
) %>% as.data.frame %>% mutate(bootstrap = as.numeric(bootstrap), regression = as.numeric(regression), statistic = as.character(statistic))
rm(tab_ATE_ovr_lm)

write_csv(tab_ATE_ovr, "paper/results/stat_ATE.csv")

#     Extreme subgroups     =======================================================================================

#     TEXT:  an electorate consisting entirely of "policy centrists"-the middle two bins-
#            would result in a resounding defeat of a candidate who would adopt an undemocratic platform...
#            In an electorate consisting entirely of the two most extreme subgroups, undemocratic candidates
#            would only lose...

#     ESTIMATOR: Compare the pooled CATE from [-1, -.8] and [.8, 1] to the pooled CATE from (-.2, 0] and [0, .2)

dat_cand %>%
  filter(!is.na(diff_prox)) %>%
  group_by(Type, diff_proxSq_bin) %>%
  fn_DIM() %>%
  group_by(Type, absdiff_proxSq_bin = abs(diff_proxSq_bin)) %>%
  summarize(estimate = sum(estimate)/2) %>%
  (function(x){
    bind_rows(
      x, 
      x %>% summarize(absdiff_proxSq_bin = -4, estimate = estimate[5] - estimate[1])
    )
  })

fn_compareModExtr = function(x){
  fn_DIM(x) %>%
    group_by(Type, absdiff_pRateSq_bin = abs(diff_pRateSq_bin)) %>%
    summarize(estimate = sum(estimate)/2) %>%
    (function(x){
      bind_rows(
        x, 
        x %>% summarize(absdiff_pRateSq_bin = -4, estimate = estimate[5] - estimate[1])
      )
    })
  
}

tab_pRate_absDiff = block_boot(
  dat_cand %>% filter(!is.na(diff_pRate)) %>% group_by(Type, diff_pRateSq_bin),
  fn_compareModExtr, "id", Nsim, groups_in_output = c("Type", "absdiff_pRateSq_bin")
)

write_csv(data.frame(tab_pRate_absDiff), "paper/results/stat_extremeSubgroups.csv")

#  Section 3.2: Partisanship       =======================================================================================
#     ATE by candidate partisanship    =======================================================================================
#     TEXT:  Overall, undemocratic candidates are penalized by a loss of ___ and ___ of voters
#            in the Rep D+ ...

fn_ATEbyCandParty      = function(x) x %>% do(tidy(lm(c_win ~ Type,         data = ., weights = weight))[,1:2])
fn_ATEbyCandParty_diff = function(x) x %>% do(tidy(lm(c_win ~ Type*c_Party, data = ., weights = weight))[,1:2])

#  to be bootstrapped
fn_ATEbyCandParty_diff(dat_cand %>% filter(m_candSameParty == "Cand diff"))

#  note that in the regression just above, 
#  the D- vs. D+ group is Dem D- vs Rep D+, and
#  the D+ vs. D- group is Dem D+ vs Rep D- i.e. Rep D- vs. Dem D+
#  to see this, compare:
fn_ATEbyCandParty(dat_cand %>% filter(m_candSameParty == "Cand diff") %>% group_by(c_Party))

tab_candPartyPunish =
  block_boot(
    dat_cand %>% filter(m_candSameParty == "Cand diff") %>% group_by(c_Party),
    fn_ATEbyCandParty, "id", Nsim
  ) %>% filter(term!="TypeD+ vs. D-")
write_csv(tab_candPartyPunish, "paper/results/stat_ATEbyCandParty.csv")

#  bootstrap
tab_candPartyPunishDiff =
  block_boot(
    dat_cand %>% filter(m_candSameParty == "Cand diff"),
    fn_ATEbyCandParty_diff, "id", Nsim
  ) %>% filter(!grepl("vs. D-", term))
write_csv(tab_candPartyPunishDiff, "paper/results/stat_diffInATEbyCandParty.csv")

#     Vote share for own vs other party       =======================================================================================

##    TEXT: 63.05% of our respondents support their own party in the control
#           condition; this number declines to 54.76% when a respondent's co-partisan adopts an
#           undemocratic position (-8.29%, CI: 5.74, 10.91

#     First the means, then the difference in means:

#  to be bootstrapped:
dat_cand %>%
  filter(pid_lean != "I", m_candSameParty == "Cand diff") %>%
  group_by(Type, m_candSameParty, m_respSameParty) %>%
  fn_binPref

#  bootstrap:
tab_candRespDiffPartyMeans =
  block_boot(
    dat_cand %>%
      filter(pid_lean != "I", m_candSameParty == "Cand diff") %>%
      group_by(m_respSameParty, Type),
    fn_binPref, "id", Nsim
  )
write_csv(tab_candRespDiffPartyMeans, "paper/results/stat_doubleStandardMeans.csv")

# clustered equivalent (does not appear in text)
dat_cand %>%
  filter(!is.na(m_respSameParty), m_candSameParty == "Cand diff") %>%
  group_by(m_respSameParty, Type) %>%
  do(
    lm_robust(c_win ~ 1, data = ., clusters = id, weights = weight) %>% tidy
  )

#      DIFFERENCE IN MEANS:

fn_DIM_reg = function(x) x %>% do(tidy(lm(c_win ~ Type, data = ., weights = weight))[,1:2])

#  to be bootstrapped:
fn_DIM_reg(dat_cand %>% filter(pid_lean != "I", m_candSameParty == "Cand diff", candNum == 1) %>% group_by(m_respSameParty))

#  bootstrap
tab_defect_candDiffParty =
  block_boot(
    dat_cand %>% filter(!is.na(m_respSameParty), m_candSameParty == "Cand diff") %>% group_by(m_respSameParty),
    fn_DIM_reg, "id", Nsim
  ) %>% filter(term!="TypeD+ vs. D-")
write_csv(tab_defect_candDiffParty, "paper/results/stat_doubleStandardATEs.csv")

#     Same vs different party contests       =======================================================================================

##    TEXT: Consistent with our theory, our respondents are more willing to punish undemocratic candidates in same-party 
##          than different-party contests: a candidate who adopts an undemocratic position is penalized by a loss 
##          of ____ and _____ of voters, respectively

#  function for bootstrap
fn_punish_candParty = function(x){
  bind_rows(
    x %>% do(
      tidy(lm(c_win ~ diff_dem_type_u, data = ., weights = weight))[,1:2]
    ),
    tidy(lm(c_win ~ diff_dem_type_u * m_candSameParty, data = x, weights = weight))[,1:2] %>% mutate(m_candSameParty = "Difference")
  )
}

#  example of what will be bootstrapped
fn_punish_candParty(dat_cand %>% group_by(m_candSameParty) %>% filter(candNum == 1))

#  bootstrap
tab_punish_candParty = 
  block_boot(
    dat_cand %>% group_by(m_candSameParty),
    fn_punish_candParty, "id", Nsim, cand1_only=T
  )
write_csv(tab_punish_candParty, "paper/results/stat_ATEsameVsDiffParty.csv")


##  In sum, the promise of primaries as a democratic check is undermined by partisan double standard. 
##  In same party contests, an undemocratic candidate's vote share declines by 16.98\% among respondents 
##  from the other party but only by 8.16\% among his co-partisans. 

fn_punish_respSameParty = function(x){
  x = fn_DIM(x) %>% group_by()
  rbind(
    x,
    data.frame(m_respSameParty = "Difference", Type = x$Type[1:3], estimate = x$estimate[1:3] - x$estimate[4:6])
  )
}

fn_punish_respParty = function(x){
  bind_rows(
    x %>% do(
      tidy(lm(c_win ~ Type, data = ., weights = weight))[,1:2]
    ),
    tidy(lm(c_win ~ Type * m_respSameParty, data = x, weights = weight))[,1:2] %>% mutate(m_respSameParty = "Difference")
  )
}

fn_punish_respParty(dat_cand %>% filter(m_candSameParty == "Cand same", !is.na(m_respSameParty)) %>% group_by(m_respSameParty))

tab_punish_candRespSameParty = 
  block_boot(
    dat_cand %>% filter(m_candSameParty == "Cand same", !is.na(m_respSameParty)) %>% group_by(m_respSameParty),
    fn_punish_respParty, "id", Nsim
  ) %>% filter(!grepl("D-$|D-:", term))
write_csv(tab_punish_candRespSameParty, "paper/results/tab_punish_candRespSameParty.csv")

#  DOUBLE STANDARD,
#  DIFFERENT PARTY 

fn_punish_respParty(dat_cand %>% filter(m_candSameParty == "Cand diff", !is.na(m_respSameParty)) %>% group_by(m_respSameParty))

tab_punish_candRespDiffParty = 
  block_boot(
    dat_cand %>% filter(m_candSameParty == "Cand same", !is.na(m_respSameParty)) %>% group_by(m_respSameParty),
    fn_punish_respParty, "id", Nsim
  ) %>% filter(!grepl("D-$|D-:", term))
write_csv(tab_punish_candRespDiffParty, "paper/results/tab_punish_candRespDiffParty.csv")


#  Section 3.3: Platform polarization    =======================================================================================
#     Reading off the figure ---------------

#     TEXT:  We see that 15.9% of respondents defect
#            from a D??? candidate when the two candidates take the same policy or have policy
#            differences that cancel out (CI: 13.6%, 18.3%); the defection rate declines to levels that are
#            statistically indistinguishable from 0 when both policies are as far apart as possible.

#     This statistic comes straight off the figure ---
#     see the Figure 6 calculation in "paper figs"
#     or results/tab_defect_platform.csv

#  Section 3.4: Menu of manipulation    =======================================================================================
#     F-test: differences between treatments ---------------

#     TEXT:  Footnote: an F-test...

model1 = lm(c_win ~ diff_dem_type_u, data = dat_cand %>% filter(candNum == 1))
model2 = lm(c_win ~ diff_dem_code_u_gerry2 + diff_dem_code_u_banProtest + diff_dem_code_u_court + diff_dem_code_u_execRule + diff_dem_code_u_gerry10 + diff_dem_code_u_journalists + diff_dem_code_u_limitVote, data = dat_cand %>% filter(candNum == 1))

anova(model1, model2) %>% summary

model3 = lm(c_win ~ diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Hispanic +
              diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer + diff_pro_Police_officer +
              diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner +
              diff_pro_Teacher + diff_respParty + 
              diff_p1_rate + diff_p2_rate +
              diff_dem_code_g_committee + diff_dem_code_g_officestructure + diff_dem_code_g_procedure +
              diff_dem_code_g_progEval + diff_dem_code_g_record + diff_dem_code_g_schedule + 
              diff_dem_code_u_banProtest +
              diff_dem_code_u_court + diff_dem_code_u_execRule + diff_dem_code_u_gerry2 + diff_dem_code_u_gerry10 +
              diff_dem_code_u_journalists + diff_dem_code_u_limitVote + diff_dem_code_v_affair + diff_dem_code_v_tax, 
            data = dat_cand %>% filter(candNum == 1), weights = weight)

library(car)

linearHypothesis(model2, c("diff_dem_code_u_banProtest = diff_dem_code_u_court", "diff_dem_code_u_banProtest = diff_dem_code_u_execRule", "diff_dem_code_u_banProtest = diff_dem_code_u_gerry2", "diff_dem_code_u_banProtest = diff_dem_code_u_gerry10", "diff_dem_code_u_banProtest = diff_dem_code_u_journalists", "diff_dem_code_u_banProtest = diff_dem_code_u_limitVote"), singular.ok = T)
linearHypothesis(model3, c("diff_dem_code_u_banProtest = diff_dem_code_u_court", "diff_dem_code_u_banProtest = diff_dem_code_u_execRule", "diff_dem_code_u_banProtest = diff_dem_code_u_gerry2", "diff_dem_code_u_banProtest = diff_dem_code_u_gerry10", "diff_dem_code_u_banProtest = diff_dem_code_u_journalists", "diff_dem_code_u_banProtest = diff_dem_code_u_limitVote"), singular.ok = T)

#  Conclusion: realistic scenarios table ------------

fn_realisticProgression = function(x){
  x = rbind(
    x %>% filter(m_candSameParty == "Cand diff") %>% mutate(Subset = "Cross-party contests"),
    x %>% filter(m_candSameParty == "Cand diff", noSamePolicy == 1) %>%  mutate(Subset = "Platform divergence"),
    x %>% filter(m_candSameParty == "Cand diff", partyPolicyAligned == 1) %>%  mutate(Subset = "Moderate party-policy alignment")
  ) %>% group_by(Subset)
  x %>% do(tidy(lm(c_win ~ diff_dem_type_u, data = ., weights = weight))[,1:2])
}

fn_realisticProgression(dat_cand)

if(fresh){
  
  tab_realisticProgression = block_boot(
    dat_cand %>% filter(!grepl("V", Type)),
    fn_realisticProgression, "id", Nsim, groups_in_output = c("Subset", "term"), 
    needed = c("id", "weight", "matchNum", "candNum", "c_win", "Type", "diff_dem_type_u", "m_candSameParty", "noSamePolicy", "partyPolicyAligned"), cand1_only = T
  )
  
  write_csv(tab_realisticProgression, "paper/results/tab_conclusion.csv")
}



