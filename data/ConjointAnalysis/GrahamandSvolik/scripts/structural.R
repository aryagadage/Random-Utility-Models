
###  Environment   ====================================================================================================================================

rm(list=ls())

setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Dropbox/GrahamSvolik/"))

library(tidyverse)
library(reshape2)
library(estimatr)
library(coefplot)
library(xtable)
library(lmtest)  # only used once
source("scripts/helpers.R")

dat_full = read_csv("data/data_experiment.csv")

#  script options
fresh = F       # set to TRUE if you want to calculate the bootstrapped results from scratch
Nsim  = 1000    # if "fresh" is TRUE, set this lower to reduce the number of bootstrap sims, which will enable you to calcuate the results faster

#  Note: to use the bootstrap functions, you must install the plyr package.   <---- install.packages("plyr")
#  However, plyr's large number of conflicts with dplyr makes it inadvisable
#  to LOAD both packages at once. Therefore, the helpers.R script calls two
#  functions from plyr without loading the whole package.
#  If "fresh" is set to FALSE, you'll never need plyr.

""

###  Datasets       ============================================================================================================

dat_full = dat_full %>%
  mutate(
    Type = relevel(factor(Type), "D+ vs. D+")
  )

#  extra variables
norm <- function(x) {x/(2*sd(x, na.rm=TRUE))}

dat_full = dat_full %>%
  mutate(
    c_sameParty = as.numeric(c_party == pid_lean),
    o_sameParty = as.numeric(o_party == pid_lean),
    diff_sameParty = c_sameParty - o_sameParty,
    
    c_M = as.numeric(c_dem_type == "u"),
    o_M = as.numeric(o_dem_type == "u"),
    diff_M = c_M - o_M,
    
    c_M_pi = c_M * c_sameParty,
    o_M_pi = o_M * o_sameParty,
    diff_M_pi = c_M_pi - o_M_pi,
    
    c_M_rate = -1*c_dem_rate + 1,
    o_M_rate = -1*o_dem_rate + 1,
    diff_M_rate = c_dem_rate - o_dem_rate,
    
    c_M_rateN = norm(c_M_rate),
    o_M_rateN = norm(o_M_rate),
    diff_M_rateN = norm(c_M_rate - o_M_rate),
    
    c_experN = norm(c_age - c_exper),
    c_ageN = norm(c_age),
    o_experN = norm(o_age - o_exper),
    o_ageN = norm(o_age),
    diff_experN = c_experN - o_experN,
    diff_ageN = c_ageN - o_ageN,
    
    c_p1_rateN = norm((1-c_p1_rate)^2),
    c_p2_rateN = norm((1-c_p2_rate)^2),
    o_p1_rateN = norm((1-o_p1_rate)^2),
    o_p2_rateN = norm((1-o_p2_rate)^2),
    
    diff_p1_rateN = c_p1_rateN - o_p1_rateN,
    diff_p2_rateN = c_p2_rateN - o_p2_rateN,
    
    c_p1_rateRev = c_p1_rate^2 - 1,
    c_p2_rateRev = c_p2_rate^2 - 1,
    o_p1_rateRev = o_p1_rate^2 - 1,
    o_p2_rateRev = o_p2_rate^2 - 1,
    
    diff_p1_rateRev = c_p1_rateRev - o_p1_rateRev,
    diff_p2_rateRev = c_p2_rateRev - o_p2_rateRev
  )

#  dat_cand: excluding matchups with negative valence candidates
dat_cand  = dat_full %>% filter(!(c_dem_type == "v" | o_dem_type =="v"))

###  Analysis function    ===============

#  function to bootstrap parameters
fn_structureBoot = function(x, formula){
  x %>%
    do(
      glm(as.formula(formula), family=binomial(link="logit"), na.action=na.omit, data = ., weights = weight) %>%
        tidy
    ) %>%
    (function(x){
      denominator = sum(abs(x$estimate[2:length(x$estimate)]))
      rbind(
        x[,1:2],
        c("weight_M",     abs(x$estimate[2]) / denominator),
        c("weight_p1",    abs(x$estimate[3]) / denominator),
        c("weight_p2",    abs(x$estimate[4]) / denominator),
        c("weight_party", abs(x$estimate[5]) / denominator),
        c("priceM_p1",    abs(x$estimate[2]) / abs(x$estimate[3])),
        c("priceM_p2",    abs(x$estimate[2]) / abs(x$estimate[4])),
        c("priceM_party", abs(x$estimate[2]) / abs(x$estimate[5]))
      )
    })
}

###  Main estimate: party, policy, democracy only        ============================================================================================================

#  to be bootstrapped
fn_structureBoot(dat_cand, as.formula("c_win ~ diff_M + diff_p1_rateN + diff_p2_rateN + diff_sameParty"))

#  bootstrap
if(fresh==T){
  tab_structureMain = block_boot(
    dat_cand, 
    function(x) fn_structureBoot(x, formula = "c_win ~ diff_M + diff_p1_rateN + diff_p2_rateN + diff_sameParty"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T
  )
  write_csv(tab_structureMain, "paper/results/tab_structureMain.csv")
  
  tab_structureMain_natural = block_boot(
    dat_cand, 
    function(x) fn_structureBoot(x, formula = "c_win ~ diff_M + diff_p1_rateRev + diff_p2_rateRev + diff_sameParty"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T, quantileType=4
  )
  write_csv(tab_structureMain_natural, "paper/results/tab_structureMain_natural.csv")
  
  tab_structureMain_natural_withRatings = block_boot(
    dat_cand, 
    function(x) fn_structureBoot(x, formula = "c_win ~ diff_M_rate + diff_p1_rateRev + diff_p2_rateRev + diff_sameParty"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T, quantileType=4
  )
  write_csv(tab_structureMain_natural_withRatings, "paper/results/tab_structureMain_natural_withRatings.csv")
}
if(fresh==F){
  tab_structureMain = read_csv("paper/results/tab_structureMain.csv")
  tab_structureMain_natural = read_csv("paper/results/tab_structureMain_natural.csv")
  tab_structureMain_natural_withRatings = read_csv("paper/results/tab_structureMain_natural_withRatings.csv")
}

###  Full estimate: all attributes        ============================================================================================================

fn_structureBoot(dat_cand, as.formula("c_win ~ diff_M + diff_p1_rateN + diff_p2_rateN + diff_sameParty + diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Black + diff_race_Hispanic + diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer + diff_pro_Police_officer + diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner + diff_pro_Teacher + diff_experN + diff_ageN"))

if(fresh==T){
  tab_structureAll = block_boot(
    dat_cand, 
    function(x) fn_structureBoot(x, formula = "c_win ~ diff_M + diff_p1_rateN + diff_p2_rateN + diff_sameParty + diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Black + diff_race_Hispanic + diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer  + diff_pro_Police_officer + diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner + diff_pro_Teacher + diff_experN + diff_ageN"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T
  )
  write_csv(tab_structureAll, "paper/results/tab_structureAll.csv")
  
  tab_structureAll_natural = block_boot(
    dat_cand, 
    function(x) fn_structureBoot(x, formula = "c_win ~ diff_M + diff_p1_rateRev + diff_p2_rateRev + diff_sameParty + diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Black + diff_race_Hispanic + diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer  + diff_pro_Police_officer + diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner + diff_pro_Teacher + diff_experN + diff_ageN"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T
  )
  write_csv(tab_structureAll_natural, "paper/results/tab_structureAll_natural.csv")
  
  tab_structureAll_natural_withRatings = block_boot(
    dat_cand, 
    function(x) fn_structureBoot(x, formula = "c_win ~ diff_M_rate + diff_p1_rateRev + diff_p2_rateRev + diff_sameParty + diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Black + diff_race_Hispanic + diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer  + diff_pro_Police_officer + diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner + diff_pro_Teacher + diff_experN + diff_ageN"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T
  )
  write_csv(tab_structureAll_natural_withRatings, "paper/results/tab_structureAll_natural_withRatings.csv")
}
if(fresh==F){
  tab_structureAll = read_csv("paper/results/tab_structureAll.csv")
  tab_structureAll_natural = read_csv("paper/results/tab_structureAll_natural.csv")
  tab_structureAll_natural_withRatings = read_csv("paper/results/tab_structureAll_natural_withRatings.csv")
}

###  Table 2: structural estimates ---------------

tab_out = rbind(
  tab_structureMain %>% mutate(transformation = "Standardized", model = "Main"),
  tab_structureAll %>% mutate(transformation = "Standardized", model = "All"),
  tab_structureMain_natural %>% mutate(transformation = "Natural", model = "Main"),
  tab_structureAll_natural %>% mutate(transformation = "Natural", model = "All"),
  tab_structureMain_natural_withRatings %>% mutate(transformation = "Ratings", model = "Main"),
  tab_structureAll_natural_withRatings %>% mutate(transformation = "Ratings", model = "All")
) %>%
  filter(grepl("^weight_", term)) %>%
  mutate(model = paste(transformation, model)) %>% select(-transformation) %>%
  mutate(
    model = factor(model, unique(model)),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")"),
    Estimate = paste0("SPACES", trail_zero(estimate, 3), "STARS"),
    #Estimate = trail_zero(estimate, 3),
    Blank = "") %>%
  select(model, term, Estimate, CI) %>%
  gather(variable, value, -model, -term) %>%
  mutate(variable = factor(variable, c("Estimate", "CI"))) %>%
  arrange(model, term, variable)

tab_out = tab_out %>%
  spread(model, value) %>%
  mutate(
    Term = recode(term, weight_M="DELTA Democracy", weight_p1 = "ALPHA1 Economic policy", weight_p2 = "ALPHA2 Social policy", weight_party = "ALPHA3 Party")
  ) %>%
  group_by() %>%
  select(-term, -variable) %>%
  select(Term, everything()) 

for(i in nrow(tab_out):2){
  if(tab_out$Term[i] == tab_out$Term[i-1]) tab_out$Term[i] = ""
}

string_out = tab_out %>% group_by() %>%
  xtable() %>%
  print(include.rownames = F, caption.placement = "top", only.contents = T, hline.after = NULL, include.colnames = F) %>%
  strsplit("\\n") %>% .[[1]] %>%
  .[3:length(.)] %>%
  (function(x){
    x[((1:((length(x)-2)/2))*2)] = paste0(x[((1:((length(x)-2)/2))*2)], " \\\\[-1.1ex]")
    x
  }) %>%
  paste(collapse = "\n")
gsub("DELTA", "$\\\\delta$ ~", string_out) %>%
  gsub("ALPHA1", "$\\\\alpha_1$ ~", .)  %>%
  gsub("ALPHA2", "$\\\\alpha_2$ ~", .)  %>%
  gsub("ALPHA3", "$\\\\alpha_3$ ~", .)  %>%
  gsub("STARS", "$^{***}$", .)  %>%
  gsub("SPACES", "~~~", .)  %>%
  write("paper/figures/tab2_numbers.txt")

###  Table 2: calculate N -----------

dat_cand %>% filter(!is.na(c_win), !is.na(diff_p1_rate), !is.na(diff_p2_rate), candNum == 1) %>% nrow

dat_cand %>% filter(!is.na(c_win), !is.na(diff_p1_rate), !is.na(diff_p2_rate), !is.na(diff_M_rate), candNum == 1) %>% nrow

###  Main text: F test of difference        ============================================================================================================

glm_main = glm("c_win ~ diff_M + diff_p1_rateN + diff_p2_rateN + diff_sameParty",
               weights = weight, data = dat_cand)

glm_full = glm("c_win ~ diff_M + diff_p1_rateN + diff_p2_rateN + diff_sameParty + diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Black + diff_race_Hispanic + diff_pro_Teacher + diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer + diff_experN + diff_ageN",
               weights = weight, data = dat_cand)

anova(glm_main, glm_full, test = "F")  

lmtest::lrtest(glm_main, glm_full)

###  Main text: estimate pi        ============================================================================================================

fn_regOnly = function(x, formula){
  x %>%
    do(
      glm(formula, family=binomial(link="logit"), na.action=na.omit, data = ., weights = weight) %>%
        tidy
    ) %>% (function(x){x[,1:2]})
}

fn_regOnly(dat_cand, as.formula("c_win ~ diff_M + diff_M_pi + diff_p1_rateRev + diff_p2_rateRev + diff_sameParty"))

if(fresh==T){
  tab_structHet = block_boot(
    dat_cand, 
    function(x) fn_regOnly(x, formula = "c_win ~ diff_M + diff_M_pi + diff_p1_rateRev + diff_p2_rateRev + diff_sameParty"), 
    "id", Nsim, groups_in_output = "term", cand1_only = T, quantileType = 1,
    returnBoots = T
  )
  write_csv(tab_structHet, "paper/results/tab_structureHetero.csv")
}
if(fresh==F) tab_structHet = read_csv("paper/results/tab_structureHetero.csv")

tab_structHet = as.data.frame(tab_structHet)
b_econ   = tab_structHet[tab_structHet$term == "diff_p1_rateRev", c(2, which(names(tab_structHet)==1):ncol(tab_structHet))] %>% t %>% as.vector
b_social = tab_structHet[tab_structHet$term == "diff_p2_rateRev", c(2, which(names(tab_structHet)==1):ncol(tab_structHet))] %>% t %>% as.vector
b_party  = tab_structHet[tab_structHet$term == "diff_sameParty",  c(2, which(names(tab_structHet)==1):ncol(tab_structHet))] %>% t %>% as.vector
b_M      = tab_structHet[tab_structHet$term == "diff_M",          c(2, which(names(tab_structHet)==1):ncol(tab_structHet))] %>% t %>% as.vector %>% (function(x)x*-1)
b_int    = tab_structHet[tab_structHet$term == "diff_M_pi",       c(2, which(names(tab_structHet)==1):ncol(tab_structHet))] %>% t %>% as.vector

pi = b_int / b_M
mean(pi)
quantile(pi, c(.025, .975))

###  Appendix plot: isoelects     =============================================================================================

plotDat_isoelect = data.frame(
  sim = c("Main", 1:(length(b_econ)-1)),
  control_yint = (b_econ - b_party) / b_social,
  control_xint = (b_social - b_party) / b_econ,
  c_yint = (b_econ - b_party - b_M - b_int) / b_social,
  c_xint = (b_social - b_party - b_M - b_int) / b_econ,
  o_yint = (b_econ - b_party + b_M) / b_social,
  o_xint = (b_social - b_party + b_M) / b_econ
) %>%
  gather(variable, value, -sim) %>%
  mutate(candidate = gsub("_.+", "", variable),
         variable = gsub(".+_", "", variable)) %>%
  spread(variable, value) %>%
  mutate(sim = ifelse(sim=="Main", "Main", "Other"),
         Types = ifelse(sim!="Main", "Bootstrap", recode(candidate, control="D+ vs. D+", c="D- vs. D+", o="D+ vs. D-")),
         Types = factor(Types, c("D+ vs. D+", "D- vs. D+", "D+ vs. D-", "Bootstrap"))
         )

g = plotDat_isoelect %>%
  ggplot(aes(x = -1, xend=xint, y = yint, yend=-1, lty = Types, size = sim, alpha = sim, color = Types)) +
  scale_size_manual(values = c(1.25, .25), guide = "none") +
  scale_linetype_manual(values = c(1, 2, 3, 1)) +
  scale_alpha_manual(values = c(1, .025), guide = "none") +
  scale_color_manual(values = c("black", "blue", "red", "gray65")) +
  scale_x_continuous(limits = c(-3,3), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-3,3), expand = c(0, 0)) +
  geom_segment() +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        legend.key.width = unit(1.2, "cm")) +
  guides(linetype = guide_legend(override.aes = list(size = 1.1))) +
  labs(
    x = "Difference in Economic Policy Rating\n(in favor of co-partisan candidate 1)",
    y = "Difference in Social Policy Rating\n(in favor of co-partisan candidate 1)"
  )

g

ggsave("paper/figures/appendix_structural_isoelects.png", g, "png", width = 6, height = 4, dpi = 400)

plotDat = data.frame(
  b_econ = b_econ,
  b_social = b_social,
  b_party = b_party,
  b_M = b_M,
  b_int = b_int
)

control <- function(x) - x*(b_econ/b_social) - b_party/b_social
dminus_1 <- function(x) (- x*b_econ - b_party - b_M - b_int)/b_social
dminus_2 <- function(x) (- x*b_econ - b_party + b_M)/b_social

ggplot(data = plotDat) +
  stat_function(fun = control, size=2, aes(colour = "black")) + 
  stat_function(fun = dminus_2, size=2, lty=3, aes(colour = "red")) + 
  stat_function(fun = dminus_1, size=2, lty=2, aes(colour = "blue"))

tab_structureHetero = 
  rbind(
    tab_structHet
    # FILL IN --- compute the other terms out of the rows
  )


###  Appendix tables: full logit results ----------

tab_logit =
  full_join(
    tab_structureMain %>% 
      mutate_at(vars(estimate, q2.5, q97.5), as.numeric) %>%
      mutate(`CI 1` = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Term=term, `Estimate 1`=estimate, `SE 1`=se, `CI 1`),
    tab_structureAll  %>% 
      mutate_at(vars(estimate, q2.5, q97.5), as.numeric) %>%
      mutate(`CI 2` = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Term=term, `Estimate 2`=estimate, `SE 2`=se, `CI 2`)) %>%
  mutate(Term = gsub("_p1_", "_econ_", Term),
         Term = gsub("_p2_", "_social_", Term)) %>%
  filter(!grepl("^weight|^price", Term))

print(xtable(
  tab_logit, include.rownames = FALSE, caption.placement = "top", 
  caption = "Logistic regression results, standardized measures", label = "tab:structureLogit", digits = 3), 
  include.rownames = F, include.colnames = F, caption.placement = "top", only.contents = T, hline.after = NULL
) %>%
  write("paper/figures/appendix_structural_logit_col1-2.txt")

## SAME THING FOR NATURAL UNITS

tab_logit =
  full_join(
    tab_structureMain_natural %>% 
      mutate_at(vars(estimate, q2.5, q97.5), as.numeric) %>%
      mutate(`CI 1` = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Term=term, `Estimate 1`=estimate, `SE 1`=se, `CI 1`),
    tab_structureAll_natural  %>% 
      mutate_at(vars(estimate, q2.5, q97.5), as.numeric) %>%
      mutate(`CI 2` = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Term=term, `Estimate 2`=estimate, `SE 2`=se, `CI 2`)) %>%
  mutate(Term = gsub("_p1_", "_econ_", Term),
         Term = gsub("_p2_", "_social_", Term),
         Term = gsub("Rev$", "", Term)) %>%
  filter(!grepl("^weight|^price", Term))

print(xtable(
  tab_logit, include.rownames = FALSE, caption.placement = "top", 
  caption = "Logistic regression results, standardized measures", label = "tab:structureLogit", digits = 3), 
  include.rownames = F, include.colnames = F, caption.placement = "top", only.contents = T, hline.after = NULL
) %>%
  write("paper/figures/appendix_structural_logit_col3-4.txt")

## SAME THING FOR ESTIMATES W/ DEMOCRACY RATINGS

tab_logit =
  full_join(
    tab_structureMain_natural_withRatings %>% 
      mutate_at(vars(estimate, q2.5, q97.5), as.numeric) %>%
      mutate(`CI 1` = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Term=term, `Estimate 1`=estimate, `SE 1`=se, `CI 1`),
    tab_structureAll_natural_withRatings  %>% 
      mutate_at(vars(estimate, q2.5, q97.5), as.numeric) %>%
      mutate(`CI 2` = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Term=term, `Estimate 2`=estimate, `SE 2`=se, `CI 2`)) %>%
  mutate(Term = gsub("_p1_", "_econ_", Term),
         Term = gsub("_p2_", "_social_", Term),
         Term = gsub("Rev$", "", Term)) %>%
  filter(!grepl("^weight|^price", Term))

print(xtable(
  tab_logit, include.rownames = FALSE, caption.placement = "top", 
  caption = "Logistic regression results, standardized measures", label = "tab:structureLogit", digits = 3), 
  include.rownames = F, include.colnames = F, caption.placement = "top", only.contents = T, hline.after = NULL
) %>%
  write("paper/figures/appendix_structural_logit_col5-6.txt")

