
###  Packages (run once)    =====================================

if(FALSE){
  install.packages("tidyverse")
  install.packages("plyr")         # won't be loaded, but need a couple functions
  install.packages("reshape2")
  install.packages("estimatr")
  install.packages("coefplot")
  install.packages("xtable")
  install.packages("grid")
  install.packages("gridExtra")
  install.packages("lemon")        # only needed in one section
  install.packages("dotwhisker")   # only needed for Figures 7 and 8
}

###  Environment   ====================================================================================================================================

rm(list=ls())

setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Dropbox/GrahamSvolik/"))

library(tidyverse)
library(reshape2)
library(estimatr)
library(coefplot)
library(xtable)
library(gridExtra)
library(grid)
library(lemon)        # only needed in one section
library(dotwhisker)   # only needed for Figures 7 and 8
source("scripts/helpers.R")

w1_dem = read_csv("data/data_howDemocratic.csv")
dat_full = read_csv("data/data_experiment.csv")

# script options
fresh = T       # FALSE: sometimes load pre-calculated results. TRUE: calculate everything from scratch
Nsim  = 1000    # how many bootstrap replicates?

""

###  Datasets   ====================================================================================================================================

dat_full = dat_full %>%
  mutate(
    Type = relevel(factor(Type), "D+ vs. D+"),
    pid_relative = factor(pid_relative, c("Strong\nopposite", "Weak\nopposite", "Lean\nopposite", "Independent",
                                          "Lean\nsame", "Weak\nsame", "Strong\nsame")),
    Party7 = factor(Party7, c("Strong\nDem", "Dem", "Lean\nDem", "Indep", "Lean\nRep", "Rep", "Strong\nRep"))
  )

#  default dataset: excluding matchups with negative valence candidates
dat_cand  = dat_full %>% filter(!(c_dem_type == "v" | o_dem_type =="v"))

#  dataset for multi-measure plots
d_allMeasures = 
  dat_full %>%
  select(
    id, candNum, matchNum,
    diff_proxSq, diff_prox, 
    diff_pRateSq, diff_pRate,
    diff_distSq, diff_distance,
    diff_pRankSq, diff_pRank,
    diff_proxSq_bin, diff_prox_bin,
    diff_pRateSq_bin, diff_pRate_bin,
    diff_distSq_bin, diff_distance_bin,
    diff_pRankSq_bin, diff_pRank_bin
  ) %>%
  melt(c("id", "candNum", "matchNum"))

d_allMeasures = d_allMeasures %>%
  mutate(
    var = case_when(
      grepl("^diff", variable) & grepl("_bin$", variable) ~ "value_diff_bin",
      grepl("^diff", variable) ~ "value_diff"
    ),
    Transformation = 
      case_when(grepl("Sq_|Sq$", variable) ~ "Squared", 
                TRUE ~ "Absolute") %>%
      factor(c("Absolute", "Squared")),
    Measure =
      case_when(
        grepl("prox",  variable) ~ "Party/Policy Bundle",
        grepl("pRate", variable) ~ "Policy Rating",
        grepl("dist",  variable) ~ "Spatial Distance",
        grepl("pRank", variable) ~ "Policy Rank"
      )
  )

d_allMeasures = d_allMeasures %>% select(-variable) %>% spread(var, value)

d_allMeasures = left_join(d_allMeasures, 
                          dat_full[,which(!grepl("diff_|absdiff_", names(dat_cand)) & !grepl("prox|pRate|rank", names(dat_cand)))], 
                          by = c("id", "candNum", "matchNum")) 

measures = d_allMeasures %>% select(Measure, Transformation) %>% distinct()

# dataset for plots faceted by democracy position
dem_codes = unique(dat_full$c_dem_code)[-2]

library(foreach)
d_eachUndem = foreach(i = 1:length(dem_codes), .combine=rbind) %dopar% {
  dat_full %>% filter(Type=="D+ vs. D+"|m_dem_code==dem_codes[i]) %>% mutate(Which = dem_codes[i])
}

d_eachUndem = d_eachUndem %>% 
  filter(
    !(c_dem_type == "g" & o_dem_type!="g")
  ) %>%
  mutate(Bad = as.numeric(c_dem_type!="g"))

###  Plot styling     ====================================================================================================================================

theRed = "red"
theBlue = "#263c73"

theme_allPlots = function(){
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.ticks = element_blank(),
      axis.title = element_text(size = 9, color = "gray5"),
      axis.title.x = element_text(margin = margin(6,0,0,0)),
      axis.title.y = element_text(margin = margin(0,6,0,0)),
      axis.text = element_text(size = 9, color = "gray5"),
      legend.text = element_text(size = 9, color = "gray5"),
      legend.title = element_text(size = 9, color = "gray5")
    )
}

theme_linePlot = function(){
    theme_allPlots() +
    theme(
      axis.text.x = element_text(hjust = .8, angle = 0),
      panel.border = element_blank(),
      legend.title = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}


plot_voteShare = function(x, mid=.5, linesize = .4, dotsize = 1, errorbarwidth = .02){
  thebreaks = unique(x$value) %>% gsub("^-0\\.", "-.", .) %>% gsub("^0\\.", "\\.", .)
  x %>%
    ggplot(aes(x = value, y = estimate, ymin = q2.5, ymax = q97.5, group=Type, color=Type, lty=Type, shape=Type)) +
    theme_linePlot() +
    scale_shape_manual(values = c(15, 2, 1)) +
    geom_hline(aes(yintercept = 0), color = "white") +
    geom_hline(aes(yintercept = 0), size = linesize*(3/4), color = "gray20") +
    
    geom_hline(aes(yintercept = .5), color = "white") +
    geom_hline(aes(yintercept = .5), size = linesize*(3/4), color = "gray20") +
    
    geom_hline(aes(yintercept = 1), color = "white") +
    geom_hline(aes(yintercept = 1), size = linesize*(3/4), color = "gray20") +
    
    geom_point(size=dotsize) +
    geom_errorbar(width=errorbarwidth, lty=1, size = linesize*(5/8)) +
    geom_path(size=linesize) +
    
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = c("gray10", theRed, "firebrick4")) +
    scale_linetype_manual(values = c(1,2,2)) +
    theme(panel.grid = element_line(size = linesize*(1/2))) +
    scale_x_continuous(breaks = unique(x$value), labels = thebreaks) +
    labs(x="Value", y="Estimate")
}

plot_defect = function(x, mid=0, linesize = .4, dotsize = 1, errorbarwidth = .02, theColor = theRed){
  thebreaks = unique(x$value) %>% gsub("^-0\\.", "-.", .) %>% gsub("^0\\.", "\\.", .)
  x %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = value, y = estimate, ymin = q2.5, ymax = q97.5, group = "")) +
    theme_allPlots() +
    geom_hline(aes(yintercept = 0), color = "white") +
    geom_hline(aes(yintercept = 0), size = linesize*(3/4), color = "gray20") +

    geom_path(lty=2, color = theColor, lty=2) +    
    geom_point(size=dotsize, shape=15, color = theColor) +
    geom_errorbar(width=errorbarwidth, lty=1, size = .25, color = theColor) +
    
    coord_cartesian(ylim = c(-.1, .3)) +
    scale_x_continuous(breaks = unique(x$value), labels = thebreaks) +
    theme(panel.grid = element_line(size = linesize*(1/2))) +
    labs(x="Value", y="Estimate")
}

""

###  Figure 2 calculations: group means and difference in means   ====================================================================================================================================

#  function for boostrapping
fn_meanDIM = function(x){
  summarize(x, Mean = weighted.mean(c_win, w = weight, na.rm=T)) %>%
    group_by(Measure, Transformation, value) %>%
    mutate(DIM = Mean - Mean[Type == "D+ vs. D+"]) %>%
    gather(Stat, estimate, Mean, DIM)
}

#  preview of what will be bootstrapped
fn_meanDIM(d_allMeasures %>% 
  rename(value = value_diff_bin) %>% 
  filter(!is.na(value)) %>% 
  group_by(Type, Measure, Transformation, value))

if(fresh==T){
  
  # bootstrapped estimates
  tab_meanDIM_boot = block_boot(
    d_allMeasures %>% 
      rename(value = value_diff_bin) %>% filter(!is.na(value)) %>% 
      group_by(Type, Measure, Transformation, value), 
    fn_meanDIM, "id", Nsim, cand1_only = F
  )
  
  # regression-based estimates w/ clustered SEs
  tab_mean_reg = 
    d_allMeasures %>% 
    rename(value = value_diff_bin) %>% 
    filter(!is.na(value)) %>% 
    group_by(Type, Measure, Transformation, value) %>%
    do(
      lm_robust(c_win ~ 1, data = ., weights = weight, clusters = id) %>% tidy
    )
  
  tab_mean_reg = tab_mean_reg %>%
    mutate(
      Stat = "Mean"
    ) %>%
    rename(
      estimate_reg = estimate
    ) %>%
    select(-term)
  
  tab_DIM_reg = 
    d_allMeasures %>% 
    rename(value = value_diff_bin) %>% 
    filter(!is.na(value), Type!="D+ vs. D-") %>% 
    group_by(Measure, Transformation, value) %>%
    do(
      lm_robust(c_win ~ Type, data = ., weights = weight, clusters = id) %>% tidy
    )

  tab_DIM_reg = tab_DIM_reg %>%
    group_by() %>%
    mutate(
      Stat = "DIM",
      Type = gsub("Type", "", term)
    ) %>%
    filter(term!="(Intercept)") %>%
    rename(
      estimate_reg = estimate
    ) %>%
    select(-term)
  
  # combine bootstrapped and regression-based
  tab_meanDIM = bind_rows(
    tab_mean_reg,
    tab_DIM_reg
  ) %>%
    left_join(tab_meanDIM_boot, .) %>% group_by()
  
  write_csv(tab_meanDIM, "paper/results/fig2_results.csv")
}
if(fresh==F) tab_meanDIM = read_csv("paper/results/fig2_results.csv")

tab_mean = 
  tab_meanDIM %>% 
  filter(!grepl("-$", Type), Stat=="Mean") %>%
  group_by() %>% 
  mutate(Type = factor(Type, c("D+ vs. D+", "D- vs. D+", "V- vs. D+")))

tab_defect = 
  tab_meanDIM %>% 
  mutate(
    estimate = -1*estimate,
    q2.5 = -1*q2.5,
    q97.5 = -1*q97.5
  ) %>%
  filter(Type=="D- vs. D+", Stat=="DIM") %>%
  group_by() %>% 
  mutate(Type = factor(Type, c("D+ vs. D+", "D- vs. D+", "V- vs. D+")))

###  Figure 2 plot         ====================================================================================================================================

g1 = plot_voteShare(tab_mean %>% filter(Measure=="Policy Rating", Transformation=="Squared", Type!="V- vs. D+"), 
                    linesize = .5, dotsize = 1.5, errorbarwidth = .05) +
  scale_x_continuous(breaks = (-5:5)/5, labels = c("-1", "-.8", "-.6", "-.4", "-.2", "0", ".2", ".4", ".6", ".8", "1")) +
  scale_y_continuous(limits = 0:1, expand = c(0, .00001)) +
  theme(strip.text = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = .5),
        legend.position = c(.8, .15),
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(.125, "cm"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.margin = margin(0,0,0,0),
        axis.title = element_blank(),
        axis.text = element_text(size = 8, color = "gray5"),
        plot.title = element_text(size = 9, hjust = .5),
        panel.border = element_rect(color = "gray5", fill = "transparent")) +
  labs(title = "Fraction voting for candidate 1", x = "Difference in policy proximity (in favor of candidate 1)")

g2 = plot_defect(tab_defect %>% filter(Transformation=="Squared", Measure=="Policy Rating"),
                    dotsize = 1.5, errorbarwidth = .05) +
  scale_x_continuous(breaks = (-5:5)/5, labels = c("-1", "-.8", "-.6", "-.4", "-.2", "0", ".2", ".4", ".6", ".8", "1")) +
  scale_y_continuous(limits = c(-.1, .3), expand = c(0, .00001)) +
  theme(
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = .5),
    axis.text = element_text(size = 8, color = "gray5"),
    axis.title = element_blank(),
    panel.border = element_rect(color = "gray5", fill = "transparent"),
    plot.title = element_text(size = 9, hjust = .5),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    x = "Difference in policy proximity (in favor of candidate 1)", 
    title = "Fraction defecting when candidate 1 is D-"
  )

g = grid.arrange(g1, g2, nrow = 1, 
                 bottom = textGrob("Difference in policy proximity (in favor of candidate 1)", gp = gpar(fontsize = 9)))
g
ggsave("paper/figures/fig2.pdf", g, "pdf", width = 6.5, height = 3.1)

g = plot_voteShare(tab_mean, errorbarwidth = .05) + 
  facet_grid(Transformation ~ Measure, switch = "y") +
  labs(y = "Fraction voting for candidate 1", x = "Value") +
  scale_x_continuous(breaks = (-5:5)/5, labels = c("-1", "-.8", "-.6", "-.4", "-.2", "0", ".2", ".4", ".6", ".8", "1")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme(axis.text.x = element_text(size = 8),
        legend.position = "bottom",
        panel.border = element_rect(size = .25, color = "black", fill = "transparent"),
        panel.spacing.x = unit(.5, "cm"))
g
ggsave("paper/figures/appendix_fig2a_allMeasures.pdf", g, "pdf", width = 8.5, height = 6)

g = plot_defect(tab_defect, errorbarwidth = .05) + 
  facet_grid(Transformation ~ Measure, switch = "y") +
  scale_x_continuous(breaks = (-5:5)/5, labels = c("-1", "-.8", "-.6", "-.4", "-.2", "0", ".2", ".4", ".6", ".8", "1")) +
  theme(panel.border = element_rect(color = "gray10", fill = "transparent"),
        panel.spacing.x = unit(.5, "cm"),
        axis.text.x = element_text(color = "black", size = 8))
g
ggsave("paper/figures/appendix_fig2b_allMeasures.pdf", g, "pdf", width = 8.5, height = 6)


###  Appendix: Numerical results for Fig. 2 (appendix)   ====================================================================================================================================

out = print(
  xtable(
    tab_meanDIM %>%
      group_by() %>%
      mutate(
        `Boot CI` = paste0("(", round(q2.5, 3), ", ", round(q97.5, 3), ")"),
        `Cluster CI` = paste0("(", round(conf.low, 3), ", ", round(conf.high, 3), ")"),
        Value = as.character(round(value, 2))
      ) %>%
      filter(Stat == "Mean", Type %in% c("D+ vs. D+", "D- vs. D+", "V- vs. D+")) %>%
      arrange(Measure, Transformation, value) %>%
      select(Measure, Transformation, Type, Value, Estimate = estimate, 
             `Boot SE` = se, `Boot CI`, `Cluster SE`=std.error, `Cluster CI`), digits = 3
  ), digits = 3, include.rownames = F, size = "footnotesize"
)

out = strsplit(out, "\n")[[1]]

out[10:(length(out)-4)] %>% 
  paste(collapse = "\n") %>%
  write("paper/figures/tab_fig2a.txt")

out = print(
  xtable(
    tab_meanDIM %>%
      mutate(
        `Boot CI` = paste0("(", round(q2.5, 3), ", ", round(q97.5, 3), ")"),
        `Cluster CI` = paste0("(", round(conf.low, 3), ", ", round(conf.high, 3), ")"),
        Value = as.character(round(value, 2))
      ) %>%
      filter(Stat == "DIM", Type %in% c("D- vs. D+", "V- vs. D+")) %>%
      arrange(Measure, Transformation, value) %>%
      select(Measure, Transformation, Type, Value, Estimate = estimate, 
             `Boot SE` = se, `Boot CI`, `Cluster SE`=std.error, `Cluster CI`), digits = 3
  ), digits = 3, include.rownames = F, size = "footnotesize"
)

out = strsplit(out, "\n")[[1]]

out[10:(length(out)-4)] %>% 
  paste(collapse = "\n") %>%
  write("paper/figures/tab_fig2b.txt")

###  Appendix: Figure 2a for each undem treatment & distance measure   ====================================================================================================================================

#  preview of what will be bootstrapped
fn_binPref(d_allMeasures %>% 
             filter(Type %in% c("D+ vs. D+", "D- vs. D+", "V- vs. D+")) %>%
             mutate(value = value_diff_bin) %>% 
             group_by(Type, m_dem_code, Measure, Transformation, value) %>% 
             filter(!is.na(value)))

#  bootstrap
if(fresh==T){
  tab_meanByTrt = block_boot(
    d_allMeasures %>% 
      filter(Type %in% c("D+ vs. D+", "D- vs. D+", "V- vs. D+")) %>%
      mutate(value = value_diff_bin) %>% 
      group_by(Type, m_dem_code, Measure, Transformation, value) %>% 
      filter(!is.na(value)),
    fn_binPref, "id", Nsim, cand1_only = F
  )
  write_csv(tab_meanByTrt, "paper/results/fig2_byUndem.csv")
}
if(fresh==F) tab_meanByTrt = read_csv("paper/results/fig2_byUndem.csv")

#  duplicate rows in the table, so that the "generic" line will appear in every facet of the plot
library(foreach)
demCodes = unique(tab_meanByTrt$m_dem_code)
tab_meanByTrt2 = foreach(i = 1:length(demCodes), .combine = rbind) %dopar% {
  out = tab_meanByTrt %>%
    filter(m_dem_code %in% c(demCodes[i], "Generic")) %>% 
    mutate(Which = demCodes[i])
  out$m_dem_code[out$m_dem_code==demCodes[i]] = "Negative"
  out
}
tab_meanByTrt2 = left_join(tab_meanByTrt2, read_csv("data/key_amce.csv")) %>% group_by()
tab_meanByTrt2 = tab_meanByTrt2 %>% mutate(Type = recode_factor(Type, `D+ vs. D+`="Neutral", `D- vs. D+`="Negative", `V- vs. D+`="Negative")) %>% filter(Which!="Generic")

fn_DIM_allTreat = function(x) x %>% do(tidy(lm(c_win ~ m_dem_code, data = ., weights = weight))[,1:2])
fn_DIM_allTreat(d_allMeasures %>% filter(candNum == 1))

#  Function to create each plot
plot_meanByTrt = function(plotDat, textDat, measure, transform){
  plotDat = plotDat %>% filter(Measure==measure, Transformation==transform)
  plot_voteShare(plotDat) +
    theme(axis.text.x = element_text(size = 7, angle = 40))
}

#  Example of what the loop below produces
g = plot_meanByTrt(tab_meanByTrt2, tab_ATEbyUndem2, "Policy Rating", "Absolute") +
  facet_wrap(~label2, nrow=2)
lemon::reposition_legend(g, "center", panel = "panel-5-2")

for(i in 1:nrow(measures)){
  name = paste("demByTrt", measures$Measure[i], measures$Transformation[i])
  name = gsub(" |/", "_", name)
  g = plot_meanByTrt(tab_meanByTrt2, tab_ATEbyUndem2, measures$Measure[i], measures$Transformation[i]) +
    scale_x_continuous(breaks = (-5:5)/5, labels = c("-1", "-.8", "-.6", "-.4", "-.2", "0", ".2", ".4", ".6", ".8", "1")) +
    facet_wrap(~label2, nrow=2) +
    theme(strip.text = element_text(size = 8),
          axis.text.x = element_text(size = 6.5, angle = 0))
  g = lemon::reposition_legend(g, "center", panel = "panel-5-2")
  ggsave(paste0("paper/figures/", name, ".pdf"), 
         g, 
         "pdf", width = 7, height = 4)
}

###  Figure 3: Different party contests by 7-point PID    ====================================================================================================================================

#  this is what will be bootstrapped
fn_binPref(dat_full %>% filter(m_candSameParty=="Cand diff") %>% mutate(c_win = R_win) %>% group_by(pid7, Type, c_Party))

#  bootstrap
if(fresh==T){
  tab_diffPartyDem = block_boot(
    dat_full %>% filter(m_candSameParty=="Cand diff", !is.na(pid7)) %>% 
      mutate(c_win = R_win) %>% 
      group_by(Party7, Type, c_party),
    fn_binPref, "id", Nsim, cand1_only = F
  )
  write_csv(tab_diffPartyDem, "paper/results/fig3_results.csv")
}
if(fresh==F) tab_diffPartyDem = read_csv("paper/results/fig3_results.csv")

#  prepare data for plotting
tab_diffPartyDem2 = 
  tab_diffPartyDem %>%
  filter(!grepl("-$", Type), !grepl("V-", Type)) %>%
  group_by() %>%
  mutate(
    Candidates = recode(paste0(c_party, Type), `DD- vs. D+` = "Rep D+ vs. Dem D-", 
                        `RD- vs. D+` = "Rep D- vs. Dem D+", .default = "Rep D+ vs. Dem D+"),
    Candidates = factor(Candidates, c("Rep D+ vs. Dem D-", "Rep D+ vs. Dem D+", "Rep D- vs. Dem D+"))
  )

tab_diffPartyDem2 = tab_diffPartyDem2 %>% mutate(
  Party = recode_factor(Party7, `Strong\nDem`="Strong\nDemocrat\n(24.1%)",
                        Dem = "\nDemocrat\n(13.6%)", `Lean\nDem`="Lean\nDemocrat\n(7.7%)",
                        Indep = "\nIndependent\n(14.7%)", `Lean\nRep`="Lean\nRepublican\n(7.4%)",
                        Rep = "\nRepublican\n(14.1%)", `Strong\nRep`="Strong\nRepublican\n(18.5%)")
) %>%
  arrange(Party)

#  plot
g = ggplot(tab_diffPartyDem2,# %>% filter(!(type=="g" & c_party=="D"), type!="v"), 
           aes(x = Party, y = estimate, ymin = q2.5, ymax = q97.5, color = Candidates, shape = Candidates, lty = Candidates, group = paste0(Candidates, c_party))) +
  theme_linePlot() +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, size = 8), 
        legend.position = c(.8,.16),
        legend.key = element_blank(),
        legend.key.height = unit(.5, "cm"),
        legend.background = element_blank()) +
  scale_color_manual(values = c(theBlue, "gray10", theRed)) +
  scale_linetype_manual(values = c(2, 1, 3)) +
  scale_shape_manual(values = c(15, 19, 2)) +
  
  geom_hline(aes(yintercept = 0), color = "white") +
  geom_hline(aes(yintercept = 0), color = "gray20") +
  geom_hline(aes(yintercept = .5), color = "white") +
  geom_hline(aes(yintercept = .5), color = "gray20") +
  geom_hline(aes(yintercept = 1), color = "white") +
  geom_hline(aes(yintercept = 1), color = "gray20") +
  scale_y_continuous(expand = c(0.01,0.01), limits = 0:1) +
  #scale_x_discrete(expand = c(0.01,0.01)) +
  
#  geom_rect(aes(xmin = "Lean\nRepublican\n(7.4%)", xmax=Inf, ymin = 0.02, ymax = 0.25), color = "gray75", fill = "white", show.legend = F, lty = 1, size = .25) +
  
  geom_point() +
  geom_path() +
  geom_errorbar(width = .1, lty=1) +
  labs(
    x = "Respondent partisanship", y = "Fraction voting for the Republican candidate"
  )

ggsave("paper/figures/fig3.pdf", g, "pdf", width = 5.5, height = 3.75)

#  accompanying appendix table:
print(
  xtable(
    tab_diffPartyDem2 %>%
      mutate(
        CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")
      ) %>%
      select(Respondent = Party7, Candidates, `R vote share` = estimate, 
             `Bootstrap SE`=se, `Bootstrap CI`=CI)%>%
      distinct, 
    digits = 3, caption = "Numerical results for Figure 3"
  ), digits = 3, include.rownames = F, size = "small", caption.placement = "top"
) %>%
  write("paper/figures/appendix_fig3_numerical.txt")


###  Figure 4: Partisan double standard    ====================================================================================================================================

fn_DIM_reg = function(x) x %>% do(tidy(lm(c_win ~ Type, data = ., weights = weight))[,1:2])

#  to be bootstrapped:
dat_cand %>%
  filter(m_candSameParty == "Cand diff", candNum == 1) %>%
  group_by(pid_relative) %>%
  fn_DIM_reg()
    #  notice that D- vs. D+ strong same = D+ vs. D- strong opposite,
    #              D- vs. D+ weak same   = D+ vs. D- weak opposite, etc.

#  bootstrap:
if(fresh==T){
  tab_partyRelative =       #  note: to obtain the smaller standard errors add cand1_only = F to block_boot(),
    block_boot(             #        which fixes the D+ vs. D+ baseline at 0.5 (which it is by design)
      dat_cand %>%
        filter(m_candSameParty == "Cand diff") %>%
        group_by(pid_relative),
      fn_DIM_reg, "id", Nsim
    ) %>% filter(term != "TypeD+ vs. D-")
  write_csv(tab_partyRelative, "paper/results/fig4_results.csv")
}
if(fresh == F) tab_partyRelative = read_csv("paper/results/fig4_results.csv")

tab_partyRelative = tab_partyRelative %>% mutate(Type = gsub("Type", "", term)) %>% group_by()

# regression equivalent
dat_cand %>%
  filter(m_candSameParty == "Cand diff") %>%
  mutate(Type = relevel(factor(Type), "D+ vs. D+")) %>%
  group_by(pid_relative) %>%
  do(
    lm_robust(c_win ~ Type, weights = weight, clusters = id, data = .) %>% tidy
  )

# plot  
g = tab_partyRelative %>%
  mutate(pid_relative = factor(pid_relative, c("Strong\nopposite", "Weak\nopposite", "Lean\nopposite", "Independent", "Lean\nsame", "Weak\nsame", "Strong\nsame"))) %>%
  arrange(pid_relative) %>%
  filter(Type == "D- vs. D+") %>%
  ggplot(
    aes(x = pid_relative, y = -1*estimate, ymin = -1*q2.5, ymax = -1*q97.5, group = "")
  ) +
  theme_linePlot() +
  theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
  scale_color_manual(values = c(theRed)) +
  scale_shape_manual(values = c(15, 2, 19)) +
  geom_hline(aes(yintercept = 0), color = "white") +
  geom_hline(aes(yintercept = 0), size = 3/4, color = "gray20") +
  geom_point(shape = 15, color = theRed) +
  geom_path(color = theRed, lty = 2) +
  geom_errorbar(width = .1, lty=1, alpha = .8, color = theRed) +
  labs(
    x = "Respondent and D- candidate partisanship", y = "Fraction defecting from D- candidate"
  )
g
ggsave("paper/figures/fig4.pdf", g, width = 5.5, height = 3.5)

tab_partyRelative %>%
  mutate(
    Estimate = trail_zero(estimate, 3),
    se = trail_zero(se, 3),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")"),
    pid_relative = factor(pid_relative, levels(dat_full$pid_relative))
  ) %>%
  arrange(pid_relative) %>%
  filter(Type == "D- vs. D+") %>%
  select(Party = pid_relative, Estimate,
         `Bootstrap SE`=se, `Bootstrap CI`=CI) %>%
  xtable(digits = 3, caption = "Numerical results for Figure 4") %>%
  print(digits = 3, include.rownames = F, size = "small", caption.placement = "top") %>%
  write("paper/figures/appendix_fig4_numerical.txt")

###  Appendix: Regression test for heterogeneity   ====================================================================================================================================

fn_partyTest = function(x){
  rbind(
    lm(c_win ~ diff_dem_type_u*`Party Strength`,                            data = x, weights = weight) %>% tidy %>% mutate(Model = "No controls"),
    lm(c_win ~ diff_dem_type_u*`Party Strength` + c_respParty,              data = x, weights = weight) %>% tidy %>% mutate(Model = "Same party control"),
    lm(c_win ~ diff_dem_type_u*`Party Strength` + c_respParty + diff_pRank, data = x, weights = weight) %>% tidy %>% mutate(Model = "Party and policy controls")
  )[,c(1,6,2)]
}

fn_partyTest(dat_cand %>% mutate(`Party Strength` = ifelse(pid7 != 0, abs(pid7) - 1, NA_real_)/2))

if(fresh){
  tab_partyTest =
    block_boot(
      dat_cand %>% mutate(`Party Strength` = ifelse(pid7 != 0, abs(pid7) - 1, NA_real_)/2), 
      fn_partyTest, "id", Nsim, groups_in_output = c("Model", "term"),
      needed = c("id", "weight", "matchNum", "candNum", "c_win", "Type", "diff_dem_type_u", "Party Strength", "c_respParty", "diff_pRank"), 
      cand1_only = T
    )
  write_csv(tab_partyTest, "paper/results/tab_partyTest.csv")
}
if(!fresh) tab_partyTest = read_csv("paper/results/tab_partyTest.csv")

tab_partyTest = tab_partyTest %>% 
  mutate(
    Term = gsub("diff_dem_type_u", "(M1-M2)", term) %>% gsub("`Party Strength`", "Party strength", .) %>% 
      gsub("diff_pRank", "(P1-P2)", .) %>% gsub("c_respParty", "Same-party candidate 1", .) %>% 
      gsub(":", " x ", .),
    Term = factor(Term, unique(Term)),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")
  ) %>%
  mutate(Model = factor(Model, c("No controls", "Same party control", "Party and policy controls"))) %>%
  arrange(Model, Term) %>%
  mutate(
    Model = ifelse(term == "(Intercept)", as.character(Model), "")
  ) %>%
  select(
    Model, Term, Estimate = estimate, SE = se, CI
  )

print(
  xtable(
    tab_partyTest, digits = 3, caption = "Regression test to accompany Figure 4", label = "tab:partyRegTest"
  ), include.rownames = F, caption.placement = "top", size = "small"
) %>%
  write("paper/figures/appendix_partyStrengthRegTest.txt")

###  Figure 5: Same party contests by 7-point PID   ====================================================================================================================================

#  to be bootstrapped:
dat_cand %>% 
  filter(m_candSameParty=="Cand same", Type == "D- vs. D+", !is.na(pid7)) %>% 
  mutate(c_win = 1-moreD_win) %>% 
  group_by(Party7, Type, c_Party) %>%
  fn_binPref

#  bootstrap:
if(fresh==T){
  tab_samePartyDem = block_boot(
    dat_cand %>% 
      filter(m_candSameParty=="Cand same", Type == "D- vs. D+", !is.na(pid7)) %>% 
      mutate(c_win = 1-moreD_win) %>% 
      group_by(Party7, Type, c_Party),
    fn_binPref, "id", Nsim, cand1_only = F
  )
  write_csv(tab_samePartyDem, "paper/results/fig5_results.csv")
}
if(fresh==F) tab_samePartyDem = read_csv("paper/results/fig5_results.csv") %>% group_by()

tab_samePartyDem = tab_samePartyDem %>% mutate(Candidates = recode(c_Party, Democrat = "Dem D- vs. Dem D+", Republican = "Rep D- vs. Rep D+"))

tab_samePartyDem = tab_samePartyDem %>% mutate(
  Party = recode_factor(Party7, `Strong\nDem`="Strong\nDemocrat\n(24.1%)",
                        Dem = "\nDemocrat\n(13.6%)", `Lean\nDem`="Lean\nDemocrat\n(7.7%)",
                        Indep = "\nIndependent\n(14.7%)", `Lean\nRep`="Lean\nRepublican\n(7.4%)",
                        Rep = "\nRepublican\n(14.1%)", `Strong\nRep`="Strong\nRepublican\n(18.5%)")
)  %>% filter(Type == "D- vs. D+") %>% arrange(Party)

g = ggplot(tab_samePartyDem, 
       aes(x = Party, y = estimate, ymin = q2.5, ymax = q97.5, color = Candidates, lty = Candidates, group = Candidates, shape = Candidates)) +
  theme_linePlot() +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, size = 8),
        legend.key.width = unit(1.2, "cm"),
        legend.position = c(.81, .87),
        legend.background = element_blank(),
        legend.key = element_blank()) +
  scale_color_manual(values = c(theBlue, theRed)) +
  scale_linetype_manual(values = c(2, 3)) +
  
  scale_shape_manual(values = c(15, 2, 19)) +
  scale_y_continuous(expand = c(0.01,0.01)) +
  
  geom_hline(aes(yintercept = 0), color = "white") +
  geom_hline(aes(yintercept = 0), color = "gray20") +
  
  geom_hline(aes(yintercept = .5), color = "white") +
  geom_hline(aes(yintercept = .5), color = "gray20") +
  
  geom_hline(aes(yintercept = 1), color = "white") +
  geom_hline(aes(yintercept = 1), color = "gray20") +
  
  geom_path() +
  geom_errorbar(width = .1, lty=1, alpha = .8, show.legend = F) +
  
  geom_point() +
  labs(
    x = "Respondent partisanship", y = "Fraction choosing candidate 1"
  )
ggsave("paper/figures/fig5.pdf", g, "pdf", width = 5.75, height = 3.75)

# appendix table
tab_samePartyDem %>% group_by() %>%
  mutate(
    Party7 = factor(Party7, levels(dat_full$Party7)),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")"),
    estimate = trail_zero(estimate, 3),
    se = trail_zero(se, 3)
  ) %>%
  select(Respondent = Party7, Candidates, `D- vote share` = estimate, 
         `Bootstrap SE`=se, `Bootstrap CI`=CI)%>%
  distinct %>% 
  xtable(digits = 3, caption = "Numerical results for Figure 5") %>%
  print(digits = 3, include.rownames = F, size = "small", caption.placement = "top") %>%
  write("paper/figures/appendix_fig5_numerical.txt")

###  Figure 6: Platform distance     ====================================================================================================================================

# bootstrap function
fn_defectPlatform = function(x){
  rbind(
    x %>% mutate(Subset = "All"),
    x %>% mutate(Subset = "Distinct platforms") %>% filter(noSamePolicy)
  ) %>%
    group_by(Subset, m_platformDist) %>%
    do(
      tidy(lm(c_win ~ Type, data = ., weights = weight))[,1:2]
    )
}

# example bootstrap
fn_defectPlatform(dat_cand %>% filter(candNum == 1))

# bootstrap
if(fresh==T){
  tab_defect_Platform =    #  note: to obtain the smaller standard errors add cand1_only = F to block_boot(),
    block_boot(            #        which fixes the D+ vs. D+ baseline at 0.5 (which it is by design)
      dat_cand,
      fn_defectPlatform, "id", Nsim, groups_in_output = c("Subset", "m_platformDist", "term"), cand1_only = T
    ) %>% filter(term != "TypeD+ vs. D-")
  write_csv(tab_defect_Platform, "paper/results/fig6_results.csv")
}
if(fresh==F) tab_defect_Platform = read_csv("paper/results/fig6_results.csv")

plot_platform = function(x){
  x %>% 
    mutate(value = m_platformDist,
           Type = gsub("^Type", "", term)) %>%
    filter(Type == "D- vs. D+") %>%
    mutate(estimate = -1*estimate, q2.5 = -1*q2.5, q97.5 = -1*q97.5) %>%
    plot_defect(dotsize = 2, errorbarwidth=0.06, linesize=1) +
    coord_cartesian(ylim = c(-.1, .2)) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_text(angle = 0)
    ) +
    labs(
      x = "Mean absolute distance between candidate platforms",
      y = "Fraction defecting from D- candidate"
    ) +
    scale_x_continuous(breaks = unique(tab_defect_Platform$m_platformDist))
}

g = tab_defect_Platform %>% plot_platform + facet_wrap(~Subset)
g
ggsave("paper/figures/appendix_fig6_alternative.pdf", g, "pdf", width = 5.5, height = 3)

g = tab_defect_Platform %>% filter(Subset=="All") %>% plot_platform
g
ggsave("paper/figures/fig6.pdf", g, "pdf", width = 6, height = 3.5)

tab_defect_Platform %>%
  filter(Subset == "All", term != "(Intercept)") %>%
  mutate(
    `Platform Distance` = trail_zero(m_platformDist, 1),
    Estimate = trail_zero(estimate, 3),
    se = trail_zero(se, 3),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")
  ) %>%
  group_by() %>%
  select(`Platform Distance`, Estimate, SE=se, CI) %>%
  xtable(digits = 3, caption = "Numerical results for Figure 6") %>%
  print(digits = 3, include.rownames = F, size = "small", caption.placement = "top") %>%
  write("paper/figures/appendix_fig6_numerical.txt")


###  Appendix: Regression test       ==================================================================================================================================  

if(fresh){
  
  fn_platformStat =
    function(x){
      x %>% do(
        lm(c_win ~ diff_dem_type_u * m_platformDist, data = ., clusters = id, weights = weight) %>% tidy
      ) %>% .[1:3]
    }
  
  fn_platformStat(rbind(
    dat_cand %>% 
      mutate(Subset = "All"),
    dat_cand %>% filter(noSamePolicy) %>%
      mutate(Subset = "Distinct platforms")
  ) %>% 
    group_by(Subset))
  
  tab_platformStat =
    block_boot(
      rbind(
        dat_cand %>% 
          mutate(Subset = "All"),
        dat_cand %>% filter(noSamePolicy) %>%
          mutate(Subset = "Distinct platforms")
      ) %>% group_by(Subset),
      fn_platformStat, "id", Nsim, cand1_only = T,
      needed = c("id", "weight", "matchNum", "candNum", "Type", "c_win", "diff_dem_type_u", "m_platformDist", "noSamePolicy", "Subset")
    )

  write_csv(tab_platformStat, "paper/results/tab_platformStat.csv")
}
if(!fresh) tab_platformStat = read_csv("paper/results/tab_platformStat.csv")

tab_platformStat = tab_platformStat %>%
  group_by() %>%
  mutate(
    Term = gsub("diff_dem_type_u", "(M1-M2)", term) %>% gsub("m_platformDist", "Platform polarization", .) %>% 
      gsub("diff_pRank", "(P1-P2)", .) %>% gsub(":", " x ", .),
    Term = factor(Term, unique(Term)),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")
  ) %>%
  arrange(Subset, Term) %>%
  mutate(
    Subset = ifelse(term == "(Intercept)", as.character(Subset), "")
  ) %>%
  select(
    Subset, Term, Estimate = estimate, SE = se, CI
  )

library(xtable)
write(
  print(xtable(
    tab_platformStat,
    caption = "Regression test to accompany Figure 6", label = "tab:heteroDefectPlatform", digits = 4
  ), include.rownames = FALSE, caption.placement = "top", size = "small"),
  "paper/figures/appendix_fig6_regression.txt"
)

###  Figure 7 calc: bootstrap "amce"    ==================================

if(fresh){
  tab_amce_boot = 
    block_boot(
      dat_full,
      (function(x){
        lm(c_win ~ diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Hispanic +
             diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer + diff_pro_Police_officer +
             diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner +
             diff_pro_Teacher + diff_respParty + 
             diff_p1_rate + diff_p2_rate +
             diff_dem_code_g_committee + diff_dem_code_g_officestructure + diff_dem_code_g_procedure +
             diff_dem_code_g_progEval + diff_dem_code_g_record + diff_dem_code_g_schedule + 
             diff_dem_code_u_banProtest +
             diff_dem_code_u_court + diff_dem_code_u_execRule + diff_dem_code_u_gerry2 + diff_dem_code_u_gerry10 +
             diff_dem_code_u_journalists + diff_dem_code_u_limitVote + diff_dem_code_v_affair + diff_dem_code_v_tax, 
           data =x, weights = weight) %>% tidy %>% .[,1:2]
      }), "id", Nsim, groups_in_output="term"
    )
  write_csv(tab_amce_boot, "paper/results/fig7_bootstrapped.csv")
}
if(!fresh) tab_amce_boot = read_csv("paper/results/fig7_bootstrapped.csv")

tab_amce_boot = tab_amce_boot %>% mutate(
  termOrig = term,
  term = gsub("^m_|dem_code_|diff_|sex_|race_|pro_", "", term)
) 

key_amce = 
  read_csv("data/key_amce.csv") %>% 
  select(-label, -label2) %>% 
  rename(term = Which, label = amce) %>%
  filter(!grepl("Ban Left|Ban Right", label))

tab_amce_boot = full_join(key_amce, tab_amce_boot)

tab_amce_boot$category = factor(tab_amce_boot$category, unique(tab_amce_boot$category))

tab_amce_boot = arrange(tab_amce_boot, category)

tab_amce_boot = mutate_at(tab_amce_boot, vars(estimate, se), function(x){x[is.na(x)]=0+runif(length(x),0,10^-10); x})

tab_amce_boot = tab_amce_boot %>% select(-term) %>% rename(term = label)

key_amce$label = factor(key_amce$label, key_amce$label)
key_amce = key_amce %>% arrange(!grepl("baseline", label), label)

tab_amce_boot$term = factor(tab_amce_boot$term, key_amce$label)


###  Figure 7 plot: marginal effects overall     ====================================================================================================================================

tab_full = 
  bind_rows(
    tab_amce_boot[tab_amce_boot$category %in% c("v", "u", "g"),],
    tab_amce_boot[!(tab_amce_boot$category %in% c("v", "u", "g")),]
  ) %>% filter(term!="(Intercept)")

tab_full = tab_full %>% mutate(
  term = factor(term, levels(tab_amce_boot$term)),
  category = factor(category, c("sex", "race", "pro", "p", "g", "u", "v")),
  estimate = ifelse(term %in% c("Different Party", "Economic Policy", "Social Policy"), -1*estimate, estimate),
  conf.low = ifelse(term %in% c("Different Party", "Economic Policy", "Social Policy"), -1*q2.5, q2.5),
  conf.high = ifelse(term %in% c("Different Party", "Economic Policy", "Social Policy"), -1*q97.5, q97.5)
) %>%
  arrange(category, term) %>%
  mutate(term = factor(term, term))

brackets = list(c("Gender / Race", "Male (baseline)", "Hispanic"),
                c("Occupation", "Business Executive (baseline)", "Teacher"),
                c("Party / Policy", "Same Party (baseline)", "Social Policy"),
                c("Neutral", "D+ Board of Elections (baseline)", "D+ Legislative Schedule"),
                c("Undemocratic", "D- Gerrymander by 2", "D- Ban Protests"),
                c(" Valence", "V- Extramarital Affairs", "V- Underpaid Taxes"))

g = ({dotwhisker::dwplot(tab_full,
             show_intercept = F,
             vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
             dot_args = list(size=2, color = "gray10"),
             whisker_args = list(color = "gray10", size = .25)) +
    xlab("Coefficient Estimate") +
    theme_minimal() +
    theme(legend.position="none",
          axis.title.x = element_text(size = 8),
          axis.text = element_text(size = 8))} %>%
      add_brackets(brackets, "plain"))
g
ggsave("paper/figures/fig7.pdf", g, "pdf", width = 6.75, height = 6.5)

###  Appendix plot: similar effects for candidates 1 & 2      ====================================================================================================================================

tab_both = 
  if(fresh){
    tab_both = 
      block_boot(
        dat_full,
        (function(x){
          lm(c_win ~ c_sex_Female + c_race_Asian + c_race_Black + c_race_Hispanic +
              c_pro_Farmer + c_pro_Lawyer + c_pro_Legislative_staffer + c_pro_Police_officer +
              c_pro_Served_in_the_army + c_pro_Served_in_the_navy + c_pro_Small_business_owner +
              c_pro_Teacher + c_respParty + 
              c_p1_rate + c_p2_rate +
              c_dem_code_g_committee + c_dem_code_g_officestructure + c_dem_code_g_procedure +
              c_dem_code_g_progEval + c_dem_code_g_record + c_dem_code_g_schedule + 
              c_dem_code_u_banProtest +
              c_dem_code_u_court + c_dem_code_u_execRule + c_dem_code_u_gerry2 + c_dem_code_u_gerry10 +
              c_dem_code_u_journalists + c_dem_code_u_limitVote + c_dem_code_v_affair + c_dem_code_v_tax + 
              o_sex_Female + o_race_Asian + o_race_Black + o_race_Hispanic +
              o_pro_Farmer + o_pro_Lawyer + o_pro_Legislative_staffer + o_pro_Police_officer +
              o_pro_Served_in_the_army + o_pro_Served_in_the_navy + o_pro_Small_business_owner +
              o_pro_Teacher + o_respParty + 
              o_p1_rate + o_p2_rate +
              o_dem_code_g_committee + o_dem_code_g_officestructure + o_dem_code_g_procedure +
              o_dem_code_g_progEval + o_dem_code_g_record + o_dem_code_g_schedule + 
              o_dem_code_u_banProtest +
              o_dem_code_u_court + o_dem_code_u_execRule + o_dem_code_u_gerry2 + o_dem_code_u_gerry10 +
              o_dem_code_u_journalists + o_dem_code_u_limitVote + o_dem_code_v_affair + o_dem_code_v_tax, 
            data = x, weights = weight) %>% tidy %>% .[,1:2]
        }), "id", Nsim, groups_in_output="term", cand1_only = T
      )
    write_csv(tab_both, "paper/results/fig7_bothCand.csv")
  }
if(!fresh) tab_both = read_csv("paper/results/fig7_bothCand.csv")

tab_both = tab_both %>% mutate(
  model = ifelse(grepl("c_", term), "Candidate 1", "Candidate 2"),
  termOrig = term,
  term = gsub("^c_|^o_|dem_code_|diff_|sex_|race_|pro_", "", term),
  estimate = mean,
  conf.low = q2.5,
  conf.high = q97.5,
  std.error = se
) %>%
  filter(term!="(Intercept)")

key_amce = 
  read_csv("data/key_amce.csv") %>% 
  select(-label, -label2) %>% 
  rename(term = Which, label = amce)  %>%
  filter(!grepl("Ban Left|Ban Right", label)) %>%
  (function(x){
    rbind(
      x %>% mutate(model = "Candidate 1"),
      x %>% mutate(model = "Candidate 2")
    )
  })

tab_both = full_join(key_amce, tab_both)

tab_both$category = factor(tab_both$category, unique(tab_both$category))

tab_both = arrange(tab_both, category)

tab_both = mutate_at(tab_both, vars(estimate, std.error), function(x){x[is.na(x)]=0+runif(length(x),0,10^-10); x})

tab_both = tab_both %>% select(-term) %>% rename(term = label)

key_amce$label = factor(key_amce$label, unique(key_amce$label))
key_amce = key_amce %>% arrange(!grepl("baseline", label), label)

tab_both$term = factor(tab_both$term, unique(key_amce$label))

tab_both = 
  bind_rows(
    tab_both[tab_both$category %in% c("v", "u", "g"),],
    tab_both[!(tab_both$category %in% c("v", "u", "g")),]
  )

tab_both = tab_both %>% mutate(
  term = factor(term, levels(tab_both$term)),
  
  category = factor(category, c("sex", "race", "pro", "p", "g", "u", "v")),
  estimate = ifelse(term %in% c("Different Party", "Economic Policy", "Social Policy"), -1*estimate, estimate),
  conf.low = ifelse(term %in% c("Different Party", "Economic Policy", "Social Policy"), -1*conf.low, conf.low),
  conf.high = ifelse(term %in% c("Different Party", "Economic Policy", "Social Policy"), -1*conf.high, conf.high),
  
  estimate = ifelse(model == "Candidate 2", -1*estimate, estimate),
  conf.low = ifelse(model == "Candidate 2", -1*conf.low, conf.low),
  conf.high = ifelse(model == "Candidate 2", -1*conf.high, conf.high)
  
) %>%
  arrange(category, term) %>%
  mutate(term = factor(term, unique(term)))

brackets = list(c("Gender / Race", "Male (baseline)", "Hispanic"),
                c("Occupation", "Business Executive (baseline)", "Teacher"),
                c("Party / Policy", "Same Party (baseline)", "Social Policy"),
                c("Neutral", "D+ Board of Elections (baseline)", "D+ Legislative Schedule"),
                c("Undemocratic", "D- Gerrymander by 2", "D- Ban Protests"),
                c(" Valence", "V- Extramarital Affairs", "V- Underpaid Taxes"))

g = ({dotwhisker::dwplot(tab_both,
                         show_intercept = F,
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                         dot_args = list(aes(shape = model, size = model), color = "gray10"),
                         whisker_args = list(color = "gray10", size = .25)) +
    xlab("Coefficient Estimate") +
    theme_minimal() +
    scale_shape_manual(values = c(2, 17)) +
    scale_size_manual(values = c(1.25, 1.5)) +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          plot.margin = margin(0,0,0,0),
          axis.title.x = element_text(size = 9))} %>%
      add_brackets(brackets, "plain"))
g
ggsave("paper/figures/appendix_fig7_separated.pdf", g, "pdf", width = 8, height = 9)


###  Appendix calc: clustered version     ====================================================================================================================================

tab_amce_reg = 
  lm_robust(c_win ~ diff_sex_Female + diff_race_Asian + diff_race_Black + diff_race_Hispanic +
              diff_pro_Farmer + diff_pro_Lawyer + diff_pro_Legislative_staffer + diff_pro_Police_officer +
              diff_pro_Served_in_the_army + diff_pro_Served_in_the_navy + diff_pro_Small_business_owner +
              diff_pro_Teacher + diff_respParty + 
              diff_p1_rate + diff_p2_rate +
              diff_dem_code_g_committee + diff_dem_code_g_officestructure + diff_dem_code_g_procedure +
              diff_dem_code_g_progEval + diff_dem_code_g_record + diff_dem_code_g_schedule + 
              diff_dem_code_u_banProtest +
              diff_dem_code_u_court + diff_dem_code_u_execRule + diff_dem_code_u_gerry2 + diff_dem_code_u_gerry10 +
              diff_dem_code_u_journalists + diff_dem_code_u_limitVote + diff_dem_code_v_affair + diff_dem_code_v_tax, 
            data = dat_full, weights = weight, clusters = id) %>% tidy

# note: removing the "%>% filter(candNum==1)" offers a good illustration
#       of how little the estimates depend on whether the data have one 
#       observation per candidate or one observation per choice

""


###  Appendix table: clustered vs block bootstrapped SE      ====================================================================================================================================

tab_SEcompare = 
  left_join(
    tab_full      %>% group_by() %>% select(Term=term, term=termOrig, Estimate=estimate, `Bootstrap SE`=se),
    tab_amce_reg  %>% group_by() %>% select(`Clustered SE`=std.error, term) %>% filter(term != "(Intercept)")
  ) %>% 
  group_by() %>% select(-term)

tab_SEcompare = tab_SEcompare %>%
  mutate(Term = factor(Term, tab_full$term)) %>%
  arrange(Term)

tab_SEcompare = within(tab_SEcompare, {
  Estimate[grepl("baseline", Term)] = NA_real_
  `Clustered SE`[grepl("baseline", Term)] = NA_real_
  `Bootstrap SE`[grepl("baseline", Term)] = NA_real_
})

mean(tab_SEcompare$`Bootstrap SE`[complete.cases(tab_SEcompare)]) / mean(tab_SEcompare$`Clustered SE`[complete.cases(tab_SEcompare)])
cor(tab_SEcompare$`Bootstrap SE`[complete.cases(tab_SEcompare)], tab_SEcompare$`Clustered SE`[complete.cases(tab_SEcompare)])

tab_SEcompare = 
  tab_SEcompare %>%
  mutate(Term = factor(Term, tab_full$term)) %>%
  mutate_at(vars(-Term), function(x) ifelse(round(x, 9)==0, NA_real_, x)) %>%
  arrange(Term)
  

library(xtable)
write(
  print(
    xtable(tab_SEcompare, 
           digits = 4, caption = "Average marginal effect, numerical results", label = "tab:AMCE"), 
    include.rownames = F, caption.placement = "top", size="\\fontsize{10pt}{11pt}\\selectfont"
  ),
  "paper/figures/appendix_fig7_numerical.txt"
)

###  Figure 8 calc: marginal means by party      ====================================================================================================================================

#  to be bootstrapped
dat_full %>% 
  filter(candNum == 1,
         !grepl("g_", c_dem_code)) %>%
  (function(x)rbind(x %>% mutate(model = "Overall"), x %>% filter(pid_lean!="I") %>% mutate(model = pid_Lean))) %>% 
  group_by(model, c_dem_code) %>% fn_binPref

#  bootstrap
if(fresh){
  tab_hetero = block_boot(
    dat_full %>% 
      filter(candNum == 1, !grepl("g_", c_dem_code)) %>%
      (function(x)rbind(x %>% mutate(model = "Overall"), x %>% filter(pid_lean!="I") %>% mutate(model = pid_Lean))) %>% 
      group_by(model, c_dem_code),
    fn_binPref, "id", Nsim, cand1_only = F
  )
  write_csv(tab_hetero, "paper/results/fig8_results.csv")
}
if(!fresh) tab_hetero = read_csv("paper/results/fig8_results.csv")
  
#  prepare for plotting
tab_hetero = tab_hetero %>% 
  as.data.frame %>%
  mutate(
    Category =
      case_when(
        grepl("^g_", c_dem_code) ~ "Neutral",
        grepl("^u_", c_dem_code) ~ "Undemocratic",
        grepl("^v_", c_dem_code) ~ "Negative Valence"
      ) %>%
      factor(
        c("Undemocratic", "Negative Valence")
      )
  )

key_amce = read_csv("data/key_amce.csv") %>% 
  select(-label, -label2, -category) %>% rename(Attribute = Which, label = marginalmean) %>%
  filter(!grepl("Ban Left|Ban Right", label))

tab_hetero = left_join(
  tab_hetero %>% rename(Attribute = c_dem_code),
  key_amce
)

tab_hetero = tab_hetero %>%
  group_by() %>%
  mutate(
    label = factor(label, key_amce$label),
    model = factor(model, c("Overall", "Democrat", "Independent", "Republican")[4:1]),
    conf.low = q2.5,
    conf.high = q97.5,
    std.error = se
  ) %>%
  arrange(
    model, Category, label
  )

tab_heteroPlot = tab_hetero %>% filter(model!="Independent")

###  Figure 8 plot: marginal means by party      ====================================================================================================================================

brackets <- list(
  c("Undemocratic", "D- Gerrymander by 2", "D- Ban Protests"),
  c("Valence", "V- Extramarital Affairs", "V- Underpaid Taxes"))

g = {dotwhisker::dwplot(tab_heteroPlot %>% 
              #filter(Category %in% c("Neutral", "Undemocratic", "Negative Valence"), !is.na(model), term!="(Intercept)") %>%
              mutate(term = label),
            show_intercept = F,
            vline = geom_vline(xintercept = 0.5, colour = "grey60", linetype = 2),
            dot_args = list(aes(shape=model, colour=model), size=1.5), 
            whisker_args = list(size = .25)) +
    xlab("Estimate") +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 9)
    ) +
    guides(color = guide_legend(reverse = T), shape = guide_legend(reverse = T)) +
    scale_colour_manual(values = c(theRed, theBlue, "gray10")) +
    scale_shape_manual(values = c(17, 15, 16))} %>%
  add_brackets(brackets, "plain")
g
ggsave("paper/figures/fig8.pdf", g, "pdf", width = 7.5, height = 4.25)

###  Appendix table: marginal means by party     ====================================================================================================================================

print(
  xtable(
    tab_hetero %>% 
      mutate(CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")")) %>%
      select(Category, Position=label, Party=model, Estimate=estimate, `Bootstrap SE`=std.error, CI) %>%
      mutate(Party = factor(Party, c("Overall", "Democrat", "Independent", "Republican"))) %>%
      arrange(Position, Party), 
    caption = "Marginal means by party", digits = 3
  ), caption.placement = "top", include.rownames = F, size="\\fontsize{10pt}{11pt}\\selectfont"
) %>%
  write("paper/figures/appendix_fig8_numerical.txt")

###  Appendix plot: distribution of policy rating variables      ====================================================================================================================================

d_allMeasures$pid_fold = abs(d_allMeasures$pid7)

ks_allCombos = function(x, pid_fold){
  d_ks = data.frame(g1 = c(0, 0, 0, 1, 1, 2), g2 = c(1, 2, 3, 2, 3, 3), Statistic = NA, p = NA)
  for(i in 1:nrow(d_ks)){
    test = ks.test(x[pid_fold==d_ks$g1[i]], x[pid_fold==d_ks$g2[i]])
    d_ks$Statistic[i] = test$statistic
    d_ks$p[i] = test$p.value
  }
  d_ks
}

tab_ks = d_allMeasures %>%
  mutate(value = value_diff) %>%
  filter(!is.na(value), !is.na(Measure), !is.na(Transformation)) %>%
  group_by(Measure, Transformation) %>%
  do(ks_allCombos(.$value, .$pid_fold))

tab_ks = tab_ks %>% mutate(p = trail_zero(p, 3)) %>%
  mutate(g1 = as.character(g1), g2 = as.character(g2)) %>%
  rename(`Group 1` = g1, `Group 2` = g2)

library(xtable)
tab_ks = print(xtable(tab_ks), include.rownames = F)
tab_ks = strsplit(tab_ks, "\n")[[1]]
tab_ks = tab_ks[-c(1:3, length(tab_ks))]
tab_ks = paste(tab_ks, collapse = "\n")

write(tab_ks, "paper/figures/appendix_KStest.txt")

g =
  d_allMeasures %>%
  mutate(`Party Strength` = recode_factor(pid_fold, `0`="0. Independent", `1`="1. Lean", `2`="2. Regular", `3`="3. Strong")) %>%
  ggplot(aes(x = value_diff, color = `Party Strength`, lty = `Party Strength`)) +
  scale_color_manual(values = paste0("firebrick", 1:4)) +
  geom_density(size = 0.25) +
  theme_bw() +
  facet_grid(Transformation ~ Measure) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "gray92")) +
  labs(x = "Value", y = "Density")

ggsave("paper/figures/appendix_densityFull.pdf", g, "pdf", width = 10, height = 7)
ggsave("paper/figures/appendix_densityZoom.pdf", g + coord_cartesian(ylim = c(0, 1.2)), "pdf", width = 10, height = 7)
#g + coord_cartesian(ylim = c(0, 1.2))


# NEXT --- make a function to do all the KS tests
#    spit it out into a DF that matches the facets
#    put it on the plots



###  Appendix plot: logit vs. actual data      ====================================================================================================================================

tab_logit = 
  d_allMeasures %>%
  group_by(Measure, Transformation) %>%
  mutate(value = value_diff) %>%
  filter(!is.na(value)) %>%
  do(
    glm(c_win ~ Type + value, family = binomial(link = "logit"), data = ., weights = weight) %>% 
      tidy %>% select(-std.error, -statistic, -p.value)
  )

tab_logitPred = data.frame(value = seq(-1, 1, .01))
tab_logitPred = foreach(i = 1:nrow(measures), .combine = rbind) %dopar% {
  coefs = tab_logit %>% filter(Measure==measures$Measure[i],
                               Transformation==measures$Transformation[i])
  coefs = coefs$estimate
  out = tab_logitPred
  out$Measure = measures$Measure[i]
  out$Transformation = measures$Transformation[i]
  temp = exp(coefs[1] + coefs[6]*out$value)
  out$`D+ vs. D+` = temp / (1 + temp)
  temp = exp(coefs[1] + coefs[2] +  coefs[6]*out$value)
  out$`D- vs. D+` = temp / (1 + temp)
  out
}
tab_logitPred = gather(tab_logitPred, Type, estimate, `D+ vs. D+`, `D- vs. D+`) %>% mutate(Type = factor(Type, c("D+ vs. D+", "D- vs. D+")))
tab_logitPred = tab_logitPred %>% mutate(q2.5 = 0, q97.5 = 0)

g = plot_voteShare(
  tab_meanDIM %>% 
    filter(!grepl("-$|V-", Type), Stat=="Mean")) +
  geom_path(lty = 1) +
  geom_path(
    data = tab_logitPred %>% filter(!grepl("-$|V-", Type)),
    lty = 2
  ) +
  facet_grid(Transformation ~ Measure, switch = "y") +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 7),
        legend.position = "bottom")
g
ggsave("paper/figures/appendix_logitFit.pdf", g, "pdf", width = 8.5, height = 6)


""

###  Appendix plot: heterogeneous effects      ====================================================================================================================================

# new data.frame
dat_full$Education = recode_factor(substr(dat_full$education, 1, 3), Did="<HS", Hig="HS", "Som"="Some college", Ass="Assos.", 
                                    Bac="BA/BS", Gra="Grad", .default = NA_character_)
dat_full$race[grepl("American Indian", dat_full$race)] = "Other"

dat_hetero = dat_full %>%
  mutate(dem_better = round(dem_better, 1) %>% recode(`0`="Strongly disagree", `0.3`="Disagree", `0.7`="Agree", `1`="Strongly agree")) %>%
  select(
    id, matchNum, candNum, weight, c_win, diff_dem_type_u, diff_dem_type_v, Type, auth_total, dem_better, voteduty, ideo, Education, race, hispanic, hhi_cut, age_cut, knowl_anes_total, trump, gender
  )

# function for bootstrapping
fn_hetero = function(x){
  melt(x, c("id", "weight", "c_win", "matchNum", "candNum", "diff_dem_type_u", "diff_dem_type_v", "Type")) %>%
    filter(!is.na(value)) %>%
    group_by(variable, value) %>%
    do(
      tidy(lm(c_win ~ diff_dem_type_u + diff_dem_type_v, data = ., weights = weight, clusters = id))[,1:2]
    )
}  

# to be bootstrapped
fn_hetero(dat_hetero)

# bootstrap
if(fresh){
  tab_hetero = block_boot(
    dat_hetero, fn_hetero, "id", Nsim, cand1_only = T
  )
  write_csv(tab_hetero, "paper/results/tab_hetero.csv")
}
if(!fresh) tab_hetero = read_csv("paper/results/tab_hetero.csv")

# prepare data for plotting
tab_hetero = tab_hetero %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = recode(variable, auth_total = "Authoritarianism", dem_better = "Democracy better", voteduty = "Voting a choice/duty", knowl_anes_total = "Knowledge",
                      ideo = "Ideology", race = "Race", hispanic = "Hispanic", hhi_cut = "Income", age_cut="Age", gender="Gender", trump="Trump approval"),
    Type = recode_factor(term, diff_dem_type_u = "D- vs. D+", diff_dem_type_v = "V- vs. D+")
  )
tab_hetero$value = gsub("201|96", "Inf", tab_hetero$value)
tab_hetero$num   = as.numeric(gsub("\\[|\\(|,.+", "", tab_hetero$value))
temp = temp2 = tab_hetero$value[is.na(tab_hetero$num)]
temp = 2 + 1*grepl("Strongly|Extremely|Yes", temp) - grepl("Slightly|Prefer not", temp) - 2*(temp=="Moderate")
temp = temp*(1-2*grepl("liberal|disagree", ignore.case=T, temp2))
temp = temp - grepl("Somewhat disapprove", temp2) - 10*grepl("Strongly disapprove", temp2) + grepl("Somewhat approve", temp2) + 2*grepl("Strongly approve", temp2)
temp = temp - 4*grepl("^-", temp)
tab_hetero$num[is.na(tab_hetero$num)] = temp
tab_hetero$num[tab_hetero$variable == "Education"] = recode(tab_hetero$value[tab_hetero$variable == "Education"], `<HS`=1, `HS`=2, `Some college`=3, `Assos.`=4, `BA/BS`=5, `Grad`=6)

tab_hetero = tab_hetero %>%
  filter(Type %in% c("D- vs. D+", "V- vs. D+")) %>%
  group_by() %>% 
  arrange(as.numeric(variable != "voteduty"), desc(variable), num, as.numeric(value), value)

tab_hetero = tab_hetero %>%
  mutate(Value = factor(value, tab_hetero %>% select(value) %>% distinct %>% .$value)) %>%
  arrange(as.character(Variable)) %>%
  mutate(Variable = factor(Variable, unique(Variable)),
         Type = recode_factor(Type, Undemocratic="Undem.", `Negative Valence` = "Negative\nValence"))

rm(temp, temp2, dat_hetero)

# plot
g = tab_hetero %>%
  filter(value!="Prefer not to answer", !grepl("-$", Type)) %>%
  ggplot(aes(y = estimate, ymin = q2.5, ymax = q97.5, x = Value, shape = Type, color = Type)) + 
  geom_hline(aes(yintercept = 0), color = "white") +
  geom_hline(aes(yintercept = 0), lty = 2, size = .75, color = "gray80") +
  geom_point(position = position_dodge(width = .5), size = 1.5) +
  geom_errorbar(position = position_dodge(width = .5), width = 0, size = .25) +
  
  scale_color_manual(values = c("firebrick3", "gray15")) +
  scale_shape_manual(values = c(19, 17)) +
  
  theme_allPlots() +
  theme(
    axis.text.x = element_text(hjust = .85, angle = 35, size = 8),
    panel.grid.major.x = element_blank(),
    legend.position = c(.91, -.11),
    legend.title = element_blank(),
    panel.spacing.y = unit(0, "cm")
  ) +
  coord_cartesian(ylim = c(-.3, 0)) +
  facet_wrap(~Variable, scales = "free_x", ncol=4) +
  labs(x = "Value", y = "Estimate")
#g
ggsave("paper/figures/hetero_trt.pdf", g, width = 6.5, height = 7)

###  Appendix plot: democracy questions      ====================================================================================================================================

w1_dem = w1_dem %>% select(-dem_treat_banProtest)

#  Get labels from codebook & transform
library(readxl)
dem_wording = read_csv("data/key_howDemocratic.csv")
dem_wording = dem_wording %>% filter(grepl("^dem_", variable)) %>% select(-scale)
dem_wording = dem_wording %>%
  mutate(
    description = gsub(".+\\] |\\n.+", "", description),
    description = gsub(" after legislators from ", " after legislators from\n", description),
    description = gsub("order to secure ", "order to secure\n", description),
    description = gsub("criticize the president ", "criticize the president\n", description),
    description = gsub("stations in areas that ", "stations in areas that\n", description),
    description = gsub("attract voters", "attract\nvoters", description),
    description = gsub("voting machines", "voting\nmachines", description),
    description = gsub("out to vote in l", "out to vote\nin l", description),
    description = gsub("proportional to maj", "proportional to\nmaj", description),
    description = gsub("sitting presidents ", "sitting presidents\n", description),
    description = gsub("government resources ", "government resources\n", description),
    description = gsub("bother with ", "bother with\n", description),
    description = gsub("governed democratically", "governed\ndemocratically", description),
    description = gsub("any other form", "any other\nform", description),
    description = gsub("democracy works in", "democracy works\nin", description),
    description = gsub("incompetent. (D-)", "incompetent. (D-)               ", description)
  )

#  Function to create figures
plot_dem = function(nameString, scaleOpt = "fixed"){
  
  dem_dat = w1_dem[,grepl(paste0("^id$|", nameString), names(w1_dem))] %>% mutate_at(vars(-id), as.numeric)
  
  tab_ess =
    dem_dat %>%
    melt("id") %>%
    group_by(variable) %>%
    filter(!is.na(value)) %>%
    summarize(estimate = mean(value, na.rm=T),
              se       = sd(value, na.rm=T)/sqrt(sum(!is.na(value)))) %>%
    left_join(dem_wording) %>%
    arrange(estimate) %>%
    mutate(description = factor(description, description),
           lab = paste0(round(estimate, 2), " (", round(se, 2), ")"),
           battery = gsub("dem_", "", variable) %>% gsub("_.+", "", .)
    )
  
  plot_ess_main =
    rbind(
      tab_ess %>% mutate(dodgeVar = "a"),
      tab_ess %>% mutate(dodgeVar = "b")
    ) %>%
    ggplot(aes(y = description, x = estimate, xmin = estimate - 2*se, xmax = estimate + 2*se, label = lab, color = dodgeVar)) +
    geom_point(position = position_dodgev(height = .9), size=1) +
    scale_color_manual(values = c("black", "transparent")) +
    geom_text(vjust = -.75, size = 3, hjust = .5, position = position_dodgev(height = .9)) +
    geom_errorbarh(height=0, position = position_dodgev(height = .9), size=.25) +
    theme_bw() +
    theme(axis.text.y = element_text(hjust=0, color="black"),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(size=9),
          legend.position = "none") +
    xlim(0, 1) +
    labs(x = "Mean rating of statement")
  
  plot_ess_main
  
  plot_ess_density =
    dem_dat %>%
    melt("id") %>%
    filter(!is.na(value)) %>%
    left_join(dem_wording) %>%
    mutate(description = factor(description, tab_ess$description[nrow(tab_ess):1])) %>%
    group_by(description, value) %>%
    summarize(N = n()) %>%
    group_by(description) %>%
    mutate(pct = N/sum(N)) %>%
    ggplot(aes(x = value, y = pct)) +
    geom_histogram(color = "black", fill = "gray95", breaks = seq(0, 1, .1), size = .25, stat = "identity") +
    theme_minimal() +
    theme(
      strip.text = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(.3,0,.3,0), "cm")
    ) +
    facet_wrap(~description, ncol=1, strip.position = "left", scales = scaleOpt) +
    labs(x = " ")
  
  g =
    grid.arrange(plot_ess_main, plot_ess_density, widths = c(.85, .1))
  
  g
}

#  Appendix figures
g = plot_dem("dem_ess")
ggsave("paper/figures/dem_essential.pdf", g, "pdf", width = 7, height = 3.2)

g = plot_dem("dem_treat")
ggsave("paper/figures/dem_treat.pdf", g, "pdf", width = 7, height = 3.8)

g = plot_dem("dem_world")
ggsave("paper/figures/dem_world.pdf", g, "pdf", width = 7, height = 4.2)

g = plot_dem("dem_sys")
ggsave("paper/figures/dem_sys.pdf", g, "pdf", width = 7, height = 2)

g = plot_dem("dem_better|dem_satisfied|dem_gerry|dem_UStoday", "free_x")
ggsave("paper/figures/dem_other.pdf", g, "pdf", width = 7, height = 2.5)

###  Appendix: abstention supplemental analysis ------------

fn_turnCATE = function(x){
  tab1 = tidy(lm(c_win ~ diff_dem_type_u, data = x,                       weights = weight))[2,1:2] %>% mutate(term="Overall")
  tab2 = tidy(lm(c_win ~ diff_dem_type_u, data = x %>% filter(c_vote==1), weights = weight))[2,1:2] %>% mutate(term="Voters only")
  rbind(
    tab1, tab2, c(term="Difference", estimate=tab1$estimate-tab2$estimate)
  )[,1:2] #%>% group_by(term)
}

fn_turnCATE(dat_cand)

if(fresh){
  tab_turnCATE = block_boot(dat_cand, fn_turnCATE, "id", Nsim, cand1_only = T)
  write_csv(tab_turnCATE, "paper/results/tab_turnCATE.csv")
}
if(!fresh){
  tab_turnCATE = read_csv("paper/results/tab_turnCATE.csv")
}

out = tab_turnCATE %>%
  group_by() %>%
  mutate(
    #Variable = recode(term, Overall="Preference only", Voters="Reflects abstention"),
    CI = paste0("(", trail_zero(q2.5, 3), ", ", trail_zero(q97.5, 3), ")"),
    Estimate = trail_zero(estimate, 3),
    SE = trail_zero(se, 3)) %>%
  select(` `=term, Estimate, SE, CI)

print(xtable(out, caption = "Treatment Effect by Outcome Variable"), include.rownames = F, caption.placement = "top") %>%
  write(., "paper/figures/appendix_turnout.txt")

###  Figure 1: illustrate predictions         ==================================

keep = ls()  # The code for this figure generates a lot of stray objects. If you run the whole chunk it'll drop every object that's not already in the environment.

inv_logit <- function (x) 1/(1+exp(-x))
pr_prediction <- function(x) inv_logit(x)

alpha <- 8
delta <- 2
x_A <- -.2
x_B <- .2
x_S <- (x_A + x_B)/2 + delta/(2*alpha*(x_B-x_A))
x_S
ate_max <- (x_A + x_B)/2 + delta/(4*alpha*(x_B-x_A))
ate_max
x_min <- -1
x_max <- 1

# RESCALE (BASED ON THE LOGIT SIMULATION)
rescale <- 6.4
x_A <- rescale*x_A
x_B <- rescale*x_B
x_S <- rescale*x_S
ate_max <- rescale*ate_max
x_min <- rescale*x_min
x_max <- rescale*x_max
x <- seq (x_min, x_max, .01)

y_control <- pr_prediction(x)
y_treatment <- pr_prediction(x - delta)
y_diff <- y_control - y_treatment

# PR VOTE 1: A PLOT THAT HAS VOTERS' IDEAL POINTS ON THE X AXIS
df_predict_control <- data.frame(x, y=y_control, dem=0)
df_predict_treatment <- data.frame(x, y=y_treatment, dem=1)
df_predict <- rbind(df_predict_control, df_predict_treatment)
df_predict <- df_predict %>% mutate(dem=as.factor(dem))

df = tibble(
  x = x,
  `D+ vs. D+` = y_control,
  `D- vs. D+` = y_treatment,
  CATE        = y_control - y_treatment
)

df = df %>%
  tidyr::gather(variable, value, -x) %>%
  mutate(
    Facet = ifelse(variable == "CATE", "CATE", "Mean") %>% factor(c("Mean", "CATE")),
    facet = ifelse(variable == "CATE", "Fraction defecting when candidate 1 is D-", "Fraction voting for candidate 1") %>% factor(unique(.)),
    variable_orig = variable,
    variable = ifelse(variable == "CATE", "D- vs. D+", variable) %>% factor(c("D+ vs. D+", "D- vs. D+"))
  )

fn_fig1 = function(theFacet, theBreaks = (0:2)/2){
  df %>%
    filter(Facet == theFacet) %>%
    ggplot(aes(x=x, y=value, col=variable)) +
    geom_hline(aes(alpha = Facet, yintercept=ifelse(Facet == "Mean", 0.5, NA_real_)), size = .25, show.legend = F) +  
    geom_vline(xintercept=0, size=.25, col="gray") +  
    scale_shape_manual(name  ="", values=c(1, 2), labels=c("D+ vs. D+", "D- vs. D+")) +
    scale_linetype_manual(name  ="", values=c("solid", "dashed"), labels=c("D+ vs. D+", "D- vs. D+")) +
    scale_color_manual(name  ="", values=c("black", "red"), labels=c("D+ vs. D+", "D- vs. D+")) +
    scale_alpha_manual(values = 1:0) +
    theme_allPlots() +
    theme(
      legend.text = element_text(size = 8),
      legend.spacing.x = unit(.125, "cm"),
      legend.margin = margin(0,0,0,0),
      axis.text = element_text(size = 8, color = "gray5"),
      plot.title = element_blank(),
      panel.border = element_rect(color = "gray5", fill = "transparent", size = .25),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.spacing = unit(.4, "cm"),
      axis.ticks = element_line(size = .25),
      axis.text.x = element_text(size = 7, lineheight = .75, margin = margin(2,0,-10,0)),
      axis.ticks = element_line(size=.25, color = "gray5"),
      axis.title = element_blank()
    ) +
    scale_y_continuous(limits = c(0, max(theBreaks)), expand = c(0, 0), breaks = theBreaks, labels = gsub("0.", ".", theBreaks)) +
    labs(
      y = "Fraction voting for candidate 1"
    ) +
    facet_wrap(~facet)
}

g1 = fn_fig1("Mean", (0:2)/2) + 
  geom_line(aes(col=variable, group=variable, linetype = variable, y = value),  size=.5) + 
  scale_x_continuous(breaks=c(-x_S, x_A, 0, x_B, x_S), 
                     labels=c(expression(x[S]^2), expression(x['2k']), expression(frac(x['1k']+x['2k'],2)), expression(x['1k']), expression(x[S]^1))) + 
  theme(legend.position = c(.82, .175))
  
g2 = fn_fig1("CATE", theBreaks = (0:2)/4) +
  geom_line(aes(col=variable, group=variable, y = value),  size=.5, lty = 2, color = theRed) +
  scale_x_continuous(breaks=c(0, x_S), 
                     labels=c(expression(frac(x['1k']+x['2k'],2)), expression(x[S]^1))) + 
  theme(legend.position = "none") +
  geom_text(
    aes(x=ate_max, y=0.025, label="x[Delta]^1"), parse=T, size = 2.3, color = "gray5"
  ) +
  geom_segment(
    aes(x=ate_max, xend=ate_max, y=0, yend=.008), size = .2
  )

g = grid.arrange(g1, g2, nrow = 1, 
                 bottom = textGrob(expression(paste("Voter ideal policies ", x['ik'])), gp = gpar(fontsize = 9)))

ggsave("paper/figures/fig1.pdf", g, "pdf", width = 6.5, height = 3.25)
ggsave("paper/figures/fig1.jpeg", g, "jpeg", width = 6.5, height = 3.25, dpi=1000)

rm(list = ls()[which(!(ls() %in% keep))])

