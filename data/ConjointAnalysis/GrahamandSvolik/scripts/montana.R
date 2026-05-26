rm(list=ls())
setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Dropbox/GrahamSvolik/"))
library(tidyverse)

theRed = "red"
theBlue = "#263c73"

# Load and process data --------------------

mt = read_csv("montana/montana.csv")

mt = mt %>% mutate(pctR = voteR / (voteD + voteR))
mt = mt %>% group_by(county, precinct) %>% 
  mutate(
    pctR_14 = sum(voteR[year==2014]) / (sum(voteR[year==2014]) + sum(voteD[year==2014])),
    pctR_16 = sum(voteR[year==2016]) / (sum(voteR[year==2016]) + sum(voteD[year==2016])),
    `2017` = as.numeric(year==2017),
    `2016` = as.numeric(year==2016),
    electDay = as.numeric(method=="polls"),
    `Voting method` = recode(method, absent="Absentee", polls="Election day"),
    precinct_co = paste0(county, precinct)
    )%>%
  group_by() %>%
  mutate(
    # cluster ID #  --- lm_robust() requires a numeric vector
    clust = as.numeric(factor(paste(county, precinct)))
  )

mt = mt %>% left_join(read_csv("montana/mt_covariates.csv"))

# Parallel trend variables -------------

    # ten:  absolute value of 14-16 diff-in-diff > 10    - OR - Lake County
    # five: absolute value of 14-16 diff in diff (5, 10] - OR - Lake County
    # problem: combined variable

mt = left_join(mt, 
               mt %>%
                 filter(county!="Lake", year!=2017) %>%
                 group_by(county, precinct) %>%
                 summarize(
                   did1614 = pctR[year==2016 & method=="polls"] - pctR[year==2014 & method=="polls"] - (pctR[year==2016 & method=="absent"] - pctR[year==2014 & method=="absent"])
                 ) %>%
                 mutate(
                   ten = as.numeric(abs(did1614)>.10),
                   five = as.numeric(abs(did1614)>.05)
                 )
)
mt$ten[mt$county=="Lake"] = 1
mt$five[mt$county=="Lake"] = 1
mt$problem = ifelse(mt$county=="Lake", "Lake County", recode(paste0(mt$ten, mt$five), `11`=">10%", `01`=">5%", `00`="<5%"))

# PLOT: Parallel trends ------------------

precinct_plot =
mt %>%
  mutate(year = year - 2000) %>%
  group_by(method, precinct = paste(county, precinct), year, problem) %>%
  summarize(#pctR = mean(pctR)
    pctR = sum(voteR) / (sum(voteD) + sum(voteR)), 
    n = sum(voteR) + sum(voteD)
  ) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = pctR, color = method, shape = method, group = paste(precinct, method))) +
  geom_point() +
  scale_shape_manual(values = c(19, 17)) +
  geom_path() +
  facet_wrap(~precinct, ncol = 11) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9, color = "gray5"),
        axis.text.x = element_text(size = 9, color = "gray5"),
        axis.title = element_text(size = 9, color = "gray5"),
        axis.title.x = element_text(margin = margin(10,0,0,0)),
        axis.title.y = element_text(margin = margin(0,8,0,0)),
        legend.position = "bottom",
        legend.title = element_blank()) +
  geom_text(aes(y = .1, x = year - .1*(year - 15.5), label = ifelse(method=="absent", n, "")), size = 3, vjust = .5, show.legend = F) + 
  geom_text(aes(y = .1, x = year - .1*(year - 15.5), label = ifelse(method!="absent", n, "")), size = 3, vjust = -.7, show.legend = F) + 
  guides(fill = guide_legend(override.aes = list(alpha=.25))) +
  scale_color_manual(values = c("gray50", "black")) +
  scale_x_continuous(breaks = c(14, 16, 17), limits = c(13.8, 17.2)) +
  labs(y = "Republican vote share", x = "Year")

#precinct_plot

# ggsave("presentations/Oct2018/mt_parallel.pdf", precinct_plot, "pdf", width = 14, height = 9.5)

precinct_plot_prob = precinct_plot +
  geom_rect(aes(xmin = -Inf, ymin = -Inf, xmax=Inf, ymax=Inf, fill = problem), alpha = .05, color = "black") + 
  scale_fill_manual(values = c("transparent", "firebrick1", "firebrick4", "gray40"))

#precinct_plot_prob

ggsave("paper/figures/mt_parallel.pdf", precinct_plot_prob, "pdf", width = 14, height = 10)

# SUMMARY TABLE: diff-in-diff + voter chars ------------------

tab_mt = mt %>%
  group_by(county, precinct, method, `Voting method`) %>%
  summarize(
    diff_1716 = pctR[year==2017] - pctR[year==2016],
    R_16      = sum(voteR[year==2016]),
    D_16      = sum(voteD[year==2016])
  ) %>%
  group_by(
    county, precinct
  ) %>%
  mutate(pctR_16 = sum(R_16) / (sum(R_16) + sum(D_16)),
         did = diff_1716[method=="polls"] - diff_1716[method=="absent"],
         Change = ifelse(did<0, "Negative diff-in-diff", "Positive diff-in-diff"))

tab_voterfile = read_csv("montana/mt_voterChar_diffs.csv")

tab_mt_dId = 
  left_join(tab_mt %>% group_by() %>% select(county, precinct, did, pctR_16), 
            tab_voterfile %>% select(county, precinct, `Birth year D-in-D` = dId_birth, `City limits D-in-D` = dId_city, `Percent absentee D-in-D` = dId_pct)
            ) %>%
  left_join(
    mt %>% group_by(county, precinct) %>% summarize(`Mean birth year` = mean(birthyr),
                                                    `Mean city limits` = mean(city),
                                                    `Mean percent absentee` = mean(pct[method=="absent"]))
  )

library(reshape2)
g = tab_mt_dId %>% select(-pctR_16) %>%
  melt(c("county", "precinct", "did")) %>%
  ggplot(
    aes(x = did, y = value)
  ) +
  geom_point(size = 1.5, alpha = .3) +
  #geom_smooth(method = "lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "gray92")) +
  facet_wrap(~variable, scale = "free_y") +
  labs(x = "Diff-in-diff")
g
ggsave("paper/figures/mt_covar_dId.pdf", g, "pdf", width = 6.5, height = 4)

g = tab_mt_dId %>% select(-did) %>%
  melt(c("county", "precinct", "pctR_16")) %>%
  ggplot(
    aes(x = pctR_16, y = value)
  ) +
  geom_point(size = 1.5, alpha = .3) +
  #geom_smooth(method = "lm") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "gray92")) +
  facet_wrap(~variable, scale = "free_y") +
  labs(x = "Percent Republican in 2016")
g
ggsave("paper/figures/mt_covar_pctR16.pdf", g, "pdf", width = 6.5, height = 4)

tab_mt %>%
  ggplot(
    aes(x = diff_1716, y = dId_pct)
  ) +
  geom_point() +
  geom_smooth() +
  theme_bw()

# PLOT: diff-in-diff visual ---------------
  
groupmeans_MT_did = tab_mt_dId %>%
  mutate(pctR_16 = cut(pctR_16, c(0,(4:8),10)/10 )) %>%
  group_by(bin = pctR_16) %>%
  summarize(did = mean(did)) %>%
  mutate(
    label = paste0(round(100*did, 1), "%"),
    pctR_16 = c(.325, .45, .55, .65, .75, .875),
    county = "blank",
    precinct = "blank",
    `Voting method` = "Absentee",
    `Difference in differences` = ifelse(did>0, "Positive", "Negative")
  ) %>%
  mutate(
    label = ifelse(did<0, label, paste0("+", label)),
    label = ifelse(!grepl("15", label), label, gsub("15", "15.0", label))
  )

plot_mt = tab_mt %>%
  mutate(`Difference in differences` = gsub(" .+", "", Change)) %>%
  ggplot(aes(pctR_16, diff_1716, shape = `Voting method`, color = `Difference in differences`, group = paste(county, precinct), size = `Voting method`)) +
  geom_hline(yintercept = 0, size = .25) +
  
  scale_shape_manual(values = c(1, 18)) +
  scale_size_manual(values = c(2.5, 3)) +
  geom_line(arrow = arrow(length=unit(0.25,"cm")), alpha = .8, size = .5) +
  theme_bw() +
  theme(legend.position = c(.213, .76),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.spacing.y = unit(.1, "cm"),
        legend.key.height = unit(.45, "cm"),
        axis.ticks = element_line(size = .25),
        axis.title = element_text(size = 8.5),
        axis.title.x = element_text(margin = margin(8,0,0,0)),
        #axis.ticks.length = unit(.1, "cm"),
        legend.title = element_text(size = 8.5),
        legend.text = element_text(size = 8.5),
        axis.text.x = element_text(hjust = .7),
        axis.text = element_text(size = 8.5),
        panel.border = element_blank(),
        text = element_text(size = 8.5)) +
  guides(
    color = guide_legend(order = 2)
  ) +
  labs(x = "Republican two-party vote share for the entire precinct, 2016",
       y = "2017 - 2016"
  ) +
  scale_x_continuous(limits = c(.25, .95), breaks = (3:9)/10, labels = gsub("0\\.", ".", (3:9)/10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-.25, .255), expand = expand_scale(add = c(0, .02))) +
  
  geom_rect(aes(xmin = -Inf, xmax = .549, ymin = .051, ymax = Inf), color = "transparent", fill = "white", size = 1) +
  
  geom_segment(aes(x = .3, xend = .3, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .35, xend = .35, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .4, xend = .4, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .45, xend = .45, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .5, xend = .5, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .55, xend = .55, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .65, xend = .65, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .75, xend = .75, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .85, xend = .85, y = .25, yend = Inf), color = "white", size = 2) +
  geom_segment(aes(x = .9, xend = .9, y = .25, yend = Inf), color = "white", size = 2) +
  
  geom_segment(aes(x = .4, xend = .4, y = .25, yend = Inf), color = "black", size = .25) +
  geom_segment(aes(x = .5, xend = .5, y = .25, yend = Inf), color = "black", size = .25) +
  geom_segment(aes(x = .6, xend = .6, y = .25, yend = Inf), color = "black", size = .25) +
  geom_segment(aes(x = .7, xend = .7, y = .25, yend = Inf), color = "black", size = .25) +
  geom_segment(aes(x = .8, xend = .8, y = .25, yend = Inf), color = "black", size = .25) +
  
  geom_segment(aes(y = -.25, yend = .25, x = .95, xend = .95), color = "black", size = .25) +
  geom_segment(aes(y = -.25, yend = .25, x = .25, xend = .25), color = "black", size = .25) +
  geom_hline(aes(yintercept = -.25), size = .25) +
  geom_hline(aes(yintercept = .25), size = .25) +
  
  geom_point(hjust = .5, fill = "white") +
  
  geom_text(
    data = groupmeans_MT_did,
    aes(y = .25, label = label), vjust = -.6, size = 3, color = "black"
  ) + 
  scale_color_manual(values = c(theBlue, theRed)) +
  guides(shape = guide_legend(ncol = 2, direction = "vertical"), color = guide_legend(ncol = 2, direction = "vertical"))

ggsave(
  "paper/figures/fig9_horizontal_arrows.pdf",
  plot_mt,
  "pdf", width = 6, height = 4
)

# Diff-in-diff regression ------------------

library(estimatr)

lm_DinD =
  lm(
    pctR ~ `2017`*electDay,
    data = mt %>% filter(year>=2016)
  )
lm_DinD_se =
  lm_robust(
    pctR ~ `2017`*electDay,
    data = mt %>% filter(year>=2016), clusters = clust
  )$std.error

lm_DinD_ctrl =
  lm(
    pctR ~ `2017`*electDay + birthyr + pct + city,
    data = mt %>% filter(year>=2016)
  )
lm_DinD_ctrl_se =
  lm_robust(
    pctR ~ `2017`*electDay + birthyr + pct + city,
    data = mt %>% filter(year>=2016), clusters = clust
  )$std.error

lm_DinD5 =
  lm(
    pctR ~ `2017`*electDay,
    data = mt %>% filter(year>=2016, five!=1)
  )
lm_DinD5_se =
  lm_robust(
    pctR ~ `2017`*electDay,
    data = mt %>% filter(year>=2016, five!=1), clusters = clust
  )$std.error

lm_DinD5_ctrl =
  lm(
    pctR ~ `2017`*electDay + birthyr + pct + city,
    data = mt %>% filter(year>=2016, five!=1)
  )
lm_DinD5_ctrl_se =
  lm_robust(
    pctR ~ `2017`*electDay + birthyr + pct + city,
    data = mt %>% filter(year>=2016, five!=1), clusters = clust
  )$std.error

lm_DinD_hetero =
  lm(
    pctR ~ `2017`*electDay*pctR_16,
    data = mt %>% filter(year>=2016)
  )
lm_DinD_hetero_se =
  lm_robust(
    pctR ~ `2017`*electDay*pctR_16,
    data = mt %>% filter(year>=2016), clusters = clust
  )$std.error

lm_DinD_hetero_ctrl =
  lm(
    pctR ~ `2017`*electDay*pctR_16 + birthyr + pct + city,
    data = mt %>% filter(year>=2016)
  )
lm_DinD_hetero_ctrl_se =
  lm_robust(
    pctR ~ `2017`*electDay*pctR_16 + birthyr + pct + city,
    data = mt %>% filter(year>=2016), clusters = clust
  )$std.error

lm_DinD_hetero5 =
  lm(
    pctR ~ `2017`*electDay*pctR_16,
    data = mt %>% filter(year>=2016, five!=1)
  )
lm_DinD_hetero5_se =
  lm_robust(
    pctR ~ `2017`*electDay*pctR_16,
    data = mt %>% filter(year>=2016, five!=1), clusters = clust
  )$std.error
lm_DinD_hetero5_ctrl =
  lm(
    pctR ~ `2017`*electDay*pctR_16 + birthyr + pct + city,
    data = mt %>% filter(year>=2016, five!=1)
  )
lm_DinD_hetero5_ctrl_se =
  lm_robust(
    pctR ~ `2017`*electDay*pctR_16 + birthyr + pct + city,
    data = mt %>% filter(year>=2016, five!=1), clusters = clust
  )$std.error

library(stargazer)

mt_regTab_noctrl = stargazer(
  lm_DinD, 
  lm_DinD_hetero, 
  lm_DinD5,
  lm_DinD_hetero5, 
  se = list(lm_DinD_se, lm_DinD_hetero_se, lm_DinD5_se, lm_DinD_hetero5_se), 
  star.cutoffs = c(.05, .01, .001),
  column.labels = c("D-in-D", "Interacted", "D-in-D", "Interacted"),
  covariate.labels = c("2017", "Election Day", "2017 x Elect Day", "\\% R 2016", "2017 x \\% R 2016", "Elect Day x \\% R 2016", "2017 x Elect Day x \\% R 2016"),
  order = c("2017`$", "^electDay$", "2017`:electDay$", "^pctR_16$", "2017`:p", "^electDay", "2017`:electDay"),
  notes = "Standard errors clustered by precinct.",
  dep.var.labels = "Republican two-party vote share", omit.stat = c("f", "rsq", "ser"),
  title = "Montana diff-in-diff, no controls",
  label = "tab:mt_reg_noctrl"
)

write(mt_regTab_noctrl, "paper/figures/mt_regTab_noctrl.txt")

mt_regTab_ctrl = stargazer(
  lm_DinD_ctrl, 
  lm_DinD_hetero_ctrl, 
  lm_DinD5_ctrl,
  lm_DinD_hetero5_ctrl, 
  se = list(lm_DinD_ctrl_se, lm_DinD_hetero_ctrl_se, lm_DinD5_ctrl_se, lm_DinD_hetero5_ctrl_se), 
  star.cutoffs = c(.05, .01, .001),
  column.labels = c("D-in-D", "Interacted", "D-in-D", "Interacted"),
  covariate.labels = c("2017", "Election Day", "2017 x Elect Day", "\\% R 2016", "2017 x \\% R 2016", "Elect Day x \\% R 2016", "2017 x Elect Day x \\% R 2016", "Mean birth year", "\\% voting absentee", "\\% in city limits"),
  order = c("2017`$", "^electDay$", "2017`:electDay$", "^pctR_16$", "2017`:p", "^electDay", "2017`:electDay"),
  notes = "Standard errors clustered by precinct.",
  dep.var.labels = "Republican two-party vote share", omit.stat = c("f", "rsq", "ser"),
  title = "Montana diff-in-diff",
  label = "tab:mt_reg_ctrl"
)

write(mt_regTab_ctrl, "paper/figures/mt_regTab_ctrl.txt")

# Placebo test version of the same -----------------


lm_placebo =
  lm(
    pctR ~ `2016`*electDay,
    data = mt %>% filter(year<=2016)
  )
lm_placebo_se =
  lm_robust(
    pctR ~ `2016`*electDay,
    data = mt %>% filter(year<=2016), clusters = clust
  )$std.error

lm_placebo_ctrl =
  lm(
    pctR ~ `2016`*electDay + birthyr + pct + city,
    data = mt %>% filter(year<=2016)
  )
lm_placebo_ctrl_se =
  lm_robust(
    pctR ~ `2016`*electDay + birthyr + pct + city,
    data = mt %>% filter(year<=2016), clusters = clust
  )$std.error
lm_placebo5 =
  lm(
    pctR ~ `2016`*electDay,
    data = mt %>% filter(year<=2016, five!=1)
  )
lm_placebo5_se =
  lm_robust(
    pctR ~ `2016`*electDay,
    data = mt %>% filter(year<=2016, five!=1), clusters = clust
  )$std.error

lm_placebo5_ctrl =
  lm(
    pctR ~ `2016`*electDay + birthyr + pct + city,
    data = mt %>% filter(year<=2016, five!=1)
  )
lm_placebo5_ctrl_se =
  lm_robust(
    pctR ~ `2016`*electDay + birthyr + pct + city,
    data = mt %>% filter(year<=2016, five!=1), clusters = clust
  )$std.error

lm_placebo_hetero =
  lm(
    pctR ~ `2016`*electDay*pctR_14,
    data = mt %>% filter(year<=2016)
  )
lm_placebo_hetero_se =
  lm_robust(
    pctR ~ `2016`*electDay*pctR_14,
    data = mt %>% filter(year<=2016), clusters = clust
  )$std.error

lm_placebo_hetero_ctrl =
  lm(
    pctR ~ `2016`*electDay*pctR_14 + birthyr + pct + city,
    data = mt %>% filter(year<=2016)
  )
lm_placebo_hetero_ctrl_se =
  lm_robust(
    pctR ~ `2016`*electDay*pctR_14 + birthyr + pct + city,
    data = mt %>% filter(year<=2016), clusters = clust
  )$std.error

lm_placebo_hetero5 =
  lm(
    pctR ~ `2016`*electDay*pctR_14,
    data = mt %>% filter(year<=2016, five!=1)
  )
lm_placebo_hetero5_se =
  lm_robust(
    pctR ~ `2016`*electDay*pctR_14,
    data = mt %>% filter(year<=2016, five!=1), clusters = clust
  )$std.error
lm_placebo_hetero5_ctrl =
  lm(
    pctR ~ `2016`*electDay*pctR_14 + birthyr + pct + city,
    data = mt %>% filter(year<=2016, five!=1)
  )
lm_placebo_hetero5_ctrl_se =
  lm_robust(
    pctR ~ `2016`*electDay*pctR_14 + birthyr + pct + city,
    data = mt %>% filter(year<=2016, five!=1), clusters = clust
  )$std.error


library(stargazer)

mt_placeboTab_noctrl = stargazer(
  lm_placebo, 
  lm_placebo_hetero, 
  lm_placebo5,
  lm_placebo_hetero5, 
  se = list(lm_placebo_se, lm_placebo_hetero_se, lm_placebo5_se, lm_placebo_hetero5_se), 
  star.cutoffs = c(.05, .01, .001),
  column.labels = c("D-in-D", "Interacted", "D-in-D", "Interacted"),
  covariate.labels = c("2016", "Election Day", "2016 x Elect Day", "\\% R 2014", "2016 x \\% R 2014", "Elect Day x \\% R 2014", "2016 x Elect Day x \\% R 2014"),
  order = c("2016`$", "^electDay$", "2016`:electDay$", "^pctR_14$", "2016`:p", "^electDay", "2016`:electDay"),
  notes = "Standard errors clustered by precinct.",
  dep.var.labels = "Republican two-party vote share", omit.stat = c("f", "rsq", "ser"),
  title = "Montana placebo diff-in-diff, no controls",
  label = "tab:mt_placebo_noctrl"
)

write(mt_placeboTab_noctrl, "paper/figures/mt_placeboTab_noctrl.txt")

mt_placeboTab_ctrl = stargazer(
  lm_placebo_ctrl, 
  lm_placebo_hetero_ctrl, 
  lm_placebo5_ctrl,
  lm_placebo_hetero5_ctrl, 
  se = list(lm_placebo_ctrl_se, lm_placebo_hetero_ctrl_se, lm_placebo5_ctrl_se, 
            lm_placebo_hetero5_ctrl_se), 
  star.cutoffs = c(.05, .01, .001),
  column.labels = c("D-in-D", "Interacted", "D-in-D", "Interacted"),
  covariate.labels = c("2016", "Election Day", "2016 x Elect Day", "\\% R 2014", "2016 x \\% R 2014", "Elect Day x \\% R 2014", "2016 x Elect Day x \\% R 2014", "Mean birth year", "\\% voting absentee", "\\% in city limits"),
  order = c("2016`$", "^electDay$", "2016`:electDay$", "^pctR_14$", "2016`:p", "^electDay", "2016`:electDay"),
  notes = "Standard errors clustered by precinct.",
  dep.var.labels = "Republican two-party vote share", omit.stat = c("f", "rsq", "ser"),
  title = "Montana placebo diff-in-diff",
  label = "tab:mt_placebo_ctrl"
)

write(mt_placeboTab_ctrl, "paper/figures/mt_placeboTab_ctrl.txt")
