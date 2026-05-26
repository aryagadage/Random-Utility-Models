#-------------------
#  ACS 2016 table --- from Public Use Microdata
#-------------------

setwd("C:/R/ACS")

library(tidyverse)
library(reshape2)
library(survey)

as_num = function(x) as.numeric(as.character(x))

dA = read_csv("psam_pusa.csv") %>% select(AGEP, CIT, RAC1P, HISP, SEX, SCHL, PWGTP)
dB = read_csv("psam_pusb.csv") %>% select(AGEP, CIT, RAC1P, HISP, SEX, SCHL, PWGTP)

dat = rbind(dA, dB)

# dat = dA %>% select(AGEP, CIT, RAC1P, HISP, SEX, SCHL, PWGTP)

dat =
  dat %>%
  mutate_all(as_num) %>%
  filter(CIT!=5,AGEP>=18) %>%         # remove non-citizens and under 18
  
  mutate(
    age      = cut(as.numeric(AGEP), c(18, 34, 49, 64, max(AGEP, na.rm=T)), include.lowest = T),
    gender   = recode(SEX, `1`="Male", `2`="Female"),
    hispanic = recode(HISP, `1`="No", .default="Yes"),
    race     = recode(RAC1P, `1`="White", `2`="Black", `3`="American Indian or Alaska Native",
                      `4`="American Indian or Alaska Native", `5`="American Indian or Alaska Native",
                      `6`="Asian or Pacific Islander", `7`="Asian or Pacific Islander", `8`="Other", `9`="Other"),
    educ     = ifelse(SCHL<=15, "Did not complete high school", ifelse(SCHL<=17, "High school graduate", 
                                                                       ifelse(SCHL<=19, "Some college, no degree", ifelse(SCHL==20, "Associate's degree",
                                                                                                                          ifelse(SCHL==21, "Bachelor's degree", "Graduate or professional degree")))))
  )

demog_tab =
  dat %>%
  select(age, gender, hispanic, ethnicity=race, education=educ, PWGTP) %>%
  melt("PWGTP") %>%
  group_by(variable, value) %>%
  summarize(n = sum(PWGTP)) %>%
  group_by(variable) %>%
  mutate(pct = n / sum(n)) 

hhi = read_csv("C:/R/ACS/acs17_hhi.csv")
hhi = hhi %>%
  mutate(
    pct = gsub("%", "", pct),
    pct = as.numeric(pct)/100,
    #pct   = n/sum(n),
    value = gsub("Less than \\$|,.+|\\$", "", value) %>% as.numeric %>% cut(c(0, 25, 50, 75, 100, 200, 201), right=F),
    variable = "hhi"
  ) %>%
  group_by(variable, value) %>% summarize(pct = sum(pct))
#hhi
  
demog_tab = bind_rows(demog_tab, hhi#, read_csv("C:/R/ACS/gallup_pid_sep4-12_2018.csv") %>% mutate(variable="Party")
                      )

#------------------------
#  Save ACS --- new plan is to merge this with ANES table
#-----------------------

setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Dropbox/GrahamSvolik"))
write_csv(demog_tab, "data/benchmarking/acs_margins.csv")

#-------------------
#  Lucid table
#-------------------

w1_covar = read_csv("data/data_covariates.csv")

w1_covar =
w1_covar %>%
  select(
    id, age, gender, ethnicity, hispanic, hhi, education#, Party=pid3
  ) %>%
  mutate(
    age       = cut(as.numeric(age), c(18, 34, 49, 64, max(dat$AGEP, na.rm=T)), include.lowest = T, ordered_result = T),
    ethnicity = gsub("Asian.+|Pacific Islander.+", "Asian or Pacific Islander", ethnicity),
    ethnicity = gsub("Some other race", "Other", ethnicity),
    ethnicity = gsub("Black.+", "Black", ethnicity),
    hispanic  = gsub(",.+| ,.+| $", "", hispanic),
    education = gsub("Doctorate |Master's or professional ", "Graduate or professional ", education),
    education = gsub("Other post high school.+", "High school graduate", education),
    education = gsub("Some high school.+", "Did not complete high school", education),
    education = gsub("Completed some.+", "Some college, no degree", education),
    hhi       = gsub("\\$|,.+|Less than \\$", "", hhi) %>% as.numeric,
    hhi       = cut(hhi, c(0, 25, 50, 75, 100, 200, 201), right = F, ordered_result = T)#,Party     = gsub("Other", "Independent", Party)
  ) 

w1_covar2 = w1_covar %>%
  filter(
    !is.na(education), !is.na(age), !is.na(ethnicity), !is.na(hispanic), !is.na(hhi), !is.na(gender)
  ) %>% 
  filter(hispanic!="Prefer not to answer", ethnicity!="Prefer not to answer", ethnicity!="American Indian or Alaska Native", education!="Refused or missing", hhi!="Refused or missing", 
         hispanic!="Refused or missing", ethnicity!="Refused or missing", education!="None of the above")

survey_obj = survey::svydesign(ids=~0, data=w1_covar2 %>% mutate_all(as.character))

to_tab = function(x){
  y = x$pct %>% (function(x)x/sum(x))
  #y = x$n
  x = x %>% filter(!is.na(x$value), !is.na(x$variable), value!="Prefer not to say", value!="Missing")
  names(y)= x$value
  y %>% as.table %>% as.data.frame
}

demog_tab2 = demog_tab %>% filter(value!="Pacific Islander", value!="American Indian or Alaska Native")
freqs_acs = split(demog_tab2, demog_tab2$variable) %>% map(to_tab)
for(i in 1:length(freqs_acs)){
  names(freqs_acs[[i]])[1] = names(freqs_acs)[i]
}

set.seed(0)
rake_obj = rake(survey_obj, list(~age, ~education, ~ethnicity, ~gender, ~hhi, ~hispanic), freqs_acs)

weight_dat = data.frame(id = rake_obj$variables$id, weight = weights(rake_obj)*length(weights(rake_obj)))
weight_dat = weight_dat %>% mutate(id = as.numeric(as.character(id)))

weight_dat = left_join(w1_covar %>% select(id), weight_dat)
weight_dat$weight[is.na(weight_dat$weight)] = 1

weight_dat$weight[weight_dat$weight > 2] = 2
weight_dat$weight[weight_dat$weight < 1/2] = 1/2
weight_dat$weight = weight_dat$weight*(nrow(weight_dat) / sum(weight_dat$weight))

write_csv(weight_dat, "weights.csv")

w1_covar = left_join(w1_covar, weight_dat)



# Comparison

library(reshape2)
lucid_tab = w1_covar %>%
  melt(c("rid", "weight")) %>%
  (function(x){x$value[is.na(x$value)|x$value=="Prefer not to answer"|x$value=="None of the above"] = "Refused or missing"; x}) %>%
  group_by(variable, value) %>%
  summarize(n = n(),
            n_w = sum(weight)) %>%
  group_by(variable) %>%
  mutate(pct = n / sum(n),
         pct_w = n_w / sum(n_w))


#-----------------------
#  Make the table

makezeroes  = function(x) rep(0, as.numeric(x)) %>% paste(collapse="")
makezerovec = function(x) vapply(x, makezeroes, "a")
trail_zero  = function(x, digits_desired=1){
  notNA = which(!is.na(x))
  y = x[notNA]
  
  y = round(y, digits_desired)
  digits = nchar(gsub(".+\\.", "", y))
  if(max(digits)>digits_desired) y = round(y, digits_desired)
  y = as.character(y)
  y[!grepl("\\.", y)] = paste0(y[!grepl("\\.", y)], ".")
  digits = nchar(gsub(".+\\.", "", y))
  paste0(y, makezerovec(digits_desired - digits))
  
  x[notNA] = y
  x
}



out_tab = left_join(lucid_tab %>% select(-n, -n_w) %>% rename(Unweighted = pct, Weighted = pct_w),
                    demog_tab %>% select(-n) %>% rename(ACS = pct)) %>%
  mutate_at(vars(ACS, Unweighted, Weighted), function(x)trail_zero(100*x)) %>%
  mutate(
    colOrder = gsub("\\[|\\]|\\(|\\)|,.+", "", value) %>% as.numeric
  ) 
out_tab$colOrder[out_tab$value=="Yes"] = 0
out_tab$colOrder[out_tab$value=="No"] = 1
out_tab$colOrder[is.na(out_tab$value)] = 2




keep_if_equal_prev = function(x){
  x1 = x[1]
  x = x[-1]
  x = ifelse(x!=c(x1, x[-length(x)]), x, "")
  c(x1, x)
}

out_tab = out_tab %>%
  arrange(variable, colOrder, value) %>%
  group_by() %>%
  mutate(
    variable = recode(variable, education="Education", age="Age", hhi="HH income", ethnicity="Race", hispanic="Hispanic", gender="Gender"),
    variable = keep_if_equal_prev(variable)
    ) %>%
  rename(Variable = variable, Value = value)

out_tab = within(out_tab,{
  Unweighted[1] = paste0(Unweighted[1], "%")
  Value[Value=="[200,201)"] = "[200,inf)"
})

library(xtable)

out = print(
  xtable(
    out_tab %>% select(-colOrder)#, label = "tab:ACSdemog", caption = "Demographic characteristics: Unweighted sample vs. 2016 American Community Survey"
  ), include.rownames=F)

# gsub out ---
# add hlines above anything that doesn't start with &

out = strsplit(out, "\n")[[1]]
i = done = 0
while(done == 0){
  i = i+1
  if(grepl("^  [A-Z]", out[i])){
    out = c(out[1:(i-1)], "\\hline ", out[i:length(out)])
    i = i+1
  }
  if(i==length(out)){
    done = 1
  }
}
out = paste(out, collapse = "\n")

write(out, "paper/figures/ACS_demog_tab.txt")
