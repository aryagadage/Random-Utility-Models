rm(list=ls())

library(tidyverse)
library(foreign)
library(reshape2)

as_num = function(x) as.numeric(as.character(x))
source("scripts/helpers.R")

###  ANES data -------------------

d = read.dta("C:/R/ANES/anes_timeseries_2016_Stata12.dta")  # get this file off the ANES website and save it to a local directory. See readme.docx

vars = c(
  senate_term = "V161513",
  spend = "V161514",
  house = "V161515",
  senate_party = "V161516",
  ryan = "V162073a",
  merkel = "V162074a",
  putin = "V162075a",
  roberts = "V162076a",
  dem_satisfied = "V162290",
  
  interview= "V160501",
  completion= "V160502",
  weight_w1 = "V160101w",
  weight_w2 = "V160102w",
  
  auth_respect = "V162239",
  auth_manners = "V162240",
  auth_obey    = "V162241",
  auth_behave  = "V162242",
  
  voteduty = "V161151x",
  
  education= "V161270",
  male     = "V161342",
  hispanic = "V161309",
  #inc      = "V161351",
  pid7     = "V161158x",
  age      = "V161267x",
  
  white = "V161310a",
  black = "V161310b",
  namer = "V161310c",
  asian = "V161310d",
  pacif = "V161310e",
  other = "V161310f"
)

###  ANES process --------------

anes = d[,vars]
names(anes) = names(vars)

anes = filter(anes, grepl("Web", anes$interview))

anes = apply(anes, 2, function(x){x[substr(x,1,1)=="-"]=NA;x}) %>% as.data.frame
anes[,grepl("white|black|namer|asian|pacif|other", names(anes))] =
  apply(anes[,grepl("white|black|namer|asian|pacif|other", names(anes))], 2, function(x)as.numeric(substr(x,1,1)))

anes$asian = anes$asian + anes$pacif
anes = anes %>% select(-pacif)

anes$other = 0
anes$other[which((anes$black+anes$white+anes$asian+anes$namer)!=1)] = 1

anes = within(anes,{
  black = black - black*other
  white = white - white*other
  asian = asian - asian*other
  namer = namer - namer*other
})

anes$roberts==-6

# don't need to recode pid7, age

anes = anes %>%
  mutate(
    weight_w1 = as_num(weight_w1),
    weight_w2 = as_num(weight_w1),
    male    = grepl("Male", male) %>% as.numeric,
    gender  = recode(male, `0`="Female", `1`="Male"),
    hispanic  = ifelse(grepl("Yes",  hispanic), "Yes", "No"),
    
    race    = ifelse(white==1, "White", ifelse(black==1, "Black", ifelse(asian==1, "Asian or Pacific Islander", ifelse(other==1, "Other", ifelse(namer==1, "American Indian or Alaska Native", NA))))),
    
    age     = as_num(gsub("\\..+", "", age)),
    age     = cut(age, c(0, 4, 7, 10, 14), c("[18,34]", "(34,49]", "(49,64]", "(64,96]")),
    education    = as_num(gsub("\\.", "", substr(education,1,2))),
    education    = cut(education, c(0,8,9,10,12,13,17), 
                  c("Did not complete high school", "High school graduate",
                    "Some college, no degree", "Associate's degree", 
                    "Bachelor's degree", "Graduate or professional degree")),
    pid7 = as_num(gsub("\\..+", "", pid7)) - 4,
    voteduty = -1*(as_num(gsub("\\..+", "", voteduty)) - 4)#,
    #completion = grepl("Pre and Post", completion) + 1
  )

anes = anes %>% select(-namer)

anes = anes %>%
  mutate_at(vars(spend, house, dem_satisfied, starts_with("auth")),
            function(x) gsub("^[0-9]..", "", x)
  )

anes = anes %>%
  mutate(
    
    roberts = as.numeric(as_num(roberts)>0),
    merkel  = as.numeric(!grepl("Not correct", merkel)),
    
    ryan  = as.numeric(!grepl("Not correct", ryan)),
    
    senate_party  = as.numeric(grepl("Republicans", senate_party)),
    
    
    putin  = as.numeric(!grepl("Not correct", putin)),
    senate_term  = as.numeric(as_num(senate_term)==6),
    
    spend = as.numeric(spend=="Foreign aid"),
    house = as.numeric(house=="Republicans")
)
anes = anes %>% mutate(
  auth_behave = recode(auth_behave, `Being considerate`="Considerate", `Well behaved`="Well-behaved")
)

###  Lucid data -------------

setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/Dropbox/GrahamSvolik/"))
lucid = read_csv("data/data_covariates.csv")
w1_dem = read_csv("data/data_howDemocratic.csv")
weight = read_csv("data/weights.csv")

lucid = 
  left_join(
    lucid,
    w1_dem %>% select(id, dem_satisfied)
  ) %>%
  left_join(weight)

lucid = lucid %>%
  mutate(
    dem_satisfied = recode_factor(round(dem_satisfied, 1), 
                           `0`="Not at all satisfied", `0.3`="Not very satisfied", `0.7`="Fairly satisfied", `1`="Very satisfied")
  )

lucid = lucid %>% select(-id, -st, -cheat_wave1, -ends_with("ideal"), -tax, -age, -educ, -hhi, -ideo, -ethnicity, -political_party, -pid_lean, -pid_fold, -pid_lean, -pid3, -pid_Lean, -state, -knowl_anes_total, -auth_total, -trump)

lucid = lucid %>% rename(hhi = hhi_cut, age = age_cut)

apply(anes %>% select(-weight_w1, -weight_w2), 2, function(x)print(table(x)))
apply(lucid %>% select(-rid, -weight), 2, function(x)print(table(x)))

###  Table for each survey --------------

refusedMissing = function(x){x[is.na(x)|x=="Prefer not to answer"|x=="None of the above"] = "Refused or missing"; x}

anes_tab =
  anes %>% 
  select(-white, -black, -asian, -other, -interview, -male) %>%
  melt(c("weight_w1", "weight_w2", "completion"))

anes_tab$wave = vars[match(as.character(anes_tab$variable), names(vars))]
anes_tab$wave = grepl("V162", anes_tab$wave) + 1
anes_tab$weight = anes_tab$weight_w1
anes_tab$weight[anes_tab$wave==2] = anes_tab$weight_w2[anes_tab$wave==2]

anes_tab = anes_tab %>% filter(!(wave==2 & completion==1))

anes_tab = anes_tab %>% select(-wave, -weight_w1, -weight_w2)

anes_tab = anes_tab %>% 
  mutate(value = refusedMissing(value)) %>%
  filter(!is.na(weight)) %>%      # dumps the wave 1 respondents who did not answer the wave 2 Qs
  group_by(variable, value) %>% 
  summarize(n = sum(weight, na.rm=T)) %>% 
  group_by(variable) %>% mutate(ANES = n/sum(n)) %>% select(-n) 

lucid_tab =
  lucid %>%
  melt(c("rid", "weight")) %>%
  mutate(variable = gsub("knowl_", "", variable),
         value = refusedMissing(value)) %>%
  group_by(variable, value) %>%
  summarize(n   = n(),
            n_w = sum(weight)) %>%
  group_by(variable) %>%
  mutate(
    Unweighted = n / sum(n),
    Weighted = n_w / sum(n_w)
  ) %>%
  select(variable, value, Unweighted, Weighted)

acs_tab = read_csv("data/benchmarking/acs_margins.csv")
acs_tab = acs_tab %>% select(-n) %>% rename(ACS=pct) %>%
  mutate(variable = recode(variable, ethnicity="race"))


###  Combined table  -------------------

compare = full_join(lucid_tab, anes_tab) %>% left_join(acs_tab) %>% filter(!is.na(value))
compare = compare %>% mutate(value = recode(value, `[18,34]`="18 to 34", `(34,49]`="35 to 49", `(49,64]`="50 to 64", `(64,96]`="65 to 96"))

makezeroes  = function(x) rep("0", as.numeric(x)) %>% paste(collapse="")
makezerovec = function(x) vapply(x, makezeroes, "a")
trail_zero  = function(x, digits_desired=1){
  notNA = which(!is.na(as.numeric(x)))
  y = as.numeric(x[notNA])
  
  y = as.character(round(y, digits_desired))
  digits = nchar(y)
  noperiod = which(!grepl("\\.", y))
  if(length(noperiod)>0) y[noperiod] = paste0(y[noperiod], ".")
  need0 = which(digits!=max(digits))
  if(length(need0)>0) y[need0] = paste0(y[need0], makezerovec(max(digits) - digits[need0] - 1))
  
  zeroes = nchar(gsub(".\\.|.+\\.", "", y))
  toomany = which(zeroes>digits_desired)
  if(length(toomany)>0) y[toomany] = gsub("\\..|\\..+", paste0(".", rep("0", digits_desired)), y[toomany])
  
  x[notNA] = y
  x
}

compare = compare %>%
  mutate_at(
    vars(ANES, Unweighted, Weighted, ACS),
    function(x) trail_zero(x*100, 1)
  ) %>%
  filter(
    !(ACS==0 & ANES==0 & Unweighted==0 & Weighted==0)
  ) %>%
  mutate_at(
    vars(Unweighted, Weighted, ANES),
    function(x) {x[x==""|is.na(x)] = "0.0"; x}
  )
compare$ACS[is.na(compare$ACS)] = ""


compare = compare %>%
  group_by() %>%
  mutate(
    Var = 
      recode_factor(
        variable, 
        
        age         = "Age",
        education   = "Education",
        gender      = "Gender",
        hispanic    = "Hispanic",
        race        = "Race",
        pid7        = "Party ID",
        hhi         = "Household income",
        
        dem_satisfied="Satisfied with U.S. democracy",
        
        auth_respect= "Authoritarian trait 1",
        auth_manners= "Authoritarian trait 2",
        auth_obey   = "Authoritarian trait 3",
        auth_behave = "Authoritarian trait 4",
        voteduty    = "Voting a duty/choice",
        spend       = "Foreign aid budget share",
        house       = "House party control",
        senate_term = "Senate term length",
        senate_party= "Senate party control",
        merkel      = "Angela Merkel's job",
        ryan        = "Paul Ryan's job",
        roberts     = "John Roberts' job",
        putin       = "Vladimir Putin's job",
        .ordered = T
      ),
    Variable = as.character(Var)
  )

compare = within(compare, {
  value[value==0 & !(variable %in% c("voteduty", "pid7"))] = "Incorrect"
  value[value==1 & !(variable %in% c("voteduty", "pid7"))] = "Correct"
})

compare = compare %>%
  mutate(
    num = as.numeric(gsub(" to .+", "", value))
  )
compare$num[is.na(compare$num)] = 0
compare$num[compare$value=="Yes"] = -1
compare$num[compare$value=="Refused or missing"] = 1000
compare = compare %>%
  arrange(Var, num, value) %>%
  mutate(
    Value = factor(value, unique(value))
  )

compare$ANES = ifelse(compare$Variable == "Household income", "", compare$ANES)

makeBlank = rep(0, nrow(compare))
for(i in 2:nrow(compare)){
  if(compare$Variable[i]==compare$Variable[i-1]) makeBlank[i] = 1
}
compare$Variable[as.logical(makeBlank)] = ""
compare = compare %>% select(Variable, Value, ACS, ANES, Weighted, Unweighted)

compare = compare %>% mutate_at(vars(-Variable, -Value), 
                                function(x){
                                  x = trail_zero(x, 1)
                                  x[x=="."] = ""
                                  x
                                  })

###  Separate and export ----------------

tab_chars    = compare[1:38,]
tab_attitude = compare[39:nrow(compare),] %>% select(-ACS)

xtab_hlines = function(out){
  require(xtable)
  out = print(xtable(out), include.rownames=F, digits = 1, size = "small")
  out = strsplit(out, "\n")[[1]]
  i = done = 0
  while(done == 0){
    i = i+1
    if(grepl("^  [A-Z]", out[i])){
      out = c(out[1:(i-1)], "\\hline ", out[i:length(out)])
      i = i+1
    }
    if(i>1) if(grepl("Party ID", out[i-1])) out[i] = gsub(" & -2", "(7 point) & -2", out[i])
    if(i==length(out)){
      done = 1
    }
  }
  out = paste(out, collapse = "\n")
  out = gsub("llllll", "llrrrr", out)
  out = gsub("lllll", "llrrr", out)
  out = gsub("ANES", "~~ANES", out)
}

tab_chars    = xtab_hlines(tab_chars)
tab_attitude = xtab_hlines(tab_attitude)

write(tab_chars, "paper/figures/appendix_benchmark_demog.txt")
write(tab_attitude, "paper/figures/appendix_benchmark_attitudes.txt")


