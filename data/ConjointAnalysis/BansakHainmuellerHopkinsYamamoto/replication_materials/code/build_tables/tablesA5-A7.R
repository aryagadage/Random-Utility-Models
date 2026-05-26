source("../helper_scripts/crosstab.R")


# Specify dataset ---------------------------------------------------------

dataset <- "Stage 1 MTurk"
#dataset <- "Stage 2 MTurk"
#dataset <- "Stage 2 SSI"


# Load data and subset to single observation per respondent ---------------

if(dataset == "Stage 1 MTurk"){
  svyx <- read.csv("../../data/cand_s1.csv")
  svyz <- subset(svyx,svyx$profile == 1)
}

if(dataset == "Stage 2 MTurk"){
  svyx <- read.csv("../../data/cand_s2_MTurk.csv")
  svyz <- subset(svyx,svyx$profile == "A1")
}

if(dataset == "Stage 2 SSI"){
  svyx <- read.csv("../../data/cand_s2_SSI.csv")
  svyz <- subset(svyx,svyx$profile == "A1")
}


# Label covariate values --------------------------------------------------

svyz$cov_gender <- NA
svyz$cov_gender[svyz$r_gender == 1] <- "Male"
svyz$cov_gender[svyz$r_gender == 2] <- "Female"

svyz$cov_age <- svyz$r_age
svyz$cov_agegroup <- NA
svyz$cov_agegroup[svyz$cov_age <= 29] <- "29 and Under"
svyz$cov_agegroup[svyz$cov_age > 29 & svyz$cov_age <= 44] <- "30-44"
svyz$cov_agegroup[svyz$cov_age > 44 & svyz$cov_age <= 64] <- "45-64"
svyz$cov_agegroup[svyz$cov_age > 64] <- "65 and Over"

svyz$cov_educ <- NA
svyz$cov_educ[svyz$r_educ == 1] <- "1. Less than High School Diploma"
svyz$cov_educ[svyz$r_educ == 2] <- "2. High School Diploma / GED"
svyz$cov_educ[svyz$r_educ == 3] <- "3. Some College (no degree)"
svyz$cov_educ[svyz$r_educ == 4] <- "4. 2-year College Degree"
svyz$cov_educ[svyz$r_educ == 5] <- "5. 4-year College Degree"
svyz$cov_educ[svyz$r_educ == 6] <- "6. Graduate Degree"

svyz$cov_income <- NA
svyz$cov_income[svyz$r_income == 1] <- "1. $0 - $24,999"
svyz$cov_income[svyz$r_income == 2] <- "2. $25,000 - $49,999"
svyz$cov_income[svyz$r_income == 3] <- "3. $50,000 - $74,999"
svyz$cov_income[svyz$r_income == 4] <- "4. $75,000 - $99,999"
svyz$cov_income[svyz$r_income == 5] <- "5. $100,000 - $149,999"
svyz$cov_income[svyz$r_income == 6] <- "6. $150,000 - $199,999"
svyz$cov_income[svyz$r_income == 7] <- "7. $200,000+"

svyz$cov_party <- NA
svyz$cov_party[svyz$r_party == 1] <- "1. Republican"
svyz$cov_party[svyz$r_party == 2] <- "2. Democrat"
svyz$cov_party[svyz$r_party == 3] <- "3. Independent"
svyz$cov_party[svyz$r_party == 4] <- "4. Other"

svyz$cov_partisanship <- NA
svyz$cov_partisanship[svyz$r_party == 1] <- "1. Republican"
svyz$cov_partisanship[svyz$r_party == 2] <- "2. Democrat"
svyz$cov_partisanship[svyz$r_lean == 1] <- "1. Republican"
svyz$cov_partisanship[svyz$r_lean == 0] <- "3. None"
svyz$cov_partisanship[svyz$r_lean == -1] <- "2. Democrat"


# Distributions -----------------------------------------------------------

crosstab(svyz,row.vars = "cov_gender")
crosstab(svyz,row.vars = "cov_agegroup")
crosstab(svyz,row.vars = "cov_educ")
crosstab(svyz,row.vars = "cov_income")
crosstab(svyz,row.vars = "cov_party")
crosstab(svyz,row.vars = "cov_partisanship")
