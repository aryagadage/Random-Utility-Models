library(cjoint)

data("immigrationconjoint")

immigrationconjoint$from_iraq <- as.integer(
  immigrationconjoint[["Country of Origin"]] == "Iraq"
)

edu_map <- c(
  "no formal"        = "below high school",
  "4th grade"        = "below high school",
  "8th grade"        = "below high school",
  "high school"      = "high school",
  "two-year college" = "college",
  "college degree"   = "college",
  "graduate degree"  = "graduate degree"
)
immigrationconjoint$Education_grp <- factor(
  edu_map[as.character(immigrationconjoint$Education)],
  levels = c("below high school", "high school", "college", "graduate degree")
)
