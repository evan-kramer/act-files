# ACT Highest Score File
# Evan Kramer
# 9/20/2017

library(tidyverse)
library(lubridate)
library(haven)
library(stringr)

# Read in data
grad = read_csv("K:/ORP_accountability/data/2017_graduation_rate/student_level_20170830.csv")
act_highest = read_dta("K:/Assessment_Data Returns/ACT/2016-17/Graduating Class- Highest/20170914_ACT_GraduatingCohortHighestScore_SY2016-17_Whalen_v1.dta")
act_state_day = read_dta("K:/ORP_accountability/data/2016_ACT/TN_790Select_2016.dta")

# Start with graduates
student_level = left_join(mutate(student_key = as.double(student_key)), act_highest, 
                          by = c("student_key" = "unique_student_id"))

# Append SAS-matched records - DON'T CURRENTLY HAVE THESE?!


