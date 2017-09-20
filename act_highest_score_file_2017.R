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
act_state_day2016 = read_dta("K:/ORP_accountability/data/2016_ACT/TN_790Select_2016.dta")
act_state_day2017 = read_dta("K:/Assessment_Data Returns/ACT/2016-17/Junior Day File/20170713_ACT_JuniorDayResults_SY2016-17_Whalen_v1.dta")

# Append all ACT files (highest, SAS-matched, two state testing files)
# Merge SAS-matched records to find highest score - DON'T CURRENTLY HAVE THESE?!
# Look in two most recent state testing files to find score
act = transmute(act_highest, unique_student_id, test_date = act_testdate, english = act_english_highest,
                math = act_math_highest, reading = act_reading_highest, science = act_science_highest, 
                composite = act_composite_highest, file = "ACT highest") %>% 
    bind_rows(transmute(act_state_day2016, unique_student_id = state_stud_id, test_date = testdate,
                        english = act_eng, math = act_math, reading = act_read, science = act_sci, 
                        composite = act_composite, file = "State day 2016")) %>% 
    bind_rows(transmute(act_state_day2017, unique_student_id = state_stud_id, test_date = testdate,
                        english = act_eng, math = act_math, reading = act_read, science = act_sci, 
                        composite = act_composite, file = "State day 2017")) %>% 
    mutate(test_date = ifelse(test_date > 999, str_c(str_sub(as.character(test_date), 1, 2), "0120", str_sub(as.character(test_date), 3, 4)), 
                              str_c("0", str_sub(test_date, 1, 1), "0120", str_sub(as.character(test_date), 2, 3))),
           test_date = mdy(test_date)) %>% 
    arrange(unique_student_id, desc(composite), desc(math), desc(reading), 
            desc(english), desc(science), desc(test_date), file) %>% 
    group_by(unique_student_id) %>% 
    summarize_all(funs(first(.)))
    
# Include only graduates
student_level = grad %>% 
    filter(included_in_cohort == "Y" & completion_type == 1) %>% 
    mutate(student_key = as.double(student_key)) %>% 
    left_join(act, by = c("student_key" = "unique_student_id")) 

# Output student level file
student_level = replace(student_level, is.na(student_level), NA)
#write_csv("K:/ORP_accountability/data/2017_ACT/act_cohort_student_level_EK.csv", na = "")

# Collapse to district and school 
## District
## School