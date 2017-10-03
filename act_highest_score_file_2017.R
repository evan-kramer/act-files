# ACT Highest Score File
# Evan Kramer
# 9/29/2017

library(tidyverse)
library(lubridate)
library(haven)
library(stringr)

# Macros
date = str_c(day(today()), month(today(), label = T), year(today()))
dat = T
sta = F
sys = T
sch = F 
che = F

# Read in data
if(dat == T) {
    grad = read_csv("K:/ORP_accountability/data/2017_graduation_rate/student_level_20170830.csv")
    act_highest = read_dta("K:/Assessment_Data Returns/ACT/2016-17/Graduating Class- Highest/20170914_ACT_GraduatingCohortHighestScore_SY2016-17_Whalen_v1.dta")
    act_tae = read_dta("K:/Assessment_Data Returns/ACT/2016-17/Graduating Class- Highest/20170921_ACT_GraduatingCohortHighestScore_SY2016-17_Tae_matching.dta")
    act_p20 = readxl::read_excel("K:/Research_Transfers/Data_Management/2017_Sept_27_ACT_P20/TLDS_ACT_2016.xlsx")
    act_state_day2016 = read_dta("K:/ORP_accountability/data/2016_ACT/TN_790Select_2016.dta") %>% 
        filter(test_location != "M")
    act_state_day2017 = read_dta("K:/Assessment_Data Returns/ACT/2016-17/Junior Day File/20170713_ACT_JuniorDayResults_SY2016-17_Whalen_v1.dta") %>% 
        filter(test_location != "M")
    
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
        bind_rows(act_tae %>% 
                      filter(is.na(unique_student_id) | usid != unique_student_id) %>% 
                      transmute(unique_student_id = usid, test_date = act_testdate, 
                                english = act_english_highest, math = act_math_highest, 
                                reading = act_reading_highest, science = act_science_highest, 
                                composite = act_composite_highest, file = "Tae match")) %>% 
        mutate(test_date = ifelse(test_date > 999, str_c(str_sub(as.character(test_date), 1, 2), "0120", str_sub(as.character(test_date), 3, 4)), 
                                  str_c("0", str_sub(test_date, 1, 1), "0120", str_sub(as.character(test_date), 2, 3))),
               test_date = mdy(test_date),
               unique_student_id = as.integer(unique_student_id)) %>% 
        arrange(unique_student_id, desc(composite), desc(math), desc(reading), 
                desc(english), desc(science), desc(test_date), file) %>% 
        group_by(unique_student_id) %>% 
        summarize_all(funs(first(.))) 
    
    # Include only graduates
    student_level = grad %>% 
        filter(included_in_cohort == "Y" & completion_type == 1) %>% 
        left_join(act, by = c("student_key" = "unique_student_id")) %>% 
        mutate(n_cr_english = ifelse(is.na(composite), NA, as.numeric(english >= 18)),
               n_cr_math = ifelse(is.na(composite), NA, as.numeric(math >= 22)), 
               n_cr_reading = ifelse(is.na(composite), NA, as.numeric(reading >= 22)),
               n_cr_science = ifelse(is.na(composite), NA, as.numeric(science >= 23)),
               n_cr_all = ifelse(is.na(composite), NA, as.numeric(n_cr_english == 1 & n_cr_math == 1 & n_cr_reading == 1 & n_cr_science == 1))) 
    
    student_level = replace(student_level, is.na(student_level), NA)
    
    # Output student level file
    output = student_level %>% 
        select(system, school, state_stud_id = student_key, last_name, first_name, 
               english, math, reading, science, composite, gender, n_cr_english,
               n_cr_math, n_cr_reading, n_cr_science, n_cr_all)
    write_csv(output, "K:/ORP_accountability/data/2017_ACT/act_cohort_student_level_EK.csv", na = "")
}

# Collapse to state, district, and school 
## State
if(sta == T) {
    ### All
    all = student_level %>% 
        summarize(subgroup = "All Students",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite)) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite))

    ### BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & race_ethnicity %in% c("B", "H", "I")) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) 
    
    ### ED
    ed = student_level %>% 
        filter(econ_dis == "Y") %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & econ_dis == "Y") %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite))
    
    ### Non-ED
    non_ed = student_level %>% 
        filter(econ_dis == "N") %>% 
        summarize(subgroup = "Non-Economically Disadvantaged",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & econ_dis == "N") %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) 
    
    ### EL
    el = student_level %>% 
        filter(el == "Y") %>% 
        summarize(subgroup = "English Learners",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & el == "Y") %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite))
    
    ### Non-EL
    non_el = student_level %>% 
        filter(el == "N") %>% 
        summarize(subgroup = "Non-English Learners",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & el == "N") %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) 
    
    ### SWD
    swd = student_level %>% 
        filter(swd == "Y") %>% 
        summarize(subgroup = "Students with Disabilities",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & swd == "Y") %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite))
    
    ### Non-SWD
    non_swd = student_level %>% 
        filter(swd == "N") %>% 
        summarize(subgroup = "Non-Students with Disabilities",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & swd == "N") %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) 
    
    ### Super 
    super = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y") %>% 
        summarize(subgroup = "Super Subgroup",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & (race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y")) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) 
    
    ### Individual racial/ethnic groups
    race_eth_list = unique(student_level$race_ethnicity[!is.na(student_level$race_ethnicity)])
    ind_race_eth = data.frame()
    
    for(r in seq_along(race_eth_list)) {
        temp = student_level %>% 
            filter(race_ethnicity == race_eth_list[r]) %>% 
            summarize(subgroup = race_eth_list[r],
                      enrolled = n(),
                      tested = sum(!is.na(composite)),
                      valid_tests = sum(!is.na(composite)),
                      valid_tests_male = sum(gender == "M" & !is.na(composite)),
                      valid_tests_female = sum(gender == "F" & !is.na(composite)),
                      n_cr_english = sum(n_cr_english, na.rm = T),
                      n_cr_math = sum(n_cr_math, na.rm = T),
                      n_cr_reading = sum(n_cr_reading, na.rm = T),
                      n_cr_science = sum(n_cr_science, na.rm = T),
                      n_cr_all = sum(n_cr_all, na.rm = T),
                      n_21_or_higher = sum(composite >= 21, na.rm = T),
                      n_below_19 = sum(composite < 19, na.rm = T),
                      n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                      n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                      n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                      n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
            mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
                   pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
                   participation_rate = round(100 * tested / enrolled, 0),
                   pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
                   pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
                   pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
                   pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
            mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
            bind_cols(student_level %>% 
                          filter(!is.na(composite) & race_ethnicity == race_eth_list[r]) %>% 
                          summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite))
        
        ind_race_eth = bind_rows(ind_race_eth, temp)
    }
    
    ind_race_eth = ind_race_eth %>% 
        mutate(subgroup = case_when(ind_race_eth$subgroup == "A" ~ "Asian",
                                    ind_race_eth$subgroup == "B" ~ "Black or African American",
                                    ind_race_eth$subgroup == "H" ~ "Hispanic or Latino", 
                                    ind_race_eth$subgroup == "I" ~ "Native American",
                                    ind_race_eth$subgroup == "P" ~ "Hawaiian or Pacific Islander",
                                    ind_race_eth$subgroup == "W" ~ "White"))
    
    state_level = bind_rows(all, bhn, ed, el, swd, non_ed, non_el, non_swd, super, ind_race_eth) %>% 
        select(subgroup, avg_english = english, avg_math = math, avg_reading = reading, avg_science = science,
               avg_composite = composite, enrolled, tested, participation_rate, starts_with("valid_tests"),
               ends_with("cr_english"), ends_with("cr_math"), ends_with("cr_reading"), 
               ends_with("cr_science"), ends_with("cr_all"), n_21_or_higher, pct_21_or_higher,
               n_below_19, pct_below_19, n_female_21_or_higher, pct_female_21_or_higher, 
               n_male_21_or_higher, pct_male_21_or_higher, n_female_below_19, pct_female_below_19, 
               n_male_below_19, pct_male_below_19) %>% 
        arrange(subgroup)
}

## District
if(sys == T) {
    ### All
    all = student_level %>%
        group_by(system) %>% 
        summarize(subgroup = "All Students",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite)) %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & race_ethnicity %in% c("B", "H", "I")) %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### ED
    ed = student_level %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & econ_dis == "Y") %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### Non-ED
    non_ed = student_level %>% 
        filter(econ_dis == "N") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Non-Economically Disadvantaged",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & econ_dis == "N") %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>%
        ungroup()
    
    ### EL
    el = student_level %>% 
        filter(el == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "English Learners",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        full_join(student_level %>% 
                      filter(!is.na(composite) & el == "Y") %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite) %>% 
                      mutate(subgroup = "English Learners"), by = c("system", "subgroup"))
    
    ### Non-EL
    non_el = student_level %>% 
        filter(el == "N") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Non-English Learners",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & el == "N") %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### SWD
    swd = student_level %>% 
        filter(swd == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Students with Disabilities",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & swd == "Y") %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### Non-SWD
    non_swd = student_level %>% 
        filter(swd == "N") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Non-Students with Disabilities",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & swd == "N") %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### Super 
    super = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Super Subgroup",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  valid_tests_male = sum(gender == "M" & !is.na(composite)),
                  valid_tests_female = sum(gender == "F" & !is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
               pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
               participation_rate = round(100 * tested / enrolled, 0),
               pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
               pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
               pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
               pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
        mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
        bind_cols(student_level %>% 
                      filter(!is.na(composite) & (race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y")) %>% 
                      group_by(system) %>% 
                      summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite)) %>% 
        ungroup()
    
    ### Individual racial/ethnic groups
    race_eth_list = unique(student_level$race_ethnicity[!is.na(student_level$race_ethnicity)])
    ind_race_eth = data.frame()
    
    for(r in seq_along(race_eth_list)) {
        temp = student_level %>% 
            filter(race_ethnicity == race_eth_list[r]) %>% 
            group_by(system) %>% 
            summarize(subgroup = race_eth_list[r],
                      enrolled = n(),
                      tested = sum(!is.na(composite)),
                      valid_tests = sum(!is.na(composite)),
                      valid_tests_male = sum(gender == "M" & !is.na(composite)),
                      valid_tests_female = sum(gender == "F" & !is.na(composite)),
                      n_cr_english = sum(n_cr_english, na.rm = T),
                      n_cr_math = sum(n_cr_math, na.rm = T),
                      n_cr_reading = sum(n_cr_reading, na.rm = T),
                      n_cr_science = sum(n_cr_science, na.rm = T),
                      n_cr_all = sum(n_cr_all, na.rm = T),
                      n_21_or_higher = sum(composite >= 21, na.rm = T),
                      n_below_19 = sum(composite < 19, na.rm = T),
                      n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                      n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                      n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                      n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
            mutate(pct_cr_english = n_cr_english, pct_cr_math = n_cr_math, pct_cr_reading = n_cr_reading,
                   pct_cr_science = n_cr_science, pct_cr_all = n_cr_all, pct_21_or_higher = n_21_or_higher, pct_below_19 = n_below_19,
                   participation_rate = round(100 * tested / enrolled, 0),
                   pct_female_21_or_higher = round(100 * n_female_21_or_higher / valid_tests_female, 1),
                   pct_male_21_or_higher = round(100 * n_male_21_or_higher / valid_tests_male, 1),
                   pct_female_below_19 = round(100 * n_female_below_19 / valid_tests_female, 1),
                   pct_male_below_19 = round(100 * n_male_below_19 / valid_tests_male, 1)) %>% 
            mutate_each(funs(round(100 * . / valid_tests, 1)), starts_with("pct_cr"), pct_21_or_higher, pct_below_19) %>% 
            full_join(student_level %>% 
                          filter(!is.na(composite) & race_ethnicity == race_eth_list[r]) %>% 
                          group_by(system) %>% 
                          summarize_each(funs(round(mean(., na.rm = T), 1)), english, math, reading, science, composite) %>% 
                          mutate(subgroup = race_eth_list[r]), by = c("system", "subgroup")) %>%
            ungroup()
        
        ind_race_eth = bind_rows(ind_race_eth, temp)
    }
    
    ind_race_eth = ind_race_eth %>% 
        mutate(subgroup = case_when(ind_race_eth$subgroup == "A" ~ "Asian",
                                    ind_race_eth$subgroup == "B" ~ "Black or African American",
                                    ind_race_eth$subgroup == "H" ~ "Hispanic or Latino", 
                                    ind_race_eth$subgroup == "I" ~ "Native American",
                                    ind_race_eth$subgroup == "P" ~ "Hawaiian or Pacific Islander",
                                    ind_race_eth$subgroup == "W" ~ "White"))
    
    district_level = bind_rows(all, bhn, ed, el, swd, non_ed, non_el, non_swd, super, ind_race_eth) %>% 
        select(system, subgroup, avg_english = english, avg_math = math, avg_reading = reading, avg_science = science,
               avg_composite = composite, enrolled, tested, participation_rate, starts_with("valid_tests"),
               ends_with("cr_english"), ends_with("cr_math"), ends_with("cr_reading"), 
               ends_with("cr_science"), ends_with("cr_all"), n_21_or_higher, pct_21_or_higher,
               n_below_19, pct_below_19, n_female_21_or_higher, pct_female_21_or_higher, 
               n_male_21_or_higher, pct_male_21_or_higher, n_female_below_19, pct_female_below_19, 
               n_male_below_19, pct_male_below_19) %>% 
        arrange(system, subgroup)
}

## School
if(sch == T) {
    ### All
    all = student_level %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "All Students",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system)
    
    all2 = all %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(all2) = str_replace_all(names(all2), "n_", "pct_")
    
    all3 = student_level %>% 
        filter(!is.na(composite)) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "All Students",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
    
    all = full_join(all, all2, by = c("system", "school", "subgroup")) %>% 
        full_join(all3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    bhn2 = bhn %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(bhn2) = str_replace_all(names(bhn2), "n_", "pct_")
    
    bhn3 = student_level %>% 
        filter(!is.na(composite) & race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
    
    bhn = full_join(bhn, bhn2, by = c("system", "school", "subgroup")) %>% 
        full_join(bhn3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### ED
    ed = student_level %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    ed2 = ed %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(ed2) = str_replace_all(names(ed2), "n_", "pct_")
    
    ed3 = student_level %>% 
        filter(!is.na(composite) & econ_dis == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
    
    ed = full_join(ed, ed2, by = c("system", "school", "subgroup")) %>% 
        full_join(ed3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### Non-ED
    non_ed = student_level %>% 
        filter(econ_dis == "N") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Non-Economically Disadvantaged",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    non_ed2 = non_ed %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(non_ed2) = str_replace_all(names(non_ed2), "n_", "pct_")
    
    non_ed3 = student_level %>% 
        filter(!is.na(composite) & econ_dis == "N") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Non-Economically Disadvantaged",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
    
    non_ed = full_join(non_ed, non_ed2, by = c("system", "school", "subgroup")) %>% 
        full_join(non_ed3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### EL
    el = student_level %>% 
        filter(el == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "English Learners",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    el2 = el %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(el2) = str_replace_all(names(el2), "n_", "pct_")
    
    el3 = student_level %>% 
        filter(!is.na(composite) & el == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "English Learners",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
                  
    el = full_join(el, el2, by = c("system", "school", "subgroup")) %>% 
        full_join(el3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### Non-EL
    non_el = student_level %>% 
        filter(el == "N") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Non-English Learners",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    non_el2 = non_el %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(non_el2) = str_replace_all(names(non_el2), "n_", "pct_")
    
    non_el3 = student_level %>% 
        filter(!is.na(composite) & el == "N") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Non-English Learners",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
                  
    non_el = full_join(non_el, non_el2, by = c("system", "school", "subgroup")) %>% 
        full_join(non_el3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### SWD
    swd = student_level %>% 
        filter(swd == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Students with Disabilities",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    swd2 = swd %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(swd2) = str_replace_all(names(swd2), "n_", "pct_")
    
    swd3 = student_level %>% 
        filter(!is.na(composite) & swd == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Students with Disabilities",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
                  
    swd = full_join(swd, swd2, by = c("system", "school", "subgroup")) %>% 
        full_join(swd3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### Non-SWD
    non_swd = student_level %>% 
        filter(swd == "N") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Non-Students with Disabilities",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    non_swd2 = non_swd %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(non_swd2) = str_replace_all(names(non_swd2), "n_", "pct_")
    
    non_swd3 = student_level %>% 
        filter(!is.na(composite) & swd == "N") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Non-Students with Disabilities",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup()
    
    non_swd = full_join(non_swd, non_swd2, by = c("system", "school", "subgroup")) %>% 
        full_join(non_swd3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### Super 
    super = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Super Subgroup",
                  enrolled = n(),
                  tested = sum(!is.na(composite)),
                  valid_tests = sum(!is.na(composite)),
                  n_cr_english = sum(n_cr_english, na.rm = T),
                  n_cr_math = sum(n_cr_math, na.rm = T),
                  n_cr_reading = sum(n_cr_reading, na.rm = T),
                  n_cr_science = sum(n_cr_science, na.rm = T),
                  n_cr_all = sum(n_cr_all, na.rm = T),
                  n_21_or_higher = sum(composite >= 21, na.rm = T),
                  n_below_19 = sum(composite < 19, na.rm = T),
                  n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                  n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                  n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                  n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
        ungroup() %>% 
        arrange(system, school)
    
    super2 = super %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(super2) = str_replace_all(names(super2), "n_", "pct_")
    
    super3 = student_level %>% 
        filter(!is.na(composite) & (race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y")) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Super Subgroup",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
        ungroup() 
                  
    super = full_join(super, super2, by = c("system", "school", "subgroup")) %>% 
        full_join(super3, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ### Individual racial/ethnic groups
    race_eth_list = unique(student_level$race_ethnicity[!is.na(student_level$race_ethnicity)])
    ind_race_eth = data.frame()
    
    for(r in seq_along(race_eth_list)) {
        temp = student_level %>% 
            filter(race_ethnicity == race_eth_list[r]) %>% 
            group_by(system, school) %>% 
            summarize(subgroup = race_eth_list[r],
                      enrolled = n(),
                      tested = sum(!is.na(composite)),
                      valid_tests = sum(!is.na(composite)),
                      n_cr_english = sum(n_cr_english, na.rm = T),
                      n_cr_math = sum(n_cr_math, na.rm = T),
                      n_cr_reading = sum(n_cr_reading, na.rm = T),
                      n_cr_science = sum(n_cr_science, na.rm = T),
                      n_cr_all = sum(n_cr_all, na.rm = T),
                      n_21_or_higher = sum(composite >= 21, na.rm = T),
                      n_below_19 = sum(composite < 19, na.rm = T),
                      n_female_21_or_higher = sum(gender == "F" & composite >= 21, na.rm = T),
                      n_male_21_or_higher = sum(gender == "M" & composite >= 21, na.rm = T),
                      n_female_below_19 = sum(gender == "F" & composite < 19, na.rm = T),
                      n_male_below_19 = sum(gender == "M" & composite < 19, na.rm = T)) %>% 
            ungroup() %>% 
            arrange(system, school)
        
        temp2 = temp %>% 
            mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
            select(system, school, subgroup, starts_with("n_"))
        names(temp2) = str_replace_all(names(temp2), "n_", "pct_")
        
        temp3 = student_level %>% 
            filter(!is.na(composite) & race_ethnicity == race_eth_list[r]) %>% 
            group_by(system, school) %>% 
            summarize(subgroup = race_eth_list[r],
                      avg_english = round(mean(english, na.rm = T), 1),
                      avg_math = round(mean(math, na.rm = T), 1),
                      avg_reading = round(mean(reading, na.rm = T), 1),
                      avg_science = round(mean(science, na.rm = T), 1), 
                      avg_composite = round(mean(composite, na.rm = T), 1)) %>% 
            ungroup()
                      
        temp = full_join(temp, temp2, by = c("system", "school", "subgroup")) %>% 
            full_join(temp3, by = c("system", "school", "subgroup")) %>% 
            mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
            arrange(system, school)
        
        ind_race_eth = bind_rows(ind_race_eth, temp)
    }
    
    ind_race_eth = ind_race_eth %>% 
        mutate(subgroup = case_when(ind_race_eth$subgroup == "A" ~ "Asian",
                                    ind_race_eth$subgroup == "B" ~ "Black or African American",
                                    ind_race_eth$subgroup == "H" ~ "Hispanic or Latino", 
                                    ind_race_eth$subgroup == "I" ~ "Native American",
                                    ind_race_eth$subgroup == "P" ~ "Hawaiian or Pacific Islander",
                                    ind_race_eth$subgroup == "W" ~ "White"))
    
    ### Bind all rows together
    school_level = bind_rows(all, bhn, ed, el, swd, non_ed, non_el, non_swd, ind_race_eth) %>% 
        select(system, school, subgroup, starts_with("avg_"), enrolled, tested, participation_rate, valid_tests,
               ends_with("cr_english"), ends_with("cr_math"), ends_with("cr_reading"), 
               ends_with("cr_science"), ends_with("cr_all"), n_21_or_higher, pct_21_or_higher,
               n_below_19, pct_below_19, n_female_21_or_higher, pct_female_21_or_higher, 
               n_male_21_or_higher, pct_male_21_or_higher, n_female_below_19, pct_female_below_19, 
               n_male_below_19, pct_male_below_19) %>% 
        arrange(system, school, subgroup)
    
    ### Bind school, district, and state levels together 
    base = bind_rows(school_level, district_level, state_level) %>% 
        mutate_each(funs(ifelse(is.na(.), 0, .)), system, school) %>% 
        arrange(system, school, subgroup) %>% 
        left_join(mutate(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_school_crosswalk.dta"), system = as.integer(system), school = as.integer(school)),
                  by = c("system", "school")) %>% 
        select(starts_with("system"), starts_with("school"), everything()) %>% 
        mutate(school_name = ifelse(is.na(school_name) & school == 0, "All Schools", school_name),
               school_name = ifelse(is.na(school_name) & system == 61 & school == 40, "F.I. Denning Center of Technology and Careers", school_name),
               school_name = ifelse(is.na(school_name) & system == 190 & school == 740, "Johnson Alternative Learning Center", school_name),
               school_name = ifelse(is.na(school_name) & system == 580 & school == 75, "Central Prep Academy", school_name),
               school_name = ifelse(is.na(school_name) & system == 650 & school == 46, "Morgan County Career and Technical Center", school_name),
               school_name = ifelse(is.na(school_name) & system == 792 & school == 8275, "The Excel Center", school_name),
               school_name = ifelse(is.na(school_name) & system == 830 & school == 155, "Sumner County Middle College High School", school_name),
               school_name = ifelse(is.na(school_name) & system == 830 & school == 1010, "Sumner County Middle Technical College High School at Portland", school_name),
               school_name = ifelse(is.na(school_name) & system == 900 & school == 115, "Tennessee Virtual Learning Academy", school_name),
               school_name = ifelse(is.na(school_name) & system == 970 & school == 140, "DCS Affiliated Schools", school_name),
               school_name = ifelse(is.na(school_name) & system == 985 & school == 45, "Pathways in Education - TN", school_name), 
               school_name = ifelse(is.na(school_name) & system == 985 & school == 50, "Pathways in Education - Whitehaven", school_name), 
               school_name = ifelse(is.na(school_name) & system == 985 & school == 8055, "Fairley High School", school_name),
               school_name = ifelse(is.na(school_name) & system == 985 & school == 8065, "Martin Luther King Preparatory High School", school_name),
               school_name = ifelse(is.na(school_name) & system == 985 & school == 8140, "Hillcrest High School", school_name),
               system_name = ifelse(is.na(system_name), dendextend::na_locf(system_name), system_name),
               system_name = ifelse(system == 0, "State of Tennessee", system_name)) %>% 
        mutate_each(funs(ifelse(is.nan(.), NA, .)), starts_with("avg_"))
    
    base = replace(base, is.na(base), NA)
    
    ### Output file
    write_csv(base, "K:/ORP_accountability/data/2017_ACT/act_base_EK.csv", na = "")
}

## Check against Jessica's files
if(che == T) {
    ## Student level
    
    ## State level
    
    ## District level
    check = district_level %>% 
        mutate(system = as.numeric(system))
    check = mutate(check, subgroup = case_when(check$subgroup == "Hispanic or Latino" ~ "Hispanic",
                                        check$subgroup == "Hawaiian or Pacific Islander" ~ "HPI",
                                        check$subgroup == "English Learners" ~ "English Language Learners with T1/T2",
                                        check$subgroup == "Non-English Learners" ~ "Non-English Language Learners")) %>% 
        full_join(read_dta("K:/ORP_accountability/data/2017_ACT/ACT_district2018.dta"), by = c("system", "subgroup")) %>%
        # filter(abs(avg_english - english_avg) >= 0.1 | abs(avg_math - math_avg) >= 0.1 | 
        #            abs(avg_reading - reading_avg) >= 0.1 | abs(avg_science - science_avg) >= 0.1 | 
        #            abs(avg_composite - act_composite_avg) >= 0.1) %>% # seems to be a rounding issue
        filter(is.na(avg_english) & !is.na(english_avg)) %>% 
        select(system, subgroup, contains("avg"))
    
        
    ## School
}
