# ACT Highest Score File
# Evan Kramer
# 9/20/2017

library(tidyverse)
library(lubridate)
library(haven)
library(stringr)

# Markers
date = str_c(day(today()), month(today(), label = T), year(today()))
sys = T
sch = F 

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
    mutate(n_cr_english = as.numeric(english >= 18),
           n_cr_math = as.numeric(math >= 22), 
           n_cr_reading = as.numeric(reading >= 22),
           n_cr_science = as.numeric(science >= 23),
           n_cr_all = as.numeric(n_cr_english == 1 & n_cr_math == 1 & n_cr_reading == 1 & n_cr_science == 1))
    
student_level = replace(student_level, is.na(student_level), NA)

# Output student level file
output = student_level %>% 
    select(system, school, state_stud_id = student_key, last_name, first_name, 
              english, math, reading, science, composite, gender, n_cr_english,
              n_cr_math, n_cr_reading, n_cr_science, n_cr_all)
#write_csv(output, "K:/ORP_accountability/data/2017_ACT/act_cohort_student_level_EK.csv", na = "")

# Collapse to district and school 
## District
if(sys == T) {
    ### All
    all = student_level %>% 
        group_by(system) %>% 
        summarize(subgroup = "All Students",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
        select(system, subgroup, starts_with("n_"))
    names(all2) = str_replace_all(names(all2), "n_", "pct_")
    
    all = full_join(all, all2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### BHN
    bhn = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    bhn2 = bhn %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(bhn2) = str_replace_all(names(bhn2), "n_", "pct_")
    
    bhn = full_join(bhn, bhn2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### ED
    ed = student_level %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    ed2 = ed %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(ed2) = str_replace_all(names(ed2), "n_", "pct_")
    
    ed = full_join(ed, ed2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### Non-ED
    non_ed = student_level %>% 
        filter(econ_dis == "N") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Non-Economically Disadvantaged",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    non_ed2 = non_ed %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(non_ed2) = str_replace_all(names(non_ed2), "n_", "pct_")
    
    non_ed = full_join(non_ed, non_ed2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### EL
    el = student_level %>% 
        filter(el == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "English Learners",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    el2 = el %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(el2) = str_replace_all(names(el2), "n_", "pct_")
    
    el = full_join(el, el2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### Non-EL
    non_el = student_level %>% 
        filter(el == "N") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Non-English Learners",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    non_el2 = non_el %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(non_el2) = str_replace_all(names(non_el2), "n_", "pct_")
    
    non_el = full_join(non_el, non_el2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### SWD
    swd = student_level %>% 
        filter(swd == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Students with Disabilities",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    swd2 = swd %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(swd2) = str_replace_all(names(swd2), "n_", "pct_")
    
    swd = full_join(swd, swd2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### Non-SWD
    non_swd = student_level %>% 
        filter(swd == "N") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Non-Students with Disabilities",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    non_swd2 = non_swd %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(non_swd2) = str_replace_all(names(non_swd2), "n_", "pct_")
    
    non_swd = full_join(non_swd, non_swd2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### Super 
    super = student_level %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | el == "Y" | swd == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Super Subgroup",
                  avg_english = round(mean(english, na.rm = T), 1),
                  avg_math = round(mean(math, na.rm = T), 1),
                  avg_reading = round(mean(reading, na.rm = T), 1),
                  avg_science = round(mean(science, na.rm = T), 1), 
                  avg_composite = round(mean(composite, na.rm = T), 1),
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
    
    super2 = super %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, subgroup, starts_with("n_"))
    names(super2) = str_replace_all(names(super2), "n_", "pct_")
    
    super = full_join(super, super2, by = c("system", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system)
    
    ### Individual racial/ethnic groups
    race_eth_list = unique(student_level$race_ethnicity[!is.na(student_level$race_ethnicity)])
    ind_race_eth = data.frame()
    
    for(r in seq_along(race_eth_list)) {
        temp = student_level %>% 
            filter(race_ethnicity == race_eth_list[r]) %>% 
            group_by(system) %>% 
            summarize(subgroup = race_eth_list[r],
                      avg_english = round(mean(english, na.rm = T), 1),
                      avg_math = round(mean(math, na.rm = T), 1),
                      avg_reading = round(mean(reading, na.rm = T), 1),
                      avg_science = round(mean(science, na.rm = T), 1), 
                      avg_composite = round(mean(composite, na.rm = T), 1),
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
        
        temp2 = temp %>% 
            mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
            select(system, subgroup, starts_with("n_"))
        names(temp2) = str_replace_all(names(temp2), "n_", "pct_")
        
        temp = full_join(temp, temp2, by = c("system", "subgroup")) %>% 
            mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
            arrange(system)
        
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
    district_level = bind_rows(all, bhn, ed, el, swd, non_ed, non_el, non_swd, ind_race_eth) %>% 
        select(system, subgroup, starts_with("avg_"), enrolled, tested, participation_rate, valid_tests,
               ends_with("cr_english"), ends_with("cr_math"), ends_with("cr_reading"), 
               ends_with("cr_science"), ends_with("cr_all"), n_21_or_higher, pct_21_or_higher,
               n_below_19, pct_below_19, n_female_21_or_higher, pct_female_21_or_higher, 
               n_male_21_or_higher, pct_male_21_or_higher, n_female_below_19, pct_female_below_19, 
               n_male_below_19, pct_male_below_19) %>% 
        arrange(system, subgroup)
    
    ### Output file
    #write_csv(district_level, "K:/ORP_accountability/data/2017_ACT/act_district_level_EK.csv", na = "")
    #rm(list = ls())
}

## School
if(sch == T) {
    ### All
    ### BHN
    ### ED
    ### Non-ED
    ### EL
    ### Non-EL
    ### SWD
    ### Non-SWD
    ### Super
    ### Individual racial/ethnic groups    
}

