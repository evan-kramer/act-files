# ACT Files with Subgroups
# Evan Kramer
# 6/13/2017

library(tidyverse)
library(lubridate)
library(stringr)
library(haven)

rm(list = ls())
date = str_replace_all(as.character(Sys.Date()), "-", "")
setwd("K:/ORP_accountability/data/2016_ACT")

sch = T
sys = T

# Read in student-level file
act = read_dta("act_student_level_EK.dta") %>% 
    mutate(enrolled = 1,
           tested = !is.na(composite),
           valid_tests = tested == T)

# Calculate subgroup performance and participation rates - school level
if(sch == T) {
    all = act %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "All Students",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    all2 = all %>% 
        mutate_each(funs(ifelse(valid_tests < 30, NA, round(100 * . / valid_tests, 1))), starts_with("n_")) %>% 
        select(system, school, subgroup, starts_with("n_"))
    names(all2) = str_replace_all(names(all2), "n_", "pct_")
    
    all = full_join(all, all2, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ## BHN
    bhn = act %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    bhn = full_join(bhn, bhn2, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ## ED
    ed = act %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    ed = full_join(ed, ed2, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ## EL 
    el = act %>% 
        filter(ell == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "English Learners",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    el = full_join(el, el2, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ## SWD
    swd = act %>% 
        filter(sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(subgroup = "Students with Disabilities",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    swd = full_join(swd, swd2, by = c("system", "school", "subgroup")) %>% 
        mutate(participation_rate = ifelse(enrolled < 30, NA, round(100 * tested / enrolled, 0))) %>% 
        arrange(system, school)
    
    ## Bind all rows together
    act_sch = bind_rows(all, bhn, ed, el, swd) %>% 
        select(system, school, subgroup, starts_with("avg_"), enrolled, tested, participation_rate, valid_tests,
               ends_with("cr_english"), ends_with("cr_math"), ends_with("cr_reading"), 
               ends_with("cr_science"), ends_with("cr_all"), n_21_or_higher, pct_21_or_higher,
               n_below_19, pct_below_19, n_female_21_or_higher, pct_female_21_or_higher, 
               n_male_21_or_higher, pct_male_21_or_higher, n_female_below_19, pct_female_below_19, 
               n_male_below_19, pct_male_below_19) %>% 
        mutate_each(funs(round(., 1)), starts_with("avg_")) %>% 
    arrange(system, school, subgroup)
    
    rm(all, bhn, ed, el, swd, all2, bhn2, ed2, el2, swd2)    
}

# Calculate subgroup performance and participation rates - district level
if(sys == T) {
    all = act %>% 
        group_by(system) %>% 
        summarize(subgroup = "All Students",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    ## BHN
    bhn = act %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system) %>% 
        summarize(subgroup = "Black/Hispanic/Native American",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    ## ED
    ed = act %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Economically Disadvantaged",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    ## EL 
    el = act %>% 
        filter(ell == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "English Learners",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    ## SWD
    swd = act %>% 
        filter(sped == "Y") %>% 
        group_by(system) %>% 
        summarize(subgroup = "Students with Disabilities",
                  avg_english = mean(english, na.rm = T),
                  avg_math = mean(math, na.rm = T),
                  avg_science = mean(science, na.rm = T),
                  avg_composite = mean(composite, na.rm = T),
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
    
    ## Bind all rows together
    act_sys = bind_rows(all, bhn, ed, el, swd) %>% 
        select(system, subgroup, starts_with("avg_"), enrolled, tested, participation_rate, valid_tests,
               ends_with("cr_english"), ends_with("cr_math"), ends_with("cr_reading"), 
               ends_with("cr_science"), ends_with("cr_all"), n_21_or_higher, pct_21_or_higher,
               n_below_19, pct_below_19, n_female_21_or_higher, pct_female_21_or_higher, 
               n_male_21_or_higher, pct_male_21_or_higher, n_female_below_19, pct_female_below_19, 
               n_male_below_19, pct_male_below_19) %>% 
        mutate_each(funs(round(., 1)), starts_with("avg_")) %>% 
        arrange(system, subgroup)
    
    rm(all, bhn, ed, el, swd, all2, bhn2, ed2, el2, swd2)
}

# Calculate subgroup performance and participation rates - school and district level
if(sch == T & sys == T) {
    act_all = bind_rows(act_sys, act_sch) %>% 
        mutate(school = ifelse(is.na(school), 0, school)) %>% 
        arrange(system, school, subgroup) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_school_crosswalk.dta"),
                  by = c("system", "school")) %>% 
        select(starts_with("system"), starts_with("school"), everything()) %>% 
        mutate(school_name = ifelse(is.na(school_name) & school == 0, "All Schools", school_name),
               school_name = ifelse(is.na(school_name) & system == 61 & school == 40, "F.I. Denning Center of Technology and Careers", school_name),
               school_name = ifelse(is.na(school_name) & system == 580 & school == 75, "Central Prep Academy", school_name),
               school_name = ifelse(is.na(school_name) & system == 650 & school == 46, "Morgan County Career and Technical Center", school_name),
               school_name = ifelse(is.na(school_name) & system == 792 & school == 8275, "The Excel Center", school_name),
               school_name = ifelse(is.na(school_name) & system == 830 & school == 155, "Sumner County Middle College High School", school_name),
               school_name = ifelse(is.na(school_name) & system == 830 & school == 1010, "Sumner County Middle Technical College High School at Portland", school_name),
               school_name = ifelse(is.na(school_name) & system == 900 & school == 115, "Tennessee Virtual Learning Academy", school_name),
               school_name = ifelse(is.na(school_name) & system == 985 & school == 45, "Pathways in Education - TN", school_name), 
               school_name = ifelse(is.na(school_name) & system == 985 & school == 50, "Pathways in Education - Whitehaven", school_name), 
               school_name = ifelse(is.na(school_name) & system == 985 & school == 8055, "Fairley High School", school_name),
               school_name = ifelse(is.na(school_name) & system == 985 & school == 8065, "Martin Luther King Preparatory High School", school_name),
               system_name = ifelse(is.na(system_name), dendextend::na_locf(system_name), system_name)) %>% 
        mutate_each(funs(ifelse(is.nan(.), NA, .)), starts_with("avg_"))
}

# Output file(s)
if((sch == T & sys == T) | "act_all" %in% ls() == T) {
    write_excel_csv(act_all, "act_district_school_level_with_subgroups_EK.csv", na = "")
}