# ACT State Day Missing IDs
# Evan Kramer
# 12/14/2017

library(tidyverse)
library(lubridate)
library(haven)
library(stringr)

# Read data
a = readxl::read_excel("K:/Assessment_Data Returns/ACT/2016-17/Junior Day File/20170921_ACT_JuniorDayResults_SY2016-17_Whalen_v1.xlsx")
x = readxl::read_excel("K:/Assessment_Data Returns/ACT/2016-17/Final Combined/TN Crosswalk - Spring 2017.xlsx")

j = separate(x, `Local Site Code`, into = c("system", "school"), sep = " ") %>% 
    transmute_at(vars(`ACT Organization Code`, system, school), funs(as.numeric(.))) %>% 
    filter(!is.na(`ACT Organization Code`)) %>% 
    right_join(a, by = c("ACT Organization Code" = "acthscode")) %>% 
    group_by(system) %>% 
    summarize(n = n(), n_missing_id = sum(is.na(state_stud_id))) %>% 
    ungroup() %>% 
    mutate(pct = round(100 * n_missing_id / n, 1)) %>% 
    arrange(desc(pct))
    
#o = filter(a, is.na(state_stud_id))[1:20, ]
#write_csv(o, "C:/Users/CA19130/Downloads/act_junior_day_missing_ids.csv", na = "")
