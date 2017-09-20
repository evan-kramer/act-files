# Split ACT Retake Files
# Evan Kramer
# 5/31/2016

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(haven)
library(readxl)

setwd("K:/ORP_accountability/data/2016_ACT/ACT retake")
out = "K:/ORP_accountability/data/2016_accountability/District Level Files/"

# System level
a = read_excel("act_retake_system_level_formatted_jp_FINAL.xls")
sys_list = unique(a$`District Number`)
for(i in seq_along(sys_list)) {
    b = a  %>% filter(`District Number` == sys_list[i])
    write_excel_csv(b, paste0(out, "ACT Retake District Level/", sys_list[i], "_DistrictLevelACTRetake_31may2017.csv"), na = "")
}

# School level
a = read_excel("act_retake_school_level_formatted.xls")
sys_list = unique(a$`District Number`)
for(i in seq_along(sys_list)) {
    b = a  %>% filter(`District Number` == sys_list[i])
    write_excel_csv(b, paste0(out, "ACT Retake School Level/", sys_list[i], "_SchoolLevelACTRetake_31may2017.csv"), na = "")
}

# Student level
a = read_excel("student_level_retakefile.xls")
sys_list = unique(a$system) # given the unmerged records, are we worried about putting out a student-level file?
for(i in seq_along(sys_list)) {
    b = a  %>% filter(system == sys_list[i])
    write_excel_csv(b, paste0(out, "ACT Retake Student Level/", sys_list[i], "_StudentLevelACTRetake_31may2017.csv"), na = "")
}

