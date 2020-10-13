## STATS506 Week 6 In-Class Acitivities
## Group 2: EunSeon Ahn, Zhihao Xu, Yingyi Yang, Dongyang Zhao
## Description: 
##     Part 1. Data Prep of NHANES Data
##     Part 2: Constructing table/conducting marginal tests

# library --------------------------------------------------------------------
library(tidyverse)

# Read in data files
demo = read_delim('nhanes_demo.csv',delim = ",") 
ohxden = read_delim('nhanes_ohxden.csv',delim = ",") 

# Merge variable OHDDESTS to demographics data
demo_joined = ohxden %>% select(SEQN, OHDDESTS) %>%
  left_join(demo, by = "SEQN")

# Create clean data set w/ new variables
demo_joined = demo_joined %>% 
  select(id = SEQN, gender = RIAGENDR, age = RIDAGEYR,
              exam_status = RIDSTATR, ohx_status = OHDDESTS, DMDEDUC2) %>%
  mutate(under20 = age < 20, college = FALSE) %>% 
  mutate(college = ifelse(age  > 20 & DMDEDUC2 == 4,
                          "college", "no college")) %>% 
  mutate(college = as.factor(college)) %>% select(-DMDEDUC2)

# Add ohx variable with status of the exams
demo_joined = demo_joined %>%
  mutate(ohx = ifelse(exam_status == 2 & ohx_status == 1, 
                      "complete", "missing")) %>% mutate(ohx = as.factor(ohx))

# Construct table (NOT COMPLETED -- currently wrong)
tab = demo_joined %>%
  group_by(ohx) %>% summarize(under20 = n(), gender = n(), college = n())
