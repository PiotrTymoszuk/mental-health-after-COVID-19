# This script performs explorative data analysis of the cohort data. The tool of choice is the in-house-developed 
# ExDaA package. The analysis encompasses two steps:
# 1) Visual analysis and statistical testing of the distribution of the most important numeric study variables
# 2) Comparison of the baseline, CoV and post-CoV mental health status comparison between the AT and IT cohorts.
# 3) Comparison of the baseline, CoV and post-CoV mental health status comparison between the mental-healthy 
# and depression/anoxiety subjects.

  insert_head()

# tools -----

  library(exda) ## available from https://github.com/PiotrTymoszuk/ExDA
  library(rlang)
  
# some globals ------
  
  eda_globals <- list()
  
  eda_globals$globals_tbl <- read_excel('./input data/eda_globals.xlsx')
  
  eda_globals$analysis_tbl <- cov_data %>% 
    map(select, 
        all_of(eda_globals$globals_tbl$variable), 
        depression_burnout) %>% 
    map(mutate, 
        depression_burnout = car::recode(depression_burnout, "'no' = 'DA-'; 'yes' = 'DA+'"))
  
# analysis scripts ------
  
  c('./exploration scripts/distribution.R', 
    './exploration scripts/cohort_comparison.R') %>% 
    source_all(message = TRUE, 
               crash = TRUE)
  
# END -----
  
  insert_tail()