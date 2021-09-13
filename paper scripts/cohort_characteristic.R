# This script analyzes data for the tables with the cohort characteristic: baseine characteristic, COVID-19 course
# and psychosocial features

  insert_head()
  
# data containers -----
  
  cohort_features <- list()
  
# globals: features to be analyzed for the tables ----
  
  insert_msg('Globals setup')
  
  ## variables to be shown in the table
  
  cohort_features[c('baseline_vars', 
                    'course_vars', 
                    'psych_vars')] <- c('cohort_characteristic', 
                                        'course_characteristic', 
                                        'psych_characteristic') %>% 
    map(function(x) filter(globals$var_lexicon, 
                           .data[[x]] != 'no') %>% 
          select(variable, 
                 all_of(x)) %>% 
          arrange(as.numeric(.data[[x]])) %>% 
          .$variable)
  
# serial analyses -----
  
  insert_msg('Serial analyses')
  
  cohort_features$analyses <- cohort_features[c('baseline_vars', 
                                                'course_vars', 
                                                'psych_vars')] %>% 
    map(function(x) x %>% 
          map(analyze_feature, 
              inp_tbl = cov_data %>% 
                reduce(rbind), 
              split_var = 'cohort'))
  
# stitching together in a table ------
  
  insert_msg('Generating tables')
  
  cohort_features$tables <- cohort_features$analyses %>% 
    map(get_feature_summary)
  
# selecting the tables to show in the main text and in the supplements -----
  
  insert_msg('Selecting the variables to be shown in the main tables and in the supplements')
  
  ## baseline
  
  cohort_features$main_tables$baseline_vars <- cohort_features$tables$baseline_vars %>% 
    filter(!variable %in% c('obs_time', 
                            'obs_time_strata', 
                            'completion_season', 
                            'region_class', 
                            'language', 
                            'employment_sector',
                            'diabetes', 
                            'gastrointenstinal', 
                            'malignancy', 
                            'frequent_flu_like', 
                            'two_plus_infections_antibiotics'))
    
  cohort_features$supplements$baseline_vars <- cohort_features$tables$baseline_vars %>% 
    filter(variable %in% c('obs_time', 
                           'obs_time_strata', 
                           'completion_season', 
                           'region_class', 
                           'language', 
                           'employment_sector',
                           'diabetes', 
                           'gastrointenstinal', 
                           'malignancy', 
                           'frequent_flu_like', 
                           'two_plus_infections_antibiotics'))
  
  ## disease course
  
  cohort_features$main_tables$course_vars <- cohort_features$tables$course_vars %>% 
    filter(!variable %in% c('hair_loss', 
                           'weight_loss_class', 
                           'perf_impairment_class', 
                           'complete_covelescence', 
                           'rehabilitation_fup_needed'))
  
  cohort_features$supplements$course_vars <- cohort_features$tables$course_vars %>% 
    filter(variable %in% c('hair_loss', 
                           'weight_loss_class', 
                           'perf_impairment_class', 
                           'complete_covelescence', 
                           'rehabilitation_fup_needed'))
  
# END ----
  
  insert_tail()

  
  
  
  