# This script estimates co-occurrence of pre-CoV depression/anxiety and sleep disorders
# as well as mental health features following COVID-19

  insert_head()

# tools ----
  
  library(gmodels)
  library(vcd)

# data container -----
  
  overlap <- list()
  
# globals: variable, labels and analysis table -----
  
  insert_head('Globals setup')
  
  ## variables
  
  overlap$pre_vars <- c('depression_burnout', 
                        'insomnia') ## pre-existing
  
  overlap$mental_vars <- c('phq_depression_positive', 
                           'phq_anxiety_positive',
                           'life_quality', 
                           'mental_health')
  
  overlap$mental_pairs <- overlap$mental_vars %>% 
    combn(m = 2, 
          simplify = F) %>% 
    transpose %>% 
    map(unlist) %>% 
    set_names(c('variable1', 
                'variable2'))
  
  overlap$mental_labs <- c('phq_depression_positive' = 'DPR+', 
                           'phq_anxiety_positive' = 'ANX+',
                           'life_quality' = 'poor QoL', 
                           'mental_health' = 'poor OMH')
  
  # analysis table
  
  overlap$analysis_tbl <- cov_data %>% 
    map(select, 
        all_of(overlap$pre_vars), 
        all_of(overlap$mental_vars)) %>% 
    map(mutate, 
        life_quality = ifelse(life_quality %in% c('poor', 'fair'), 
                             'yes', 'no') %>% 
          factor, 
        mental_health = ifelse(mental_health %in% c('poor', 'fair'), 
                             'yes', 'no') %>% 
          factor)
  
# serial analysis -----
  
  insert_msg('serial overlap analysis')
  
  ## pre-existing conditions
  
  overlap$pre_existing <- overlap$analysis_tbl %>% 
    map_dfr(get_kappa,
            variable1 = overlap$pre_vars[1], 
            variable2 = overlap$pre_vars[2]) %>% 
    mutate(cohort = names(overlap$analysis_tbl))
  
  ## mental features following COVID-19
  
  overlap$mental_features <- overlap$analysis_tbl %>% 
    map(function(x) overlap$mental_pairs %>% 
          pmap_dfr(get_kappa, 
                   data = x) %>% 
          mutate(p_adj = p.adjust(p_value, 'BH'))) %>% 
    map2(., names(.), 
         function(x, y) mutate(x, cohort = y))

  
# plotting as a heat-map -----
  
  insert_msg('Plotting the kappas as a heat map')
  
  overlap$plots <- list(kappa_table = overlap$mental_features, 
                        plot_title = globals$cohort_labs) %>% 
    pmap(plot_kappa)
  
# END -----
  
  insert_tail()
  
  
  
  