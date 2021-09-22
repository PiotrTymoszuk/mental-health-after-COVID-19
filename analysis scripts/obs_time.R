# This script investigates the scoring of MQP and DAS as a function of the observation time

# data containers -----

  obs_time <- list()

# globals: variables of interest and variable lexicons -----

  insert_msg('Globals setup')

  ## lexicons: prevalence and symptom counts

  obs_time$count_var_lexicon <- globals$var_lexicon %>% 
    filter(variable %in% rforest$response)
  

# Analyses of the mental health score -----
  
  insert_msg('Analysis of the DAS and MQP scoring as a function of psychosomatic, neurocognitive symptoms ans sex')
  
  obs_time$analyses <- analyze_split(inp_data_list = cov_data, 
                                     var_lexicon = obs_time$count_var_lexicon, 
                                     violin = T, 
                                     show_points = F, 
                                     box_alpha = 1, 
                                     numeric_y_lab = 'score', 
                                     point_alpha = 0.25,
                                     split_var = 'obs_time_strata', 
                                     labeller = c('up to 60 days' = '28 - 60', 
                                                  '61 - 120 days' = '61 - 120', 
                                                  '121 - 180 days' = '121 - 180', 
                                                  'more than 180 days' = '> 180'), 
                                     numeric_test = 'kruskal', 
                                     numeric_colors = globals$var_lexicon %>% 
                                       filter(variable == 'obs_time_strata') %>% 
                                       .$level_colors, 
                                     x_lab = 'Observation time, days', 
                                     show_tag = F)

  
# END ------
  
  insert_tail()