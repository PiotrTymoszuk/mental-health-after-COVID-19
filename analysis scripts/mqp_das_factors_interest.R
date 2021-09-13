# This script investigates the scoring of MQP and DAS as a function of the sum of persistent psychosomatic symptoms,
# persistent neurocognitive symptoms and sex


# data containers -----

  mqp_das_factors <- list()

# globals: variables of interest and variable lexicons -----

  insert_msg('Globals setup')

  ## lexicons: prevalence and symptom counts

  mqp_das_factors$count_var_lexicon <- globals$var_lexicon %>% 
    filter(variable %in% rforest$response)
  
  ## split variables
  
  mqp_das_factors$split_var <- c('stress_score_class', 
                                 'sum_symptoms_acute_class', 
                                 'sum_symptoms_long_class', 
                                 'neurocognitive_acute_sympt_class', 
                                 'neurocognitive_long_sympt_class', 
                                 'sex', 
                                 'depression_burnout')
  
  ## labellers
  
  mqp_das_factors$labeller <- list(NULL, 
                                   NULL, 
                                   NULL, 
                                   NULL, 
                                   NULL, 
                                   c('female' = 'Female', 
                                     'male' = 'Male'), 
                                   NULL)
  
  ## split var colors
  
  mqp_das_factors$split_var_colors <- globals$var_lexicon %>% 
    filter(variable %in% mqp_das_factors$split_var)
  
  mqp_das_factors$split_var_colors <- set_names(mqp_das_factors$split_var_colors$level_colors, 
                                                mqp_das_factors$split_var_colors$variable)[mqp_das_factors$split_var]

# Analyses of the mental health score -----
  
  insert_msg('Analysis of the DAS and MQP scoring as a function of psychosomatic, neurocognitive symptoms ans sex')
  
  mqp_das_factors[mqp_das_factors$split_var] <- list(split_var = mqp_das_factors$split_var, 
                                                     labeller = mqp_das_factors$labeller, 
                                                     numeric_colors = mqp_das_factors$split_var_colors, 
                                                     numeric_test = c(rep('kruskal', 5), rep('u', 2)), 
                                                     x_lab = translate_var(mqp_das_factors$split_var)) %>% 
    pmap(analyze_split, 
         inp_data_list = cov_data, 
         var_lexicon = mqp_das_factors$count_var_lexicon, 
         violin = T, 
         show_points = F, 
         box_alpha = 1, 
         numeric_y_lab = 'score', 
         point_alpha = 0.25)
  
# END ------
  
  insert_tail()