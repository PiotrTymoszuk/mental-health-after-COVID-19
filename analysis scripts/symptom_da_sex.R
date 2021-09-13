# This script investigates the prevalence and sum of acute and persistent symptoms
# in the pre_cov depression/anxiety and sex


# data containers -----

  sympt_analyses <- list()

# globals: variables of interest and variable lexicons -----

  insert_msg('Globals setup')

  ## lexicons: prevalence and symptom counts

  sympt_analyses$count_var_lexicon <- globals$var_lexicon %>% 
    filter(variable %in% c('sum_symptoms_acute', 
                           'sum_symptoms_long', 
                           #'psychosom_acute_sympt_sum', 
                           #'psychosom_long_sympt_sum', 
                           'neurocognitive_acute_sympt_sum', 
                           'neurocognitive_long_sympt_sum'))
  
  ## split variables
  
  sympt_analyses$split_var <- c('depression_burnout', 
                                'sex')
  
  ## labellers
  
  sympt_analyses$labeller <- list(c('no' = 'DA-', 
                                    'yes' = 'DA+'), 
                                  c('female' = 'Female', 
                                    'male' = 'Male'))
  
  ## split var colors
  
  sympt_analyses$split_var_colors <- globals$var_lexicon %>% 
    filter(variable %in% sympt_analyses$split_var)
  
  sympt_analyses$split_var_colors <- set_names(sympt_analyses$split_var_colors$level_colors, 
                                               sympt_analyses$split_var_colors$variable)[sympt_analyses$split_var]

# Analyses of the acute and persistent symptom counts -----
  
  insert_msg('Analysis of the acute and persistent symptom count in the depression, sex and age strata')
  
  sympt_analyses[c('da', 'sex')] <- list(split_var = sympt_analyses$split_var, 
                                         labeller = sympt_analyses$labeller, 
                                         numeric_colors = sympt_analyses$split_var_colors, 
                                         numeric_test = c('u', 'u'), 
                                         point_alpha = c(0.5, 0.25), 
                                         x_lab = translate_var(sympt_analyses$split_var)) %>% 
    pmap(analyze_split, 
         inp_data_list = cov_data, 
         var_lexicon = sympt_analyses$count_var_lexicon, 
         violin = T, 
         show_points = F, 
         box_alpha = 1)

# END ------
  
  insert_tail()