# This script investigates how sex, the number of persistent psychosomatic symptoms
# and of persistent neurocognitive symptoms impacts on the depression, anxiety, stress
# and DA PHQ positivity

# data containers -----

  das_prevalnce <- list()

# globals: variables of interest and variable lexicons -----

  insert_msg('Globals setup')

  ## lexicons: prevalence

  das_prevalnce$count_var_lexicon <- globals$var_lexicon %>% 
    filter(variable %in% c('phq_depression_positive', 
                           'phq_anxiety_positive', 
                           'da_positive'))

  ## split variables

  das_prevalnce$split_var <- c('stress_score_class',
                               'sum_symptoms_acute_class', 
                               'sum_symptoms_long_class', 
                               #'psychosom_acute_sympt_class', 
                               #'psychosom_long_sympt_class', 
                               'neurocognitive_acute_sympt_class', 
                               'neurocognitive_long_sympt_class')


  ## split var colors
  
  das_prevalnce$split_var_colors <- globals$var_lexicon %>% 
    filter(variable %in% mqp_das_factors$split_var)
  
  das_prevalnce$split_var_colors <- set_names(das_prevalnce$split_var_colors$level_colors, 
                                              das_prevalnce$split_var_colors$variable)[das_prevalnce$split_var]

# Analyses of the mental health score -----
  
  insert_msg('Analysis of the DA prevalence as a function of psychosomatic, neurocognitive symptoms and sex')
  
  das_prevalnce[das_prevalnce$split_var] <- list(split_var = das_prevalnce$split_var, 
                                                 x_lab = translate_var(das_prevalnce$split_var)) %>% 
    pmap(analyze_split, 
         inp_data_list = cov_data, 
         var_lexicon = das_prevalnce$count_var_lexicon, 
         labeller = NULL)
  
# Generating a table with the prevalence of PHQ depression and anxiety as a function of the variables of interest ------
  
  insert_msg('A result table')
  
  das_prevalnce$result_tbl <- das_prevalnce[das_prevalnce$split_var] %>% 
    map(make_prev_tbl) %>% 
    map2_dfr(., names(.), 
             function(x, y) mutate(x, variable = y))

# END ------
  
  insert_tail()