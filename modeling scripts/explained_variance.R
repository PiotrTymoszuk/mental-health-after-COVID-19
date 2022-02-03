# Calculates percent explained variance by the common factors identified by Random Forests.

  insert_head()
  
# container list ------
  
  expl_var <- list()
  
# Generating the models for the whole cohort and the DA subsets -----
  
  insert_msg('Generating the full models')
  
  expl_var$models <- cov_data %>% 
    map(function(cohort) globals$response %>% 
          map(~make_lm(response = .x, 
                       indep_variable = rforest$cmm_factors, 
                       mod_fun = glm, 
                       family = 'poisson', 
                       data = cohort, 
                       weight_variable = 'freq_weight')) %>%  
          set_names(globals$response))
  
  expl_var$models_da <- cov_data %>% 
    map(filter, depression_burnout == 'yes') %>% 
    map(function(cohort) globals$response %>% 
          map(~make_lm(response = .x, 
                       indep_variable = rforest$cmm_factors[rforest$cmm_factors != 'depression_burnout'], 
                       mod_fun = glm, 
                       family = 'poisson', 
                       data = cohort, 
                       weight_variable = 'freq_weight')) %>%  
          set_names(globals$response))
  
# Fraction explained variance: R squared ------
  
  insert_msg('R squared')
  
  expl_var$fit_stats <- expl_var$models %>% 
    map(~map_dfr(.x, summary, 'fit'))
  
  expl_var$fit_stats_da <- expl_var$models_da %>% 
    map(~map_dfr(.x, summary, 'fit'))

# Fraction explained deviance associated with the model paramaters ----
  
  insert_msg('Fraction explained deviance')
  
  ## table
  
  expl_var$frac_dev <- expl_var$models %>% 
    map(~map(.x, anova))
  
  expl_var$frac_dev_da <- expl_var$models_da %>% 
    map(~map(.x, anova))
  
  ## plot
  
  expl_var$frac_dev_plot <- list(lm_analysis_object_north = expl_var$models$north, 
                                 lm_analysis_object_south = expl_var$models$south, 
                                 plot_title = globals$response_labels) %>% 
    pmap(plot_frac_dev)
  
  expl_var$frac_dev_plot_da <- list(lm_analysis_object_north = expl_var$models_da$north, 
                                    lm_analysis_object_south = expl_var$models_da$south, 
                                    plot_title = paste0(globals$response_labels, ', DA+')) %>% 
    pmap(plot_frac_dev)

# END -----
  
  insert_tail()