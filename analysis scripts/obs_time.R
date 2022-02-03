# Investigates the scoring of MQP and DAS as a function of the observation time

  insert_head()

# data containers -----

  obs_time <- list()
  
  obs_time$formulas <- globals$response %>% 
    map(~paste(.x, '~ s(obs_time, bs = "cs")')) %>% 
    map(as.formula) %>% 
    set_names(globals$response)
  
# GAM modeling ------
  
  insert_msg('GAM modeling')

  obs_time$test_results <- model_gam(data_list = cov_data, 
                                     formulas = obs_time$formulas, 
                                     family = 'poisson') %>% 
    dlply(.(cohort))
  
# Plots -----
  
  insert_msg('Plotting the correlations')
  
  obs_time$plots <- list(cohort = cov_data,
                       fill_color = globals$cohort_colors, 
                       cohort_lab = globals$cohort_labs, 
                       stats = map(obs_time$test_results, 
                                   ~.x[['plot_sub']])) %>% 
    pmap(function(cohort, fill_color, stats, cohort_lab) globals$response %>% 
           map(~plot_correlation(data = cohort, 
                                 variables = c('obs_time', .x), 
                                 type = 'correlation', 
                                 point_alpha = 0.25, 
                                 point_color = fill_color, 
                                 plot_title = paste(cohort_lab, globals$response_labels[.x], sep = ': '), 
                                 cust_theme = globals$common_theme, 
                                 y_lab = 'Scoring', 
                                 x_lab = 'Survey - CoV test, days', 
                                 show_trend = FALSE)) %>% 
           set_names(globals$response) %>% 
           map2(., stats, 
                ~.x + labs(subtitle = .y)))
  
  obs_time$plots <- obs_time$plots %>% 
    map(~map(.x, ~.x + geom_smooth(method = 'loess')))
  
# END ----
  
  insert_tail()