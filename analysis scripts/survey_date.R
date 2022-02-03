# Investigates the association of the survey date and mental scoring. 
# the tool of choice to investigate the association with time is GAM.

  insert_head()
  
# container list -----
  
  survey <- list()
  
# globals ------
  
  insert_msg('Globals setup')
  
  survey$analysis_tbl <- cov_data %>% 
    map(select, 
        ID, 
        date, 
        all_of(globals$response)) %>% 
    map(mutate, 
        surv_duration = as.numeric(date - as.Date('2020-09-29')))
  
  ## GAM formulas
  
  survey$formulas <- globals$response %>% 
    map(~paste(.x, '~ s(surv_duration, bs = "cs")')) %>% 
    map(as.formula) %>% 
    set_names(globals$response)
  
# modeling with GAMs ----
  
  insert_msg('Serial modeling')
 
  survey$test_results <- model_gam(data_list = survey$analysis_tbl, 
                                   formulas = survey$formulas, 
                                   family = 'poisson') %>% 
    dlply(.(cohort))
  
# correlation plots -----
  
  insert_msg('Correlation plots')
  
  survey$plots <- list(cohort = survey$analysis_tbl,
                       fill_color = globals$cohort_colors, 
                       cohort_lab = globals$cohort_labs, 
                       stats = map(survey$test_results, 
                                   ~.x[['plot_sub']])) %>% 
    pmap(function(cohort, fill_color, stats, cohort_lab) globals$response %>% 
          map(~plot_correlation(data = cohort, 
                                variables = c('surv_duration', .x), 
                                type = 'correlation', 
                                point_alpha = 0.25, 
                                point_color = fill_color, 
                                plot_title = paste(cohort_lab, globals$response_labels[.x], sep = ': '), 
                                cust_theme = globals$common_theme, 
                                y_lab = 'Scoring', 
                                x_lab = 'Survey duration, days', 
                                show_trend = FALSE)) %>% 
           set_names(globals$response) %>% 
           map2(., stats, 
                ~.x + labs(subtitle = .y)))
  
  survey$plots <- survey$plots %>% 
    map(~map(.x, ~.x + geom_smooth(method = 'loess')))
  
# END ----
  
  insert_tail()
  