# Applies the Random Forests models trained in the AT cohort to predict the mental health
# scoring in the Depression/Anxiety-opsitive and -negative participant subsets.

  insert_head()
  
# container list ----
  
  rf_da <- list()
  
# test data sets and n numbers -----
  
  insert_msg('Test data sets')
  
  rf_da$analysis_tbl <- rforest$inp_tbl %>% 
    transpose %>% 
    map(function(resp) map(resp, ~dlply(.x, .(depression_burnout), as_tibble)) %>% 
          map(set_names, c('da_negative', 'da_positive')) %>% 
          unlist(recursive = FALSE))
  
  rf_da$n_numbers <- cov_data %>% 
    map(count, depression_burnout) %>% 
    map(~paste0('DA-: n = ', .x$n[1], ', DA+: n = ', .x$n[2])) %>% 
    map2_chr(., globals$cohort_labs, ~paste(.y, .x, sep = ': ')) %>% 
    paste(collapse = '\n') %>% 
    paste0('\n', .)

# calibrated predictions ------
  
  insert_msg('Calibrated predictions')
  
  ## optimal calibration quantiles as determined before
  
  rf_da$optimal_qu <- rforest$calibrated_preds %>% 
    map_dbl(~.x$qu)
  
  ## calibrating the predictions
  
  rf_da$preds <- list(rf_model = rforest$models_train_north, 
                      test_data = rf_da$analysis_tbl, 
                      qu = rf_da$optimal_qu) %>% 
    pmap(function(rf_model, test_data, qu) test_data %>% 
           map(~calibration(rf_model, 
                            newdata = .x, 
                            qu = qu)))
  
# prediction stats, test collectives only -----
  
  insert_msg('Prediction stats for the test collectives only')
  
  rf_da$test_preds <- rf_da$preds %>% 
    map(~map(.x, ~.x$test))
  
  rf_da$test_stats <- rf_da$test_preds %>% 
    map(~map(.x, summary) %>% 
          map2_dfr(., names(.), ~mutate(.x, prediction = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y)) %>% 
    mutate(cohort = ifelse(stri_detect(prediction, fixed = 'north'), 'north', 'south'), 
           depression_burnout = ifelse(stri_detect(prediction, fixed = 'positive'), 'yes', 'no'), 
           cohort = factor(cohort), 
           depression_burnout = factor(depression_burnout))
  
# Regression plots -----
  
  insert_msg('Regression plots')
  
  rf_da$regression_plots <- rf_da$test_preds %>% 
    map(function(response) list(x = response, 
                                plot_title = c('AT, DA-', 
                                               'AT, DA+', 
                                               'IT, DA-', 
                                               'IT, DA+'), 
                                point_color = globals$cohort_colors[c('north', 
                                                                      'north', 
                                                                      'south', 
                                                                      'south')]) %>% 
          pmap(plot, 
               type = 'fit', 
               trend_method = 'loess', 
               point_alpha = 0.3, 
               cust_theme = globals$common_theme, 
               x_lab = 'Normalized outcome', 
               y_lab = 'predicted outcome') %>% 
          map(~.x + 
                scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) + 
                scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25))))
  
# Plots of R-squared and and RMSE -----
  
  insert_msg('Plots of R-suared and RMSE')
  
  rf_da$pred_stat_plots <- list(stat = c('RMSE', 'rsq'), 
                                plot_title = list('Random Forests: fit error', 
                                                  'Random Forests: explained variance'), 
                                y_lab = list('RMSE', expression('R'^2))) %>% 
    pmap(plot_ml_stats, 
         data = rf_da$test_stats, 
         plot_order = c('north.da_negative', 
                        'north.da_positive', 
                        'south.da_negative', 
                        'south.da_positive'), 
         plot_tag = rf_da$n_numbers) %>% 
    map(~.x + 
          scale_color_manual(values = c('north.da_negative' = 'steelblue2', 
                                        'north.da_positive' = 'steelblue4', 
                                        'south.da_negative' = 'coral2', 
                                        'south.da_positive' = 'coral4'), 
                             labels = c('north.da_negative' = 'AT, DA-', 
                                        'north.da_positive' = 'AT, DA+', 
                                        'south.da_negative' = 'IT, DA-', 
                                        'south.da_positive' = 'IT, DA+'), 
                             name = 'Test\ndata set') + 
          scale_shape_manual(values = c('north.da_negative' = 16, 
                                        'north.da_positive' = 17, 
                                        'south.da_negative' = 16, 
                                        'south.da_positive' = 17), 
                             labels = c('north.da_negative' = 'AT, DA-', 
                                        'north.da_positive' = 'AT, DA+', 
                                        'south.da_negative' = 'IT, DA-', 
                                        'south.da_positive' = 'IT, DA+'), 
                             name = 'Test\ndata set') + 
          scale_x_discrete(labels = globals$response_labels) + 
          expand_limits(y = 0)) %>% 
    set_names('rmse', 'rsq')
  
# END -----
  
  insert_tail()