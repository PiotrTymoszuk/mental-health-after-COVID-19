# This script generates plots with random forest results

  insert_head()
  
# data container ----
  
  rforest_plots <- list()
  
# globals: n numbers -----
  
  insert_msg('Number of observations per model')
  
  rforest_plots$n_tags <- rforest$predictions %>% 
    map(~map(.x, caretExtra::nobs)) %>% 
    map(~paste0('AT: n = ', .x$train, 
                ', IT: n = ', .x$test))
  
# Rsq and RMSE for the RF models: raw and calibrated ------
  
  insert_msg('Rsq and RMSE plots')
  
  rforest_plots$pred_stat_plots <- list(stat = c('RMSE', 'rsq'), 
                                        plot_title = list('Random Forests: fit error', 
                                                          'Random Forests: explained variance'), 
                                        y_lab = list('RMSE', expression('R'^2))) %>% 
    pmap(plot_ml_stats, 
         data = rforest$calibrated_pred_stats, 
         plot_tag = rforest_plots$n_tags$phq_anxiety_score) %>% 
    map(~.x + 
          scale_color_manual(values = unname(globals$pred_colors), 
                             labels = unname(globals$pred_labs), 
                             name = 'Data set') + 
          scale_shape_manual(values = c(16, 17, 15), 
                             labels = unname(globals$pred_labs), 
                             name = 'Data set') + 
          scale_x_discrete(labels = globals$response_labels) + 
          expand_limits(y = 0)) %>% 
    set_names(c('rmse', 'rsq'))

# Regression/calibration plots for each response: training, Cv and test data set, raw and calibrates ------
  
  insert_msg('Regression plots, fitted vs outcome')

  rforest_plots$calibrated_regression <- globals$response %>% 
    map(function(response) list(predx_object = rforest$calibrated_preds[[response]][c('train', 'cv', 'test')], 
                                point_color = globals$pred_colors, 
                                plot_title = paste(globals$response_labels[[response]], 
                                                   globals$pred_labs, sep = ': ')) %>% 
          pmap(plot, 
               type = 'fit', 
               trend_method = 'loess', 
               point_alpha = 0.3, 
               cust_theme = globals$common_theme, 
               x_lab = 'Normalized outcome', 
               y_lab = 'predicted outcome')) %>% 
    set_names(globals$response)
  
  ## setting the axes

  rforest_plots$calibrated_regression <- rforest_plots$calibrated_regression %>% 
    map(~map(.x, ~.x + 
               scale_y_continuous(breaks = seq(0, 1, by = 0.25), 
                                  limits = c(0, 1)) + 
               scale_x_continuous(breaks = seq(0, 1, by = 0.25), 
                                  limits = c(0, 1))))
  
  
# Top 20 most important factors ----
  
  insert_msg('Top 20 most important factors for each response')
  
  rforest_plots$top_20_bar <- list(inp_tbl = map(rforest$train_summary, top_n, 20, delta_mse), 
                                   plot_title = paste0('AT: ', globals$response_labels)) %>% 
    pmap(plot_bar_rf, 
         x_lab = expression(Delta *' MSE'))
  
# Venn plot with the top 20 most influential factors for each response -----
  
  insert_msg('Venn plots')
  
  rforest_plots$venn_plot <- plot_n_venn(data = rforest$top_factors, 
                                         subset_names = globals$response_labels[names(rforest$top_factors)], 
                                         fill_color = c('SteelBlue', 
                                                        'FireBrick', 
                                                        'DarkOrange', 
                                                        'ForestGreen'), 
                                         plot_title = 'Common influential variables: AT', 
                                         plot_tag = 'Top 20 most influential variables for each response')

# Common influential factors -----
  
  insert_msg('Common influential factors in a bubble plot')
  
  rforest_plots$influence_bubble <- rforest$train_summary %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y)) %>% 
    filter(variable %in% rforest$cmm_factors) %>% 
    ggplot(aes(x = response, 
               y = reorder(variable_label, delta_mse))) + 
    geom_point(aes(size = delta_mse, 
                   fill = delta_mse), 
               shape = 21) + 
    geom_text(aes(label = signif(delta_mse, 3)), 
              size = 2.5, 
              hjust = -0.8) + 
    scale_fill_gradient2(low = 'steelblue4', 
                         mid = 'white', 
                         high = 'firebrick4', 
                         midpoint = 50) + 
    scale_x_discrete(labels = globals$response_labels) + 
    guides(fill = FALSE, 
           size = FALSE) + 
    globals$common_theme + 
    theme(axis.title = element_blank()) + 
    labs(title = 'Random Forests: variable importance, AT cohort', 
         subtitle = expression('Common influential factors, '*Delta*'MSE'))
  
# END -----
  
  insert_tail()