# This script generates plots with random forest results

  insert_head()
  
# data container ----
  
  rforest_plots <- list()
  
# globals -----
  
  rforest_plots$responses <- c('phq_depression_score', 
                               'phq_anxiety_score', 
                              # 'stress_score', 
                               'mental_health_score', 
                               'life_quality_score')

  rforest_plots$response_labels <- c('phq_depression_score' = 'DEP', 
                                     'phq_anxiety_score' = 'ANX', 
                                     #'stress_score' = 'STR', 
                                     'mental_health_score' = 'OMH', 
                                     'life_quality_score' = 'QoL')
  
  rforest_plots$response_colors <- c('phq_depression_score' = 'cadetblue3', 
                                     'phq_anxiety_score' = 'gray60', 
                                     #'stress_score' = 'coral3', 
                                     'mental_health_score' = 'lightskyblue3', 
                                     'life_quality_score' = 'darkseagreen3')

# bar plots with the most important factors in the training cohorts -----
  
  insert_msg('Bar plots of importance')
  
  rforest_plots$top_20_bar <- list(modeling_summary = rforest$cmm_train_summary, 
                                   plot_title = translate_var(names(rforest$cmm_train_summary), short = F)) %>% 
    pmap(plot_bar_rf_cohorts)
  
# Venn plots with the top20 most influential factors for each response -----
  
  insert_msg('Venn plots')
  
  rforest_plots$venn_plots <- list(plotting_lst = map2(rforest$top_factors$north, 
                                                       rforest$top_factors$south, 
                                                       function(x, y) list(AT = x, 
                                                                           IT = y)), 
                                   plot_title = translate_var(names(rforest$top_factors$north), 
                                                              short = F)) %>% 
    pmap(plot_venn, 
         colors = globals$cohort_colors, 
         fct_per_line = 1, 
         short = F)
  
# Correlation plots for the training-training prediction -----
  
  insert_msg('Correlation plots: training - training predictions')
  
  rforest_plots$train_corr_plots_north <- list(pred_tbl = rforest$predictions_north %>% 
                                                 map(function(x) x$predictions), 
                                               plot_title = translate_var(names(rforest$predictions_north), short = F), 
                                               plot_tag = rforest$predictions_north %>% 
                                                 map(function(x) paste('\n\u03C1 = ', 
                                                                       signif(x$measures_train$corr_spearman, 2), 
                                                                       ', MAE(training) = ', 
                                                                       signif(x$measures_train$mae, 2), 
                                                                       ', MAE(CV) = ', 
                                                                       signif(x$measures_cv$mae, 2), 
                                                                       ', n = ', 
                                                                       x$measures_train$n_complete))) %>% 
    pmap(plot_corr, 
         plot_subtitle = 'train: TY, test: TY', 
         fill_color = globals$cohort_colors[1])
  
  rforest_plots$train_corr_plots_south <- list(pred_tbl = rforest$predictions_south %>% 
                                                 map(function(x) x$predictions), 
                                               plot_title = translate_var(names(rforest$predictions_south), short = F), 
                                               plot_tag = rforest$predictions_south %>% 
                                                 map(function(x) paste('\n\u03C1 = ', 
                                                                       signif(x$measures_train$corr_spearman, 2), 
                                                                       ', MAE(training) = ', 
                                                                       signif(x$measures_train$mae, 2), 
                                                                       ', MAE(CV) = ', 
                                                                       signif(x$measures_cv$mae, 2), 
                                                                       ', n = ', 
                                                                       x$measures_train$n_complete))) %>% 
    pmap(plot_corr, 
         plot_subtitle = 'train: IT, test: AT', 
         fill_color = globals$cohort_colors[2])
  

# Correlation plots for the training-test prediction -----
  
  insert_msg('Correlation plots: training - training predictions')
  
  rforest_plots$test_corr_plots_north <- list(pred_tbl = rforest$predictions_test_north %>% 
                                                map(function(x) x$predictions), 
                                              plot_title = translate_var(names(rforest$predictions_test_north), short = F), 
                                              plot_tag = rforest$predictions_test_north %>% 
                                                map(function(x) paste('\n\u03C1 = ', 
                                                                      signif(x$measures_train$corr_spearman, 2), 
                                                                      ', MAE(training) = ', 
                                                                      signif(x$measures_train$mae, 2), 
                                                                      ', MAE(CV) = ', 
                                                                      signif(x$measures_cv$mae, 2), 
                                                                      ', n = ', 
                                                                      x$measures_train$n_complete))) %>% 
    pmap(plot_corr,
         plot_subtitle = 'train: AT, test: IT', 
         fill_color = globals$cohort_colors[2])
  
  rforest_plots$test_corr_plots_south <- list(pred_tbl = rforest$predictions_test_south %>% 
                                                map(function(x) x$predictions), 
                                              plot_title = translate_var(names(rforest$predictions_test_south), short = F), 
                                              plot_tag = rforest$predictions_test_south %>% 
                                                map(function(x) paste('\n\u03C1 = ', 
                                                                      signif(x$measures_train$corr_spearman, 2), 
                                                                      ', MAE(training) = ', 
                                                                      signif(x$measures_train$mae, 2), 
                                                                      ', MAE(CV) = ', 
                                                                      signif(x$measures_cv$mae, 2), 
                                                                      ', n = ', 
                                                                      x$measures_train$n_complete))) %>% 
    pmap(plot_corr, 
         rho_method = 'pearson', 
         plot_subtitle = 'train: IT, test: AT', 
         fill_color = globals$cohort_colors[1])
  
# Generating panels of correlation plots for the training-training prediction -----
  
  insert_msg('Generating panels of correlation plots for the training-training prediction')
  
  rforest_plots$train_corr_panels <-map2(rforest_plots$train_corr_plots_north, 
                                         rforest_plots$train_corr_plots_south, 
                                         function(x, y) plot_grid(x, y, ncol = 2))
  
# creating a PCA plot: each variable is given three coordinates corresponding to the delta MSE value ------
  
  insert_msg('PCA plot of the delta MSE values')

  rforest_plots$pca <- rforest$train_summary %>% 
    map(function(x) x[rforest_plots$responses]) %>% 
    map(rf_pca, 
        k = 2, 
        top_factors = 10)

# END -----
  
  insert_tail()