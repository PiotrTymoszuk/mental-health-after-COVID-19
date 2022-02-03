# Builds supplementary figures

  insert_head()
  
# data container ----
  
  suppl_figures <- list()
  
# Supplementary Figure S1: distribution of the mental scoring variables -----
  
  insert_msg('Figure S1: mental scorin variable distribution')
  
  suppl_figures$distribution <- map2(eda_dist$histograms$north[c(globals$response, 
                                                                 'stress_score', 
                                                                 'perf_impairment')], 
                                     eda_dist$histograms$south[c(globals$response, 
                                                                 'stress_score', 
                                                                 'perf_impairment')], 
                                     ~plot_grid(.x + 
                                                  theme(plot.tag = element_blank(), 
                                                        plot.title.position = 'plot') + 
                                                  labs(y = 'Count'), 
                                                .y + 
                                                  theme(plot.tag = element_blank(), 
                                                        plot.title.position = 'plot') + 
                                                  labs(y = 'Count'), 
                                                ncol = 2, 
                                                align = 'hv')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s1_distribution', 
              w = 180, 
              h = 180)
  
# Supplementary Figure S2: observation time and mental scoring -----
  
  insert_msg('Figure S2: mental scoring and the observatinon time')
  
  suppl_figures$obs_time <- map2(obs_time$plots$north,
                                 obs_time$plots$south, 
                                 ~plot_grid(.x + 
                                              theme(plot.title.position = 'plot') + 
                                              labs(tag = paste0('\n', get_tag(.x))), 
                                            .y + 
                                              theme(plot.title.position = 'plot') + 
                                              labs(tag = paste0('\n', get_tag(.x))), 
                                            ncol = 2, 
                                            align = 'hv')) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s2_obs_time', 
              w = 180, 
              h = 230)
  
# Supplementary Figure S3: survey duration and mental scoring -----
  
  insert_msg('Figure S3: mental scoring and the survey duration')
  
  suppl_figures$survey <- map2(survey$plots$north,
                               survey$plots$south, 
                               ~plot_grid(.x + 
                                            theme(plot.title.position = 'plot') + 
                                            labs(tag = paste0('\n', get_tag(.x))), 
                                          .y + 
                                            theme(plot.title.position = 'plot') + 
                                            labs(tag = paste0('\n', get_tag(.x))), 
                                          ncol = 2, 
                                          align = 'hv')) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_s3_survey_time', 
              w = 180, 
              h = 230)
  
# Supplementary Figure S4: correlation of the mental scoring variables, entire cohorts ------
  
  insert_msg('Figure S4: mental scoring variable correlation, entire cohorts')
  
  suppl_figures$correlation_responses <- c(overlap$mental_plots, 
                                           overlap$kappa_plots) %>% 
    map(~.x + guides(size = FALSE, fill = FALSE)) %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              ncol = 2, 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s4_mental_overlap_healthy', 
              w = 180, 
              h = 180)
  
# Supplementary Figure S5: correlation of the mental scoring variables, pre-CoV depression/anxiety ------
  
  insert_msg('Figure S5: mental scoring variable correlation, DA')
  
  suppl_figures$correlation_responses_da <- c(overlap$mental_plots_da, 
                                              overlap$kappa_plots_da) %>% 
    map(~.x + guides(size = FALSE, fill = FALSE)) %>% 
    plot_grid(plotlist = ., 
              align = 'hv', 
              ncol = 2, 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    as_figure(label = 'figure_s5_mental_overlap_da', 
              w = 180, 
              h = 180)
  
# Supplementary Figure S6 - S9: random forest modeling ------
  
  insert_msg('Figure S6 - S9: random forests')
  
  suppl_figures$rforest$side_panels <- rforest_plots$calibrated_regression[c('phq_anxiety_score', 
                                                                             'phq_depression_score', 
                                                                             'mental_health_score', 
                                                                             'life_quality_score')] %>% 
    map(~map(.x, ~.x + labs(tag = paste0('\n', get_tag(.x))))) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 1, 
                   align = 'hv'))
  
  suppl_figures[c('rforest_anx', 
                  'rforest_dpr', 
                  'rforest_omh', 
                  'rforest_qol')] <- map2(suppl_figures$rforest$side_panels, 
                                          rforest_plots$top_20_bar[c('phq_anxiety_score', 
                                                                     'phq_depression_score', 
                                                                     'mental_health_score', 
                                                                     'life_quality_score')], 
                                          ~plot_grid(.y, .x, 
                                                     ncol = 2, 
                                                     labels = LETTERS, 
                                                     label_size = 10))
  
  suppl_figures[c('rforest_anx', 
                  'rforest_dpr', 
                  'rforest_omh', 
                  'rforest_qol')] <- suppl_figures[c('rforest_anx', 
                                                     'rforest_dpr', 
                                                     'rforest_omh', 
                                                     'rforest_qol')] %>% 
    map2(., paste0('figure_s', 6:9, '_', names(.)), 
         as_figure, 
         w = 180, 
         h = 180)
  
  suppl_figures$rforest$side_panels <- NULL

  suppl_figures <- compact(suppl_figures)
  
# Supplementary Figure S10: percentage of explained deviance -----
  
  insert_msg('Figure S10: percentage of explained deviance')
  
  suppl_figures$perc_explained <- expl_var$frac_dev_plot[c('phq_anxiety_score', 
                                                           'phq_depression_score', 
                                                           'mental_health_score', 
                                                           'life_quality_score')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(expl_var$frac_dev_plot[[1]] + 
                           theme(legend.position = 'bottom') + 
                           labs(fill = NULL)), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s10_expl_deviance_healthy',
              w = 180, 
              h = 200)
  
# Supplementary Figure S11: performance of the RF models in depression/anxiety ------
  
  insert_msg('Figure S11: performance of the RF models in depression/anxiety')
  
  suppl_figures$rf_performance_da <- rf_da$pred_stat_plots[c('rmse', 'rsq')] %>% 
    map(~.x + 
          scale_x_discrete(limits = c('phq_anxiety_score', 
                                      'phq_depression_score', 
                                      'mental_health_score', 
                                      'life_quality_score'), 
                           labels = globals$response_labels) + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(rf_da$pred_stat_plots[[1]] + 
                           labs(color = NULL)), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_s11_rf_performance_da', 
              w = 180, 
              h = 80)
  
# Supplementary Figure S12: Cluster quality control ------
  
  insert_msg('Figure S12: cluster quality control')
  
  suppl_figures$clust_qc <- plot_grid(partclust$diagn_plots$training$observation + 
                                        labs(title = 'SOM training', 
                                             subtitle = NULL), 
                                      partclust$diagn_plots$diagnostic$node$wss + 
                                        labs(title = 'SOM node clustering', 
                                             subtitle = NULL, 
                                             tag = stri_replace(get_tag(partclust$diagn_plots$diagnostic$node$wss), 
                                                                fixed = 'Observations:', 
                                                                replacement = 'SOM nodes:') %>% 
                                               stri_replace(regex = '\nVariables:\\s{1}.*', 
                                                            replacement = '')), 
                                      partclust$diagn_plots$diagnostic$node$dendrogram + 
                                        labs(title = 'SOM node clustering', 
                                             tag = stri_replace(get_tag(partclust$diagn_plots$diagnostic$node$wss), 
                                                                fixed = 'Observations:', 
                                                                replacement = 'SOM nodes:') %>% 
                                               stri_replace(regex = '\nVariables:\\s{1}.*', 
                                                            replacement = ''), 
                                             y = 'Manhattan distance'), 
                                      partclust$clust_var_plot + 
                                        scale_y_discrete(labels = c('north' = 'AT: training', 
                                                                    'south' = 'IT: test')), 
                                      ncol = 2, 
                                      labels = LETTERS, 
                                      label_size = 10) %>% 
    as_figure(label = 'figure_s12_clustering_qc', 
              w = 180, 
              h = 160)
  
# Supplementary Figure S13: characteristic of the clusters -----
  
  insert_msg('Figure S13: characteristic of the clusters')
  
  suppl_figures$clust_features_summary <- clust_ft$volcano_plots %>% 
    map2(., c('AT: numeric features', 
              'IT: numeric features', 
              'AT: categorical features', 
              'IT: categorical features'), 
         ~.x + 
           theme(legend.position = 'none', 
                 plot.subtitle = element_blank()) + 
           labs(title = .y))
  
  suppl_figures$clust_features_summary <- plot_grid(suppl_figures$clust_features_summary$north_numeric + 
                                                      theme(plot.tag = element_blank()), 
                                                    suppl_figures$clust_features_summary$south_numeric + 
                                                      theme(plot.tag = element_blank()), 
                                                    suppl_figures$clust_features_summary$north_factor, 
                                                    suppl_figures$clust_features_summary$south_factor, 
                                                    ncol = 2, 
                                                    align = 'hv', 
                                                    labels = c('A', '', 'B'), 
                                                    label_size = 10) %>% 
    as_figure(label = 'figure_s13_cluster_feature_volcano', 
              w = 180, 
              h = 180)
  
# Supplementary Figure S14: significantly and strongly regulated features ----
  
  insert_msg('Figure S14: significant clustering features')
  
  suppl_figures$clust_features_heat <- clust_ft$hm_plots %>% 
    map2(., c('AT', 'IT'), 
         ~.x + 
           theme(legend.position = 'none') + 
           labs(title = .y, 
                subtitle = NULL)) %>%
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(clust_ft$hm_plots[[1]]), 
              ncol = 2, 
              rel_widths = c(0.87, 0.13)) %>% 
    as_figure(label = 'figure_s14_clust_hm', 
              w = 180, 
              h = 160)
  
# Saving the figures and markdown references ------
  
  insert_msg('Saving the figures')
  
  suppl_figures %>% 
    walk(save_figure, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)

# END -----
  
  insert_tail()