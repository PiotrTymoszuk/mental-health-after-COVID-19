# This script generates paper figures

  insert_head()
  
# tools -----
  
  library(cowplot)

# data containers -----
  
  paper_figures <- list()
  suppl_figures <- list()
  
# Figure 1: CONSORT plot -----
  
  insert_msg('Figure 1: CONSORT plot')
  
  paper_figures$consort <- ggdraw() + 
    draw_image('./study consort/consort_diagram.png') + 
    theme(plot.margin = globals$common_margin)
  
  paper_figures$consort <- paper_figures$consort %>% 
    as_figure_object(figure_label = 'figure_1_consort', 
                     w = 180, 
                     h = 180)

# Figure 2: Most influential factors for mental health scoring identified by random forest modeling -------
  
  insert_msg('Figure 2: random forest modeling, most influential factors for MQP and DAS')
  
  paper_figures$rf_components <- plot_grid(rforest_plots$pca$north$loadings_plot + 
                                             theme(plot.tag = element_blank(), 
                                                   plot.subtitle = element_blank()) + 
                                             guides(fill = F) + 
                                             labs(title = 'Austria: factor influence on mental health scoring'), 
                                           rforest_plots$pca$south$loadings_plot + 
                                             theme(plot.tag = element_blank(), 
                                                   plot.subtitle = element_blank()) + 
                                             guides(fill = F) + 
                                             labs(title = 'Italy: factor influence on mental health scoring'), 
                                           nrow = 2, 
                                           align = 'hv') %>% 
    as_figure_object(figure_label = 'figure_2_rf_influential_factors', 
                     w = 180, 
                     h = 160)
  
# Figure 3: top factors identified by RF and their correlation with the scores -----
  
  insert_msg('Figure 3: correlations of the top factors identified by RF with scoring')
  
  paper_figures$uni_modeling <- plot_grid(psych_analyses$forest_plots$phq_anxiety_score + 
                                            labs(title = 'ANX score') + 
                                            theme(legend.position = 'none', 
                                                  plot.tag = element_blank()), 
                                          psych_analyses$forest_plots$phq_depression_score + 
                                            labs(title = 'DPR score') + 
                                            theme(legend.position = 'none', 
                                                  plot.tag = element_blank()), 
                                          psych_analyses$forest_plots$mental_health_score + 
                                            theme(legend.position = 'none'), 
                                          psych_analyses$forest_plots$life_quality_score + 
                                            theme(legend.position = 'none', 
                                                  plot.tag = element_blank()), 
                                          ncol = 2, 
                                          align = 'hv', 
                                          labels = LETTERS, 
                                          label_size = 10) %>% 
    plot_grid(get_legend(psych_analyses$forest_plots$phq_anxiety_score + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.95, 0.05)) %>% 
    as_figure_object(figure_label = 'figure_3_univariate_modeling', 
                     w = 180, 
                     h = 228) 
  
# Figure 4: Clustering of the study participants by the most influential co-variates -----
  
  insert_msg('Figure 4: participant clustering')
  
  paper_figures$clustering <- plot_grid(partclust$hm_clust_features$north + 
                                          theme(legend.position = 'none'), 
                                        partclust$hm_clust_features$south + 
                                          theme(legend.position = 'none'), 
                                        ncol = 2) %>% 
    plot_grid(., 
              nrow = 2, 
              get_legend(partclust$hm_clust_features$north + 
                           theme(legend.position = 'bottom')), 
              rel_heights = c(0.95, 0.05)) %>% 
    as_figure_object(figure_label = 'figure_4_participant_clustering', 
                     w = 180, 
                     h = 160)
  
# Figure 5: Mental scoring and mental disorder prevalence in the risk clusters ----
  
  insert_msg('Figure 5: mental scoring, depression and anxiety prevalence in the risk clusters')
  
  paper_figures$clust_mental <- plot_grid(partclust$mental_score_clust_plots$north + 
                                            theme(legend.position = 'none', 
                                                  plot.tag.position = 'right'), 
                                          partclust$mental_score_clust_plots$south + 
                                            theme(legend.position = 'none', 
                                                  plot.tag.position = 'right'), 
                                          nrow = 2) %>% 
    plot_grid(., 
              plot_grid(partclust$preval_plots_depr_anx$north + 
                          theme(legend.position = 'none', 
                                plot.tag = element_blank()), 
                        partclust$preval_plots_depr_anx$south + 
                          theme(legend.position = 'none',
                                plot.tag = element_blank()), 
                        get_legend(partclust$preval_plots_depr_anx$north), 
                        ncol = 3, 
                        rel_widths = c(0.45, 0.45, 0.1)), 
              nrow = 2, 
              rel_heights = c(0.6, 0.4), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_5_mental_scoring_clusters', 
                     w = 180, 
                     h = 180)

# Figure 6: pre-CoV depression/anxiety and the acute and persistent symptom burden -----
  
  insert_msg('Figure S6: pre-CoV depression or anxiety and acute symptoms')
  
  paper_figures$pre_cov_da_cov_sympt <- plot_grid(sympt_analyses$da$plot_panels$sum_symptoms_acute, 
                                                  sympt_analyses$da$plot_panels$neurocognitive_acute_sympt_sum, 
                                                  sympt_analyses$da$plot_panels$sum_symptoms_long, 
                                                  sympt_analyses$da$plot_panels$neurocognitive_long_sympt_sum, 
                                                  ncol = 2, 
                                                  labels = LETTERS, 
                                                  label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_6_pre_cov_da_acute_symptoms', 
                     w = 180, 
                     h = 160)
  
# Supplementary Figure S1 - S4: RF model performance ------
  
  insert_msg('Figure S1 - S4. Random forest model performance')
  
  suppl_figures[c('mental_health_score_model', 
                  'life_quality_score_model', 
                  'phq_anxiety_score_model', 
                  'phq_depression_score_model')] <- map2(rforest_plots$top_20_bar[c('mental_health_score', 
                                                                                    'life_quality_score', 
                                                                                    'phq_anxiety_score', 
                                                                                    'phq_depression_score')], 
                                                      rforest_plots$train_corr_panels[c('mental_health_score', 
                                                                                        'life_quality_score', 
                                                                                        'phq_anxiety_score', 
                                                                                        'phq_depression_score')], 
                                                      function(x, y) plot_grid(x, y, 
                                                                               nrow = 2, 
                                                                               rel_heights = c(1.3, 0.7), 
                                                                               labels = c('A', 'B'), 
                                                                               label_size = 10))
  
  suppl_figures[c('mental_health_score_model', 
                  'life_quality_score_model', 
                  'phq_anxiety_score_model', 
                  'phq_depression_score_model')] <- suppl_figures[c('mental_health_score_model', 
                                                                    'life_quality_score_model', 
                                                                    'phq_anxiety_score_model', 
                                                                    'phq_depression_score_model')] %>%
    map2(., 
         paste('figure_s', 
               1:4, 
               '_', 
               c('mental_health_score_model', 
                 'life_quality_score_model', 
                 'phq_anxiety_score_model', 
                 'phq_depression_score_model'), 
               sep = ''), 
         as_figure_object, 
         w = 180, 
         h = 210)
  
# Supplementary Figure S5: pre-CoV depression/anxiety and the mental health scoring -----
  
  insert_msg('Figure S5: pre-CoV depression or anxiety and the mental health scoring')
  
  suppl_figures$mental_health_scoring_da <- plot_grid(mqp_das_factors$depression_burnout$plot_panels$mental_health_score, 
                                                      mqp_das_factors$depression_burnout$plot_panels$life_quality_score, 
                                                      mqp_das_factors$depression_burnout$plot_panels$phq_anxiety_score, 
                                                      mqp_das_factors$depression_burnout$plot_panels$phq_depression_score, 
                                                      ncol = 2, 
                                                      labels = LETTERS, 
                                                      label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s5_da_symptoms', 
                     w = 180, 
                     h = 140)
  
# Supplementary Figure S6: pre-CoV depression/anxiety and the acute and persistent symptom burden -----
  
  insert_msg('Figure S6: pre-CoV depression or anxiety and acute symptoms')

  suppl_figures$pre_cov_da_cov_sympt <- plot_grid(sympt_analyses$da$plot_panels$sum_symptoms_acute, 
                                                  sympt_analyses$da$plot_panels$psychosom_acute_sympt_sum, 
                                                  sympt_analyses$da$plot_panels$neurocognitive_acute_sympt_sum, 
                                                  sympt_analyses$da$plot_panels$sum_symptoms_long, 
                                                  sympt_analyses$da$plot_panels$psychosom_long_sympt_sum, 
                                                  sympt_analyses$da$plot_panels$neurocognitive_long_sympt_sum, 
                                                  ncol = 2, 
                                                  labels = LETTERS, 
                                                  label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s6_pre_cov_da_acute_symptoms', 
                     w = 180, 
                     h = 228)
  
# Supplementary Figure S7. Development of the participant clusters ------
  
  insert_msg('Figure S7: Performance and QC of the clustering')
  
  suppl_figures$clust_qc <- plot_grid(plot_train_som(partclust$clust_results$north$kohonen_obj) + 
                                        globals$common_theme + 
                                        theme(plot.tag = element_blank()) +  
                                        labs(title = 'AT: SOM training process'), 
                                      plot_train_som(partclust$clust_results$south$kohonen_obj) + 
                                        globals$common_theme + 
                                        theme(plot.tag = element_blank()) + 
                                        labs(title = 'IT: SOM training process'), 
                                      partclust$clust_results$north$clust_obj$diagnostic_plots$wss + 
                                        globals$common_theme + 
                                        labs(title = 'AT: optimal cluster number'), 
                                      partclust$clust_results$south$clust_obj$diagnostic_plots$wss + 
                                        globals$common_theme + 
                                        labs(title = 'IT: optimal cluster number'),
                                      partclust$clust_results$north$clust_obj$diagnostic_plots$dendrogram + 
                                        globals$common_theme + 
                                        theme(axis.line.x = element_blank(), 
                                              axis.ticks.x = element_blank()) + 
                                        labs(title = 'AT: hierarchical clustering dendrogram', 
                                             y = 'Euclidean distance', 
                                             x = 'SOM node'), 
                                      partclust$clust_results$south$clust_obj$diagnostic_plots$dendrogram + 
                                        globals$common_theme + 
                                        theme(axis.line.x = element_blank(), 
                                              axis.ticks.x = element_blank()) + 
                                        labs(title = 'IT: hierarchical clustering dendrogram', 
                                             y = 'Euclidena distance', 
                                             x = 'SOM node'), 
                                      ncol = 2, 
                                      align = 'hv', 
                                      labels = c('A', '', 'C', '', 'D'), 
                                      label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s7_clustering_qc', 
                     w = 180, 
                     h = 220)
  
# Supplementary Figure S8: differences in clustering features between the risk clusters ----
  
  insert_msg('Figure S8: clustering features in the risk clusters')
  
  suppl_figures$clust_factors <- plot_grid(partclust$preval_plots_clust_features$north + 
                                             theme(plot.tag.position = 'right', 
                                                   legend.position = 'none'), 
                                           partclust$preval_plots_clust_features$south + 
                                             theme(plot.tag.position = 'right', 
                                                   legend.position = 'none'), 
                                           nrow = 2) %>% 
    plot_grid(., 
              get_legend(partclust$preval_plots_clust_features$north + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.95, 0.05)) %>% 
    as_figure_object(figure_label = 'figure_s8_clustering_features', 
                     w = 180, 
                     h = 220)
  
# Supplementary Figure S9: Differences in other features between the clusters -----
  
  insert_msg('Figure S9: Differences in other features between the clusters')
  
  suppl_figures$other_factors_clusters <- plot_grid(clust_char$pie_plot$north + 
                                                      theme(legend.position = 'none'), 
                                                    clust_char$pie_plot$south + 
                                                      theme(legend.position = 'none'), 
                                                    ncol = 2) %>% 
    plot_grid(., 
              get_legend(clust_char$pie_plot$north + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.95, 0.05)) %>% 
    as_figure_object(figure_label = 'figure_s9_clust_frequency', 
                     w = 180, 
                     h = 220)
  
# Saving the figures on the disc ----
  
  insert_msg('Saving the figures')
  
  paper_figures[c('consort', 
                  'rf_components', 
                  'uni_modeling', 
                  'clustering', 
                  'clust_mental', 
                  'pre_cov_da_cov_sympt')] %>% 
    walk(save_figure_object, 
         target_folder = './paper/figures', 
         device = cairo_pdf)
  
  suppl_figures[c('mental_health_score_model', 
                  'life_quality_score_model', 
                  'phq_anxiety_score_model', 
                  'phq_depression_score_model', 
                  'mental_health_scoring_da', 
                  'clust_qc', 
                  'clust_factors', 
                  'other_factors_clusters')] %>% 
    walk(save_figure_object, 
         target_folder = './paper/supplementary figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()