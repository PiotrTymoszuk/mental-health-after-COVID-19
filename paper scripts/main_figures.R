# This script generates paper figures

  insert_head()

# data container -----
  
  paper_figures <- list()

# Figure 1: CONSORT plot -----
  
  insert_msg('Figure 1: CONSORT plot')
  
  paper_figures$consort <- ggdraw() + 
    draw_image('./study consort/consort_diagram.png') + 
    theme(plot.margin = globals$common_margin)
  
  paper_figures$consort <- paper_figures$consort %>% 
    as_figure(label = 'figure_1_consort', 
              w = 180, 
              h = 180)
  
# Figure 2: Most influential factors for mental health scoring identified by random forest modeling -------
  
  insert_msg('Figure 2: random forest modeling, most influential factors for MQP and DAS')
  
  paper_figures$rf_components$top_panel <- rforest_plots$pred_stat_plots[c('rmse', 'rsq')] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.tag = element_blank()) + 
          scale_x_discrete(limits = c('phq_anxiety_score', 
                                      'phq_depression_score',
                                      'mental_health_score', 
                                      'life_quality_score'), 
                           labels = globals$response_labels)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(plot_grid(get_legend(rforest_plots$pred_stat_plots[[1]]), 
                        ggdraw() + 
                          draw_text(text = stri_replace(get_tag(rforest_plots$pred_stat_plots[[1]]), 
                                                        fixed = ', ', 
                                                        replacement = '\n'), 
                                    size = 8, 
                                    hjust = 0, 
                                    x = 0.05), 
                        nrow = 2),
              ncol = 2, 
              rel_widths = c(0.9, 0.1))
  
  paper_figures$rf_components$bottom_panel <- plot_grid(rforest_plots$venn_plot, 
                                                        rforest_plots$influence_bubble + 
                                                          labs(title = 'Common influential variables: AT, \u0394MSE', 
                                                               subtitle = NULL) + 
                                                          scale_x_discrete(limits = c('phq_anxiety_score', 
                                                                                      'phq_depression_score',
                                                                                      'mental_health_score', 
                                                                                      'life_quality_score'), 
                                                                           labels = globals$response_labels), 
                                                        ncol = 2, 
                                                        rel_widths = c(0.4, 0.6))
  
  paper_figures$rf_components <- plot_grid(paper_figures$rf_components$top_panel, 
                                           paper_figures$rf_components$bottom_panel, 
                                           nrow = 2, 
                                           rel_heights = c(0.35, 0.65), 
                                           labels = LETTERS, 
                                           label_size = 10) %>% 
    as_figure(label = 'figure_2_random_forest', 
              w = 180, 
              h = 160)
  
# Figure 3: top factors identified by RF and their correlation with the scores -----
  
  insert_msg('Figure 3: correlations of the top factors identified by RF with scoring')
  
  paper_figures$uni_modeling <- psych_analyses$forest_plots[c('phq_anxiety_score', 
                                                              'phq_depression_score',
                                                              'mental_health_score', 
                                                              'life_quality_score')] %>% 
    map(~.x$panel) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(psych_analyses$forest_plots[[1]]$legend, 
              nrow = 2, 
              rel_heights = c(0.94, 0.06)) %>% 
    as_figure(label = 'figure_3_univariate', 
              w = 180, 
              h = 210)
  
# Figure 4: Clustering of the study participants by the most influential co-variates -----
  
  insert_msg('Figure 4: participant clustering')
  
  paper_figures$clustering$top_panel <- partclust$pca_plots %>% 
    map(~.x + theme(plot.tag = element_blank(), 
                    legend.position = 'none', 
                    plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(partclust$pca_plots[[1]]), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1))
  
  paper_figures$clustering$bottom_panel <- partclust$features_hm %>%
    map2(., c('AT: training', 'IT: test'), 
         ~.x + 
           theme(legend.position = 'none') + 
           labs(title = .y, 
                subtitle = NULL, 
                tag = paste0('\n', get_tag(.x)))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(partclust$features_hm[[1]]), 
              ncol = 2, 
              rel_widths = c(0.85, 0.15))
  
  paper_figures$clustering <- plot_grid(paper_figures$clustering$top_panel, 
                                        paper_figures$clustering$bottom_panel, 
                                        nrow = 2, 
                                        labels = LETTERS, 
                                        label_size = 10) %>% 
    as_figure(label = 'figure_4_clustering', 
              w = 180, 
              h = 180)
  
# Figure 5: Mental scoring and mental disorder prevalence in the risk clusters ----
  
  insert_msg('Figure 5: mental scoring, depression and anxiety prevalence in the risk clusters')
  
  paper_figures$clust_mental$top_panel <- clust_mental$violin_plots[c('north.phq_anxiety_score', 
                                                                      'south.phq_anxiety_score', 
                                                                      'north.phq_depression_score', 
                                                                      'south.phq_depression_score', 
                                                                      'north.mental_health_score', 
                                                                      'south.mental_health_score', 
                                                                      'north.life_quality_score', 
                                                                      'south.life_quality_score')] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 4, 
              align = 'hv', 
              labels = c('A', '', 'B', '', 'C', '', 'D'), 
              label_size = 10)
  
  paper_figures$clust_mental$bottom_panel <- plot_grid(clust_mental$frequency_plot + 
                                                         theme(plot.tag = element_blank()), 
                                                       ggdraw() + 
                                                         draw_text(text = stri_replace_all(get_tag(clust_mental$frequency_plot), 
                                                                                           fixed = ', ', 
                                                                                           replacement = '\n'), 
                                                                   size = 8, 
                                                                   hjust = 0, 
                                                                   x = 0.05), 
                                                       ncol = 2, 
                                                       rel_widths = c(0.9, 0.1))
  
  paper_figures$clust_mental <- plot_grid(paper_figures$clust_mental$top_panel, 
                                          paper_figures$clust_mental$bottom_panel, 
                                          nrow = 2, 
                                          rel_heights = c(2, 1), 
                                          labels = c('', 'E'), 
                                          label_size = 10) %>% 
    as_figure(label = 'figure_5_mental_clusters', 
              w = 180, 
              h = 180)

# Figure 6: pre-CoV depression/anxiety subset -----
  
  insert_msg('Figure 6: features of the pre-CoV subset')
  
  paper_figures$pre_cov_da$top_panel <- pheno_da$volcano_plots %>% 
    map2(., c('AT: DA+ vs DA-', 'IT: DA+ vs DA-'), 
         ~.x + 
           theme(legend.position = 'none') + 
           labs(title = .y, 
                subtitle = NULL)) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')
  
  paper_figures$pre_cov_da$bottom_panel <- pheno_da$violin_panel %>% 
    map2(., globals$cohort_labs, 
         ~.x + 
           theme(plot.tag = element_blank(), 
                 legend.position = 'none') +
           labs(title = .y, 
                subtitle = NULL))
  
  paper_figures$pre_cov_da$bottom_panel <- plot_grid(paper_figures$pre_cov_da$bottom_panel$north, 
                                                     paper_figures$pre_cov_da$bottom_panel$south + 
                                                       theme(axis.text.y = element_blank()), 
                                                     ncol = 2, 
                                                     align = 'h', 
                                                     rel_widths = c(0.6, 0.4)) %>% 
    plot_grid(get_legend(pheno_da$violin_panel[[1]]), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1))
  
  paper_figures$pre_cov_da <- plot_grid(paper_figures$pre_cov_da$top_panel,
                                        paper_figures$pre_cov_da$bottom_panel, 
                                        nrow = 2, 
                                        rel_heights = c(0.35, 0.65), 
                                        labels = LETTERS, 
                                        label_size = 10) %>% 
    as_figure(label = 'figure_6_da_features', 
              w = 180, 
              h = 220)
  
# Figure 7: result summary -------
  
  insert_msg('Figure 7: Result summary')
  
  paper_figures$summary <- ggdraw() + 
    draw_image('./result summary/result_summary.png') + 
    theme(plot.margin = globals$common_margin)
  
  paper_figures$summary <- paper_figures$summary %>% 
    as_figure(label = 'figure_7_summary', 
              w = 180, 
              h = 110)
  
# Saving the figures ------
  
  insert_msg('Saving the figures')
  
  paper_figures %>% 
    walk(save_figure, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()