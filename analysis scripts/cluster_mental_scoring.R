# Compares mental scoring and prevalence of clinically significant depression/anxiety between the clusters

  insert_head()
  
# container list ----
  
  clust_mental <- list()
  
# globals: variables and analysis tables -----
  
  insert_msg('Globals setup')
  
  clust_mental$response <- c(globals$response, 
                             'phq_depression_positive', 
                             'phq_anxiety_positive')
  
  clust_mental$test_types <- c(rep('kruskal_eta', 4), 
                               rep('cramer_v', 2))
  
  clust_mental$assignment <- partclust$clust_objects %>% 
    map(extract, 'assignment') %>% 
    map(mutate, ID = observation) %>% 
    map(select, ID, clust_id)
  
  clust_mental$analysis_tbl <- cov_data %>% 
    map(select, ID, all_of(clust_mental$response)) %>% 
    map2(., clust_mental$assignment, left_join, by = 'ID') %>% 
    map(filter, !is.na(clust_id))
  
  ## n numbers
  
  clust_mental$n_numbers <- partclust$clust_objects %>% 
    map(ngroups) %>% 
    map(~map2_chr(.x$clust_id, 
                  .x$n, 
                  paste, sep = ': n = ') %>% 
          paste(collapse = ', ') %>% 
          paste0('\n', .))
  
# Testing for differences: factor variables by Chi-squared test, numeric ones by Kruskal-Wallis test ----
  
  insert_msg('Testing for differences between the clusters')
  
  clust_mental$test_results <- clust_mental$analysis_tbl %>% 
    map(~compare_variables(.x, 
                           variables = clust_mental$response, 
                           split_factor = 'clust_id', 
                           what = 'eff_size', 
                           types = clust_mental$test_types, 
                           ci = FALSE, 
                           pub_styled = TRUE, 
                           adj_method = 'BH')) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y)) %>% 
    mutate(plot_lab = paste(significance, eff_size, sep = ', '))

# Plotting the results: violin plots. Effect sizes and p values in the subtitles -----
  
  insert_msg('Plotting the mental scores and frequency of depression and anxiety')
  
  clust_mental$violin_plots <- list(cohort = clust_mental$analysis_tbl, 
                                    prefix = globals$cohort_labs) %>% 
    pmap(function(cohort, prefix) list(variable = globals$response, 
                                       plot_title = paste(prefix, 
                                                          globals$response_labels, sep = ': ')) %>% 
           pmap(plot_variable, 
                cohort, 
                split_factor = 'clust_id', 
                data_names = unname(globals$clust_labs), 
                type = 'violin', 
                cust_theme = globals$common_theme, 
                point_alpha = 0.15) %>% 
           map(~.x + 
                 scale_fill_manual(values = globals$clust_colors) + 
                 scale_x_discrete(limits = unname(globals$clust_labs))) %>% 
           set_names(globals$response)) %>% 
    unlist(recursive = FALSE)

  ## appending the plots with p values and effect sizes
  
  clust_mental$violin_plots <- map2(clust_mental$violin_plots, 
                                    filter(clust_mental$test_results, variable %in% globals$response)[['plot_lab']], 
                                    ~.x + labs(subtitle = .y))
  
# Displaying the mental scoring differences in a single violon plot panel -----
  
  insert_msg('Violin plot panel')
  
  clust_mental$violin_panel <- list(data = clust_mental$analysis_tbl, 
                                    plot_tag = clust_mental$n_numbers) %>% 
    pmap(violin_panel, 
         variables = globals$response, 
         clust_var = 'clust_id', 
         split_by_cluster = FALSE, 
         split_var_type = FALSE) %>% 
    map(~.x + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = '') + 
          facet_grid(. ~ feature, 
                     scales = 'free', 
                     space = 'free') + 
          coord_flip() + 
          labs(y = 'Min/max normalized score') + 
          theme(axis.title.x = element_blank(), 
                axis.title.y = element_text(size = 8, 
                                            angle = 90, 
                                            margin = ggplot2::margin(r = 2, unit = 'mm'))))
  
# Frequency of depression and anxiety within the clusters ------
  
  insert_msg('Frequency of depression and anxiety')
  
  clust_mental$frequency <- clust_mental$analysis_tbl %>% 
    map(~explore(data = .x, 
                 variables = c('phq_depression_positive', 
                               'phq_anxiety_positive'), 
                 split_factor = 'clust_id', 
                 what = 'list') %>% 
          map(~rbind(.x$phq_depression_positive$statistic[2, ], 
                     .x$phq_anxiety_positive$statistic[2, ])) %>% 
          map(mutate, 
              variable = c('phq_depression_positive', 
                           'phq_anxiety_positive')) %>% 
          map2_dfr(., names(.), ~mutate(.x, clust_id = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y))
  
# Bar plot with the frequencies ----
  
  insert_msg('Frequency bar plot')
  
  clust_mental$frequency_plot <- clust_mental$frequency %>% 
    ggplot(aes(x = percent, 
               y = cohort, 
               fill = clust_id)) + 
    facet_grid(.~variable, 
               labeller = as_labeller(c(phq_anxiety_positive = 'ANX+', 
                                        phq_depression_positive = 'DPR+'))) + 
    geom_bar(stat = 'identity', 
             position = position_dodge(width = 0.9), 
             color = 'black', 
             alpha = 0.75) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = '') + 
    scale_y_discrete(labels = globals$cohort_labs) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(x = '% cluster', 
         tag = paste0('\nAT: ', clust_mental$n_numbers$north, 
                      '\n\nIT: ', clust_mental$n_numbers$south))
  
  clust_mental$frequency_plot <- clust_mental$frequency_plot + 
    geom_text(data = mutate(clust_mental$test_results, 
                            percent = 25, 
                            clust_id = 'IR') %>% 
                filter(!variable %in% globals$response), 
              aes(label = plot_lab), 
              size = 2.75, 
              hjust = 0)

# END ----
  
  insert_tail()