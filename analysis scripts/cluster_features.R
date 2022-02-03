# Compares mental scoring and prevalence of clinically significant depression/anxiety between the clusters

  insert_head()
  
# container list ----
  
  clust_ft <- list()
  
# globals: variables and analysis tables -----
  
  insert_msg('Globals setup')
  
  clust_ft$assignment <- partclust$clust_objects %>% 
    map(extract, 'assignment') %>% 
    map(mutate, ID = observation) %>% 
    map(select, ID, clust_id)
  
  clust_ft$analysis_tbl <- cov_data %>% 
    map(select, all_of(globals$variables), ID) %>% 
    map2(., clust_mental$assignment, left_join, by = 'ID') %>% 
    map(filter, !is.na(clust_id))
  
  clust_ft$test_types <- globals$variables %>% 
    map_chr(~class(clust_ft$analysis_tbl$north[[.x]])) %>% 
    car::recode("'numeric' = 'kruskal_eta'; 'factor' = 'cramer_v'")
  
  ## n numbers
  
  clust_ft$n_numbers <- partclust$clust_objects %>% 
    map(ngroups) %>% 
    map(~paste0('\nLR: n = ', .x$n[1], 
                ', IR: n = ', .x$n[2], 
                ', HR: n = ', .x$n[3]))
  
# testing for differences between the clusters: Kruskal test for numeric and Chi-Sq test for factors -----
  
  insert_msg('Testing for differences between the clusters')
  
  clust_ft$test_results <- clust_ft$analysis_tbl %>% 
    map(~compare_variables(.x, 
                           variables = globals$variables, 
                           split_factor = 'clust_id', 
                           what = 'eff_size', 
                           types = clust_ft$test_types, 
                           ci = FALSE, 
                           pub_styled = FALSE, 
                           adj_method = 'BH', 
                           .parallel = TRUE, 
                           .paropts = furrr_options(seed = TRUE, 
                                                    globals = 'clust_ft')))
  
  ## labeling of the significant effects, filtering testing failures out
  
  clust_ft$test_results <- clust_ft$test_results %>% 
    map(filter, !is.infinite(estimate)) %>% 
    map(mutate, 
        significant = ifelse(p_adjusted < 0.05, 'significant', 'ns'), 
        var_lab = translate_var(variable)) %>% 
    map(arrange, 
        -estimate)
  
# Separate tables with testing results for numeric and non-numeric features -----
  # defining the variables with at least
  
  insert_msg('Test results for clustering features and other features')
  
  clust_ft$test_results_numeric <- clust_ft$test_results %>% 
    map(filter, test == 'Kruskal-Wallis test') %>% 
    map(mutate, 
        strong_var = ifelse(estimate > 0.06, 'yes', 'no'), 
        plot_lab = ifelse(strong_var == 'yes', var_lab, NA))
  
  clust_ft$test_results_factor <- clust_ft$test_results %>% 
    map(filter, test != 'Kruskal-Wallis test') %>% 
    map(mutate, 
        strong_var = ifelse(estimate > 0.3, 'yes', 'no'), 
        plot_lab = ifelse(strong_var == 'yes', var_lab, NA))
  
# Common moderate-to-strong variables identified in both datasets -----
  
  insert_msg('Common moderate-to-strong variables')
  
  clust_ft$cmm_factors <- clust_ft[c('test_results_numeric', 
                                     'test_results_factor')] %>% 
    map(~map(.x, filter, strong_var == 'yes') %>% 
          map(~.x$variable) %>% 
          reduce(intersect)) %>% 
    reduce(union)

# Volcano plot with the results ------
  
  insert_msg('Plotting as volcanos')
  
  clust_ft$volcano_plots <- list(data = c(clust_ft$test_results_numeric, 
                                          clust_ft$test_results_factor), 
                                 plot_subtitle = c('AT: numeric features, Kruskal-Wallis test', 
                                                   'IT: numeric features, Kruskal-Wallis test', 
                                                   'AT: categorical features, Chi-squared test', 
                                                   'IT: categorical feaatures, Chi-squared test'), 
                                 effect_cutoff = c(0.06, 0.06, 0.3, 0.3), 
                                 plot_tag = rep(clust_ft$n_numbers, 2), 
                                 x_lab = list(expression('Effect size, '*eta^2), 
                                              expression('Effect size, '*eta^2), 
                                              'Effect size, V', 
                                              'Effect size, V')) %>% 
    pmap(plot_volcano, 
         plot_title = 'Differences between participant clusters') %>% 
    set_names(c('north_numeric', 
                'south_numeric', 
                'north_factor', 
                'south_factor'))
  
# Presenting the moderate-to-strong features as a heat map ------
  
  insert_msg('Heat map for the moderate-to-strong factors')
  
  clust_ft$hm_plots <- list(data = clust_ft$analysis_tbl, 
                            plot_subtitle = c('Moderate-to-strong features: AT', 
                                              'Moderate-to-strong features: IT'), 
                            plot_tag = clust_ft$n_numbers) %>% 
    pmap(plot_hm, 
         plot_title = 'Differences between participant clusters')
  
# END ----
  
  insert_tail()