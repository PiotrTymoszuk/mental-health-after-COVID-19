# This script tries to find subsets of the study participants in respect
# to the most influential factors identified by RF. AT serves as a training data set, IT is a test set.
# Predictions are done by k-NN driven label propagation.

  insert_head()

# data container -----
  
  partclust <- list()
  
# globals, analysis tables and SOM grids -----
  
  insert_msg('Globals setup')
  
  ## the most influential variables

  partclust$variables <- rforest$cmm_factors

  ## analysis table, min/max transformation
  
  partclust$analysis_tbl <- cov_data %>% 
    map(select, ID, all_of(partclust$variables)) %>% 
    map(~filter(.x, complete.cases(.x)))
  
  partclust$id_vec <- partclust$analysis_tbl %>% 
    map(~.x$ID)
  
  partclust$analysis_tbl <- partclust$analysis_tbl %>% 
    map(select, -ID) %>% 
    map(~map_dfc(.x, as.numeric)) %>% 
    map(~map_dfc(.x, min_max))
  
  partclust$analysis_tbl <- map2(partclust$analysis_tbl, 
                                 partclust$id_vec, 
                                 set_rownames)

# Developing the cluster in the AT cohort, prediction for the IT -----
  
  insert_msg('Participant clustering')
  
  partclust$clust_objects$north <- combi_cluster(data = partclust$analysis_tbl$north, 
                                                 distance_som = 'manhattan', 
                                                 xdim = floor((5*sqrt(nrow(partclust$analysis_tbl$north)))^0.5), 
                                                 ydim = floor((5*sqrt(nrow(partclust$analysis_tbl$north)))^0.5), 
                                                 topo = 'hexagonal', 
                                                 neighbourhood.fct = 'gaussian', 
                                                 toroidal = T, rlen = 2000, 
                                                 node_clust_fun = hcluster, 
                                                 distance_nodes = 'manhattan', 
                                                 k = 3, 
                                                 seed = 1234)
  
  partclust$clust_objects$south <- predict(partclust$clust_objects$north, 
                                           newdata = partclust$analysis_tbl$south, 
                                           type = 'propagation', 
                                           k = 5)
  
  ## renaming of the clusters: LR, IR and HR
  ## for low-, intermediate- and high-risk of mental health disorders
  
  partclust$clust_objects$north$clust_assignment <- extract(partclust$clust_objects$north, 'assignment') %>% 
    mutate(clust_id = car::recode(clust_id, "'1' = 'IR'; '3' = 'HR'; '2' = 'LR'"), 
           clust_id = factor(clust_id, c('LR', 'IR', 'HR')))
  
  partclust$clust_objects$south$clust_assignment <- extract(partclust$clust_objects$south, 'assignment') %>% 
    mutate(clust_id = car::recode(clust_id, "'1' = 'IR'; '3' = 'HR'; '2' = 'LR'"), 
           clust_id = factor(clust_id, c('LR', 'IR', 'HR')))

# Characteristic of the cluster objects -----
  
  insert_msg('Characetristic of the objects')
  
  # training cluster diagnostic plots
  
  partclust$diagn_plots <- c('diagnostic', 'training') %>% 
    map(~plot(combi_analysis_object = partclust$clust_objects$north, 
              type = .x, 
              cust_theme = globals$common_theme)) %>% 
  set_names(c('diagnostic', 'training'))
  
  ## clustering variance
  
  partclust$clust_var <- partclust$clust_objects %>% 
    map(var) %>% 
    map(~.x[c('total_wss', 'total_ss', 'between_ss', 'frac_var')]) %>% 
    map(as_tibble) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y))
  
  ## clustering variance plot
  
  partclust$clust_var_plot <- partclust$clust_var %>% 
    ggplot(aes(y = cohort, 
                x = frac_var, 
                fill = cohort)) + 
    geom_bar(stat = 'identity', 
             color = 'black') + 
    geom_text(aes(label = signif(frac_var, 2)), 
              size = 2.75, 
              color = 'white', 
              hjust = 1.5) + 
    scale_fill_manual(values = globals$cohort_colors) + 
    scale_y_discrete(labels = globals$cohort_labs) + 
    guides(fill = FALSE) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Clustering variance', 
         x = 'between/total sum-of-squares')
  
# PCA of the cluster structures ------
  
  insert_msg('PCA plots of the cluster structures')
  
  partclust$pca_plots <- partclust$clust_objects %>% 
    map(plot, 
        type = 'components', 
        red_fun = 'pca', 
        with = 'data', 
        k = 3, 
        cust_theme = globals$common_theme) %>% 
    map2(., c('AT: training', 'IT: test'), 
         ~.x$final + 
          scale_fill_manual(values = globals$clust_colors) +
           labs(title = .y, 
                subtitle = 'SOM/HCl clusetring, Manhattan distance'))
  
# Heat maps of the clustering features ------
  
  insert_msg('Heat maps of the clusetring features')
  
  partclust$features_hm <- list(sample_clust_object = partclust$clust_objects, 
                                plot_title = c('Participant clustering, AT cohort', 
                                               'Participant clusetring, IT cohort'), 
                                plot_subtitle = 'SOM/HCl clustering, Manhattan distance') %>% 
    pmap(plot_clust_hm, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          scale_y_discrete(labels = translate_var(partclust$variables, short = TRUE), 
                           limits = rev(c('sum_symptoms_acute', 
                                          'sum_symptoms_subacute', 
                                          'neurocognitive_acute_sympt_sum', 
                                          'neurocognitive_subacute_sympt_sum', 
                                          'imp_concentration_acute', 
                                          'imp_concentration_subacute', 
                                          'perf_impairment', 
                                          'stress_score'))) + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'black', 
                               high = 'firebrick', 
                               midpoint = 0.5, 
                               name = 'Feature:\n\n0: low/absent\n1: high/present\n') + 
          labs(x = 'Participant'))

# END ----

  insert_tail()