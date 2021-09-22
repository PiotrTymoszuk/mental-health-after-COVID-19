# This script characterizes the cluster mambers in terms of variables concerning demography
# social status, medical history, course and recovery

  insert_head()

# data container -----
  
  clust_char <- list()
  
# globals: variables non used for clustering or mental health scoring, analysis tables -----
  
  ## variables 
  
  clust_char$variables <- rforest$variables[!rforest$variables %in% partclust$variables]
  
  ## analysis table
  
  clust_char$analysis_tbl <- map2(cov_data, 
                                  partclust$clust_results %>% 
                                    map(~.x$assignment), 
                                  right_join, ## to get rid of 
                                  by = 'ID') %>% 
    map(select, 
        ID, 
        clust_name,
        all_of(clust_char$variables))
  
# serial analysis and test summaries -----
  
  insert_msg('Serial analysis')
  
  clust_char$analyses <- clust_char$analysis_tbl %>% 
    map(function(x)  clust_char$variables %>% 
          map(analyze_feature, 
              inp_tbl = x,
              split_var = 'clust_name')) %>% 
    map(set_names, 
        clust_char$variables)
  
  clust_char$summaries <- clust_char$analyses %>% 
    map(function(x) x %>% 
          map_dfr(extract_test_summary)) %>% 
    map(mutate, 
        p_adj = p.adjust(p_value, 'BH'), 
        significant = ifelse(p_adj < 0.05, 'yes', 'no'))
  
# identifying the significant factors -----
  
  insert_msg('Significant factors')
  
  clust_char$signif_factors <- clust_char$summaries %>% 
    map(filter, 
        significant == 'yes') %>% 
    map(~.x$variable)
  
  clust_char$signif_factors$common <- clust_char$signif_factors %>% 
    reduce(intersect)

# calculating the prevalence of the significant factors, merging with the testing results -----
  
  insert_msg('Frequency of the significat factors in the risk clusters')
  
  clust_char$prevalence_signif <- clust_char$analyses %>% 
    map(~.x[clust_char$signif_factors$common]) %>% 
    map(function(x) x %>% 
          map_dfr(extract_counts)) %>% 
    map2(., 
         clust_char$summaries %>% 
           map(~.x[c('variable', 'p_value', 'p_adj')]), 
         left_join, 
         by = 'variable') %>% 
    map2_dfr(., 
             names(.), 
             function(x, y) mutate(x, cohort = y))
  
# finding the 10 most significant factors in each cohort ----
  
  insert_msg('Top factors')
  
  clust_char$top_factors <- clust_char$summaries %>% 
    map(filter, 
        significant == 'yes') %>% 
    map(top_n, 
        10, 
        -p_adj) %>% 
    map(~.x$variable)
  
  clust_char$top_factors$common <- clust_char$top_factors %>% 
    reduce(union)
  
# estimating the prevalence of the common top factors in the clusters ------
  
  insert_msg('Prevalence within the clusters')
  
  clust_char$prevalence_top <- clust_char$analyses %>% 
    map(~.x[clust_char$top_factors$common]) %>% 
    map(function(x) x %>% 
          map_dfr(extract_counts)) %>% 
    map(mutate, 
        var_label = translate_var(variable), 
        plot_label = paste(var_label, strata, sep = ':'))

# representing the significant results in a pie plot -----
  
  insert_msg('Plotting the frequencies of the top significant factors in the mental disorder risk clusters')
  
  clust_char$pie_plot <- list(clust_prev_tbl = clust_char$prevalence_top, 
                              plot_title = c('AT', 'IT')) %>% 
    pmap(plot_prev_pie)
  
# END ----
  
  insert_tail()