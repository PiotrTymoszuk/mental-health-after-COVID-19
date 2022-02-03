# This script performs PCA on the delta MSE values for the most important factors identified by random forest modeling.

  insert_head()
  
# container list -----
  
  rf_pca <- list()
  
# analysis tables -----
  
  insert_msg('Analysis table')
  
  rf_pca$analysis_tbl <- rforest$train_summary %>% 
    map(filter, variable %in% rforest$cmm_factors) %>% 
    map(mutate, 
        variable_label = ifelse(level %in% c('', 'yes'), 
                                variable_label, 
                                paste(variable_label, level, sep = ': '))) %>% 
    map(select, parameter, variable_label, delta_mse) %>% 
    reduce(left_join, by = c('parameter', 'variable_label')) %>% 
    set_names(c('parameter', 'variable_label', names(rforest$train_summary)))

  rf_pca$pca_tbl <- rf_pca$analysis_tbl %>% 
    select(-variable_label) %>% 
    column_to_rownames('parameter') %>% 
    t
  
# PCA and standard plots ----
  
  insert_msg('PCA analysis')
  
  ## PCA object
  
  rf_pca$pca_object <- reduce_data(data = rf_pca$pca_tbl, 
                                   kdim = 4, 
                                   red_fun = 'pca')
  
  rf_pca$pca_object$loadings$variable <- rf_pca$analysis_tbl$variable_label
  
  ## PCA variances
  
  rf_pca$pca_var <- var(rf_pca$pca_object)
  
  rf_pca$pca_val_lab <- map2_chr(paste0('PC', 1:4), 
                                 rf_pca$pca_var$perc_var, 
                                 ~paste0(.x, ', ', signif(.y, 3), '%')) %>% 
    set_names(paste('comp', 1:4, sep = '_'))
  
  ## scree and loading plots
  
  rf_pca[c('scree_plot', 'loadings_plot')] <- c('scree', 'loadings') %>% 
    map(~plot(rf_pca$pca_object, 
              type = .x, 
              cust_theme = globals$common_theme, 
              segment_color = 'gray30', 
              point_color = 'cornsilk3'))
  
# plotting particular components -----
  
  insert_msg('Plotting particular components')
  
  rf_pca$compo_plot <- extract(rf_pca$pca_object, 'loadings') %>% 
    gather(key = 'PC', 
           value = 'loadings', 
           starts_with('comp')) %>% 
    ggplot(aes(x = loadings, 
               y = reorder(variable, loadings), 
               fill = PC)) + 
    geom_vline(xintercept = 0) + 
    facet_grid(.~PC, 
               labeller = as_labeller(rf_pca$pca_val_lab)) + 
    geom_bar(stat = 'identity', 
             color = 'black') + 
    scale_fill_manual(values = c(comp_1 = 'steelblue', 
                                 comp_2 = 'cornsilk', 
                                 comp_3 = 'salmon', 
                                 comp_4 = 'firebrick')) +
    guides(fill = FALSE) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'PCA loadings', 
         x = 'Loadings value')
  
# END -----
  
  insert_tail()

  