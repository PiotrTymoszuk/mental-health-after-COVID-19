# Checks and plots the distribution of the most important numeric study variables (normality, kurtosis, skewness). 
# Compares the distributions between the AT and IT cohorts.

  insert_head()
  
# container list -----
  
  eda_dist <- list()
  
# globals and analysis table -----
  
  insert_msg('Globals and analysis table')
  
  eda_dist$variables <- eda_globals$globals_tbl %>% 
    filter(variable_type == 'numeric') %>% 
    .$variable
  
  eda_dist$variable_labs <- translate_var(eda_dist$variables)

  eda_dist$analysis_tbl <- eda_globals$analysis_tbl %>% 
    map(select, 
        all_of(eda_dist$variables))

# Mean, median, SD and IQR, skewness, kurtosis and normality ----
  
  insert_msg('Statistics of the distribution')
  
  ## base distribution stats
  
  eda_dist$distr_stats <- eda_dist$analysis_tbl %>% 
    map(explore, what = 'table')
  
  ## skewness and kurtosis
  
  eda_dist$skewness <- eda_dist$analysis_tbl %>% 
    map(explore, what = 'skewness')
  
  eda_dist$kurtosis <- eda_dist$analysis_tbl %>% 
    map(explore, what = 'kurtosis')
  
  ## normality check
  
  eda_dist$normality <- eda_dist$analysis_tbl %>% 
    map(explore, what = 'normality', pub_styled = TRUE) %>% 
    map(mutate, 
        plot_lab = paste(test_stat, significance, sep = ', '))
  
# Comparing the distributions with KS test -----
  
  insert_msg('Comparing the distributions')
  
  eda_dist$ks_test <- compare_variables(eda_dist$analysis_tbl$north, 
                                        eda_dist$analysis_tbl$south, 
                                        variables = eda_dist$variables, 
                                        what = 'distribution', 
                                        adj_method = 'BH', 
                                        pub_styled = TRUE)
  
# Comparing the variances with Levene test -----
  
  insert_msg('Comparing the variances')
  
  eda_dist$levene_test <- compare_variables(eda_dist$analysis_tbl$north, 
                                            eda_dist$analysis_tbl$south, 
                                            variables = eda_dist$variables, 
                                            what = 'variance', 
                                            adj_method = 'BH', 
                                            pub_styled = TRUE)
  
# Distribution plots: histograms -----
  
  insert_msg('Distribution histograms')
  
  ## histogram plots
  
  eda_dist$histograms <- list(data = eda_dist$analysis_tbl, 
                              fill_color = globals$cohort_colors) %>% 
    pmap(function(data, fill_color) list(variables = eda_dist$variables, 
                                         bins = filter(eda_globals$globals_tbl, 
                                                       variable_type == 'numeric')$bins) %>% 
           pmap(explore, 
                data = data, 
                fill_color = fill_color, 
                what = 'plots', 
                type = 'hist', 
                cust_theme = globals$common_theme) %>% 
           map(~.x[[1]]) %>% 
           set_names(eda_dist$variables))

  ## setting nicer plot titles and adding the results of Shapiro-Wilk normality testing
  ## to the plot sub-captions
  
  eda_dist$histograms[c('north', 'south')] <- c('north', 'south') %>% 
    map(function(cohort) list(plot = eda_dist$histograms[[cohort]], 
                              plot_title = paste(globals$cohort_labs[cohort], eda_dist$variable_labs, sep = ': '), 
                              plot_subtitle = eda_dist$normality[[cohort]]$plot_lab) %>% 
          pmap(function(plot, plot_title, plot_subtitle) plot + labs(title = plot_title, subtitle = plot_subtitle)))

# END ----
  
  rm(i, new_var)
  
  insert_tail()
