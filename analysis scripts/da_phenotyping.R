# Characterizes the DA- and DA+ subsets in respect to the large set of explanatory variables used for RF modeling
# Strongly regulated factors are identified by Benjamini-Hochberg-adjusted test result and non-zero effect size
# (Cremer's V or min/max normalized median differences)

  insert_head()
  
# Container list ----
  
  pheno_da <- list()
  
# Globals: analysis tables with min/max normalized numeric variables -----
  
  insert_msg('Globals setup')
  
  pheno_da$response <- c(globals$response, 
                         globals$variables)

  pheno_da$analysis_tbl <- cov_data %>% 
    map(select, 
        ID, 
        depression_burnout, 
        all_of(pheno_da$response)) %>% 
    map(mutate, 
        depression_burnout = ifelse(depression_burnout == 'yes', 'DA+', 'DA-'), 
        depression_burnout = factor(depression_burnout, c('DA-', 'DA+')))
  
  pheno_da$response <- pheno_da$response[pheno_da$response != 'depression_burnout']
  
  pheno_da$test_types <- pheno_da$response %>% 
    map_chr(function(x) if(is.numeric(pheno_da$analysis_tbl$north[[x]])) 'wilcoxon_r' else 'cramer_v')
  
# Serial testing -----
  
  insert_msg('Serial testing')
  
  pheno_da$test_results <- pheno_da$analysis_tbl %>% 
    map(~compare_variables(.x, 
                           variables = pheno_da$response, 
                           split_factor = 'depression_burnout', 
                           what = 'eff_size', 
                           types = pheno_da$test_types, 
                           ci = FALSE, 
                           pub_styled = FALSE, 
                           adj_method = 'BH', 
                           .parallel = TRUE, 
                           .paropts = furrr_options(seed = TRUE, globals = 'pheno_da')))
  
# Identification of the significantly and strongly regulated features -----
  
  insert_msg('Strongly regulated features')
  
  pheno_da$test_results <- pheno_da$test_results %>% 
    map(mutate, 
        abs_estimate = abs(estimate), 
        significant = ifelse(p_adjusted < 0.05, 'yes', 'no'), 
        strong_var = ifelse(significant == 'yes' & abs_estimate > 0, 'yes', 'no'), 
        var_lab = translate_var(variable), 
        plot_lab = ifelse(significant == 'yes', var_lab, NA)) %>% 
    map(filter, 
        !is.infinite(estimate)) ## removing the testing errors
  
  ## common significant factors
  
  pheno_da$cmm_factors <- pheno_da$test_results %>% 
    map(filter, significant == 'yes') %>% 
    map(~.$variable) %>% 
    reduce(intersect)
  
  ## n numbers
  
  pheno_da$n_numbers <- pheno_da$analysis_tbl %>% 
    map(count, 
        depression_burnout) %>% 
    map(~map2_chr(.x$depression_burnout, 
                  .x$n, 
                  paste, sep = ': n = ') %>% 
          paste(collapse = ', ') %>% 
          paste('\n', .))
  
# Plotting the results as a Volcano plot -----
  
  insert_msg('Volcano plots with the results')
  
  pheno_da$volcano_plots <- list(data = pheno_da$test_results, 
                                 plot_subtitle = c('AT, Chi-squared or Mannn-Whitney test', 
                                                   'IT, Chi-squared or Mannn-Whitney test'), 
                                 plot_tag = pheno_da$n_numbers) %>% 
    pmap(plot_volcano, 
         estimate_var = 'abs_estimate', 
         eff_index = 'strong_var', 
         eff_lab = 'Significant', 
         effect_cutoff = 0, 
         jitter_h = 0, 
         jitter_w = 0, 
         plot_title = 'Differences between DA+ and DA-', 
         x_lab = 'Effect size, V or r')
  
# Presenting the results as a violin panel ----
  
  insert_msg('Violino panel with the results')
  
  pheno_da$violin_panel <- list(data = pheno_da$analysis_tbl, 
                                plot_subtitle = c('Common significant features: AT', 
                                                  'Common significant features: IT'), 
                                plot_tag = pheno_da$n_numbers) %>% 
    pmap(violin_panel,
         variables = pheno_da$cmm_factors, 
         clust_var = 'depression_burnout', 
         plot_title = 'Differences between DA+ and DA-', 
         plot_iqr = FALSE, 
         split_by_cluster = FALSE) %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'coral3'), 
                            name = ''))
  
# A table with the significantly regulated features -----
  
  insert_msg('Table with the significantly different features')
  
  ## descriptive statistic
  
  pheno_da$descr_tbl <- pheno_da$analysis_tbl %>% 
    map(~explore(data = .x, 
                 split_factor = 'depression_burnout', 
                 variables = pheno_da$response, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(~map(.x, filter, variable %in% pheno_da$cmm_factors)) %>% 
    map(~reduce(.x, left_join, by = 'variable')) %>% 
    map(set_names, 
        c('variable', 'da_negative', 'da_positive'))
  
  ## inference in the publication style
  
  pheno_da$test_results <- pheno_da$analysis_tbl %>% 
    map(~compare_variables(.x, 
                           variables = pheno_da$response, 
                           split_factor = 'depression_burnout', 
                           what = 'eff_size', 
                           types = pheno_da$test_types, 
                           ci = FALSE, 
                           pub_styled = TRUE, 
                           adj_method = 'BH', 
                           .parallel = TRUE, 
                           .paropts = furrr_options(seed = TRUE, globals = 'pheno_da')))

  ## joining and filtering
  
  pheno_da$descr_tbl <- map2(pheno_da$descr_tbl, 
                             pheno_da$test_results, 
                             left_join, 
                             by = 'variable')
  
# END -----
  
  insert_tail()