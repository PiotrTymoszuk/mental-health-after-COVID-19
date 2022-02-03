# Correlations between the mental health scoring.

  insert_head()

# data container -----
  
  overlap <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  overlap$analysis_tbl <- cov_data %>% 
    map(select, 
        ID, 
        all_of(globals$response), 
        depression_burnout, 
        mental_health, 
        life_quality, 
        phq_depression_positive, 
        phq_anxiety_positive) %>% 
    map(mutate, 
        poor_omh = ifelse(mental_health == 'poor', 'yes', 'no'), 
        poor_omh = factor(poor_omh), 
        poor_qol = ifelse(life_quality == 'poor', 'yes', 'no'), 
        poor_qol = factor(poor_qol))
  
  ## variable pairs
  
  overlap$score_pairs <- expand.grid(1:4, 1:4) %>% 
    pmap(function(Var1, Var2) globals$response[c(Var1, Var2)])
  
  overlap$ft_pairs <- expand.grid(1:4, 1:4) %>% 
    pmap(function(Var1, Var2) c('poor_omh', 'poor_qol', 'phq_depression_positive', 'phq_anxiety_positive')[c(Var1, Var2)]) 

# Correlations between the mental health scoring: the whole cohort and the DA subsets -----
  
  insert_msg('Correlations between the mental health scoring')
  
  overlap$mental_correlations <- overlap$analysis_tbl %>% 
    map(function(cohort) overlap$score_pairs %>% 
          map_dfr(~correlate_variables(cohort[.x] %>%
                                         set_names(c('x', 'y')) %>% 
                                         filter(complete.cases(.)) %>% 
                                         set_names(.x), 
                                       variables = .x, 
                                       what = 'correlation', 
                                       type = 'spearman', 
                                       adj_method = 'BH', 
                                       pub_styled = FALSE)))
  
  overlap$mental_correlations_da <- overlap$analysis_tbl %>% 
    map(filter, depression_burnout == 'yes') %>% 
    map(function(cohort) overlap$score_pairs %>% 
          map_dfr(~correlate_variables(cohort[.x] %>%
                                         set_names(c('x', 'y')) %>% 
                                         filter(complete.cases(.)) %>% 
                                         set_names(.x), 
                                       variables = .x, 
                                       what = 'correlation', 
                                       type = 'spearman', 
                                       adj_method = 'BH', 
                                       pub_styled = FALSE)))
  
# Plotting the correlation results as a bubble plot -----
  
  insert_msg('Correlograms')
  
  overlap$mental_plots <- list(data = overlap$mental_correlations, 
                               plot_title = c('Mental scoring: AT', 
                                              'Mental scoring: IT'), 
                               plot_tag = c(paste('\nn =', min(overlap$mental_correlations$north$n)), 
                                            paste('\nn =', min(overlap$mental_correlations$south$n)))) %>% 
    pmap(plot_correlogram, 
         plot_subtitle = 'Spearman correlation, \u03C1', 
         fill_lab = expression(rho), 
         size_lab = expression('abs('*rho*')'))
  
  overlap$mental_plots_da <- list(data = overlap$mental_correlations_da, 
                                  plot_title = c('Mental scoring: AT, DA+', 
                                                 'Mental scoring: IT, DA+'), 
                                  plot_tag = c(paste('\nn =', min(overlap$mental_correlations_da$north$n)), 
                                               paste('\nn =', min(overlap$mental_correlations_da$south$n)))) %>% 
    pmap(plot_correlogram, 
         plot_subtitle = 'Spearman correlation', 
         fill_lab = expression(rho), 
         size_lab = expression('abs('*rho*')'))
  
# Kappa for the overlap between clinically significant depression/anxiety, poor OMH and QoL -----
  
  insert_msg('Kappa for the overlaps between the depression/anxiety signs and poor mental health')
  
  overlap$mental_kappa <- overlap$analysis_tbl %>% 
    map(function(cohort) overlap$ft_pairs %>% 
          map_dfr(~correlate_variables(cohort[.x] %>%
                                         set_names(c('x', 'y')) %>% 
                                         filter(complete.cases(.)) %>% 
                                         set_names(.x), 
                                       variables = .x, 
                                       what = 'correlation', 
                                       type = 'kappa', 
                                       adj_method = 'BH', 
                                       pub_styled = FALSE)))
  
  overlap$mental_kappa_da <- overlap$analysis_tbl %>% 
    map(filter, depression_burnout == 'yes') %>% 
    map(function(cohort) overlap$ft_pairs %>% 
          map_dfr(~correlate_variables(cohort[.x] %>%
                                         set_names(c('x', 'y')) %>% 
                                         filter(complete.cases(.)) %>% 
                                         set_names(.x), 
                                       variables = .x, 
                                       what = 'correlation', 
                                       type = 'kappa', 
                                       adj_method = 'BH', 
                                       pub_styled = FALSE)))

# Plotting the kappas ------
  
  insert_msg('Plotting the kappas')
  
  overlap$kappa_plots <- list(data = overlap$mental_kappa, 
                               plot_title = c('Mental disorder signs: AT', 
                                              'Mental disorder signs: IT'), 
                               plot_tag = c(paste('\nn =', min(overlap$mental_kappa$north$n)), 
                                            paste('\nn =', min(overlap$mental_kappa$south$n)))) %>% 
    pmap(plot_correlogram, 
         plot_subtitle = "Cohen's \u03BA", 
         fill_lab = expression(kappa), 
         size_lab = expression('abs('*kappa*')'))
  
  overlap$kappa_plots_da <- list(data = overlap$mental_kappa_da, 
                                 plot_title = c('Mental disorder signs: AT, DA+', 
                                                'Mental disorder signs: IT, DA+'), 
                                 plot_tag = c(paste('\nn =', min(overlap$mental_kappa_da$north$n)), 
                                              paste('\nn =', min(overlap$mental_kappa_da$south$n)))) %>% 
    pmap(plot_correlogram, 
         plot_subtitle = "Cohen's kappa", 
         fill_lab = expression(kappa), 
         size_lab = expression('abs('*kappa*')'))

# END -----
  
  insert_tail()
  