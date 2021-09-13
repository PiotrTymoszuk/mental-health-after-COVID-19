# This script checks for the differences in mental health, quality of life, physical performance scoring, 
# depression positivity, anxiety positivity between the strata identified as interesting by random forests

  insert_head()
  
# data containers -----
  
  psych_analyses <- list()


# globals: variables of interest and variable lexicons -----
  
  insert_msg('Globals setup')
  
  ## modeling responses
  
  psych_analyses$responses <- rforest_plots$responses

  ## top independent variables
  
  psych_analyses$indep_vars <- rforest_plots$pca %>% 
    map(function(x) x$top_influential$variable) %>% 
    unlist %>% 
    unique
  
  ## top parameters
  
  psych_analyses$top_parameters <- rforest_plots$pca %>% 
    map(function(x) x$top_influential$parameter) %>% 
    unlist %>% 
    unique

# uni-variate modeling -----
  
  insert_msg('Univariate modeling of the psychosocial scoring')

  psych_analyses$models <- cov_data %>% 
    map(function(x) psych_analyses$responses %>% 
          map(make_lm_model,  
              indep_variable = psych_analyses$indep_vars, 
              mod_fun = glm, 
              family = 'quasipoisson', 
              weight_variable = 'freq_weight', 
              data = x,
              .parallel = F))

# Extracting the summaries and p value correction ------
  
  insert_msg('EXtraction of significance')
  
  psych_analyses$summary_tbl <- psych_analyses$models %>% 
    map(function(x) x %>% 
          get_model_summary) %>% 
    map2_dfr(., names(.), 
             function(x, y) mutate(x, 
                                   cohort = y))


# Identification of the factors correlating with the scoring in both cohorts ----
  
  insert_msg('Identification of the significant factors')
  
  psych_analyses$summary_tbl <- psych_analyses$summary_tbl %>% 
    filter(level != 'baseline') %>% 
    ddply(.(variable, response, level), 
          mutate, 
          significant = ifelse(all(p_adj < 0.05), 'yes', 'no')) %>% 
    as_tibble %>% 
    mutate(regulation = ifelse(significant == 'no', 
                               'ns', 
                               ifelse(estimate > 1, 'up', 'down')))

# Convenience vectors with the significant variables -----
  
  insert_msg('Identification of the significant factors')

  psych_analyses$signif_fct <-  psych_analyses$summary_tbl  %>% 
    filter(significant == 'yes') %>% 
    dlply(.(regulation), 
          function(x) unique(x$variable))
  
  
# plotting: heat map with the estimates of the top parameters identified by RF ------
  
  insert_msg('Displaying the top significant factors in form of bubble plots')

  psych_analyses$bubble_plot <- psych_analyses$summary_tbl %>% 
    ddply(.(variable, level), 
          function(x) if(all(x$significant == 'yes')) x else NULL) %>% 
    filter(parameter %in% psych_analyses$top_parameters) %>% 
    plot_analysis_hm(plot_title = 'Mental health scoring', 
                                                 plot_subtitle = 'Uni-variate Poisson regression', 
                                                 resp_order = c('phq_anxiety_score', 
                                                                'phq_depression_score', 
                                                                'mental_health_score', 
                                                                'life_quality_score'), 
                                                 bubble = T)

# Meta-analysis: the pooled betas for the TY and STY cohorts, top parameters identified by RF -----
  
  insert_msg('Calulating the pooled TY/STY beta estimates')
  
  psych_analyses$pooled_summary <- psych_analyses$summary_tbl %>% 
    ddply(.(variable, level), 
          function(x) if(all(x$significant == 'yes')) x else NULL) %>% 
    filter(parameter %in% psych_analyses$top_parameters) %>% 
    ddply(.(response), aggregate_modeling) %>% 
    as_tibble
  
# Traditional forest plots with the estimates of the top parameters identified by RF ----
  
  insert_msg('Forest plots for the top parameters')
  
  psych_analyses$forest_plots <- psych_analyses$responses %>% 
    map(plot_univar_forest, 
        mod_summary_tbl = psych_analyses$summary_tbl, 
        params_to_plot = psych_analyses$top_parameters) %>% 
    set_names(psych_analyses$responses)

# END -----
  
  insert_tail()