# This script checks for the differences in mental health, quality of life, physical performance scoring, 
# depression positivity, anxiety positivity between the strata identified as interesting by random forests.
# Univariate Poisson modeling.

  insert_head()
  
# data container -----
  
  psych_analyses <- list()

# analysis tables with the min/max scaled numeric variables ------
  
  psych_analyses$analysis_tbl <- cov_data %>% 
    map(select, 
        all_of(globals$response), 
        all_of(rforest$cmm_factors)) %>% 
    map(~map_dfc(.x, function(x) if(is.numeric(x)) min_max(x) else x)) %>% 
    map2(., map(cov_data, ~.x[, 'freq_weight']), cbind)
  
# uni-variate modeling -----
  
  insert_msg('Univariate modeling of the psychosocial scoring')

  psych_analyses$models$north <- globals$response %>% 
    map(function(outcome) rforest$cmm_factors %>% 
          map(~make_lm(data = psych_analyses$analysis_tbl$north, 
                       response = outcome, 
                       indep_variable = .x, 
                       mod_fun = glm, 
                       family = 'quasipoisson', 
                       weight_variable = 'freq_weight')) %>% 
          set_names(rforest$cmm_factors)) %>% 
    set_names(globals$response)
  
  psych_analyses$models$south <- globals$response %>% 
    map(function(outcome) rforest$cmm_factors %>% 
          map(~make_lm(data = psych_analyses$analysis_tbl$south, 
                       response = outcome, 
                       indep_variable = .x, 
                       mod_fun = glm, 
                       family = 'quasipoisson', 
                       weight_variable = 'freq_weight')) %>% 
          set_names(rforest$cmm_factors)) %>% 
    set_names(globals$response)

# Extracting inference summaries, R-squared and p value correction ------
  
  insert_msg('EXtraction of inference stats')
  
  ## inference summary tables
  
  psych_analyses$mod_summary <- psych_analyses$models %>% 
    map(function(cohort) cohort %>% 
          map_dfr(~map_dfr(.x, summary, type = 'inference', transf_fun = exp))) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y)) %>% 
    mutate(p_adj = p.adjust(p_value, 'BH')) %>% 
    filter(variable != 'Intercept')
  
  ## fit stats
  
  psych_analyses$fit_stats <- psych_analyses$models %>% 
    map(function(cohort) cohort %>% 
          map_dfr(~map(.x, summary, type = 'fit') %>% 
                    map2_dfr(., names(.), ~mutate(.x, variable = .y)))) %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y))

  ## a convenience vector with variables significant in both cohorts
  
  psych_analyses$signif_vars <- psych_analyses$mod_summary %>% 
    dlply(.(response, variable), 
          function(x) if(all(x$p_adj < 0.05)) x else NULL) %>% 
    compact %>% 
    reduce(rbind) %>% 
    as_tibble %>% 
    dlply(.(response), function(x) unique(x$variable))
  
# Traditional forest plots with the estimates of the top parameters identified by RF ----
  
  insert_msg('Forest plots for the top parameters')
  
  psych_analyses$forest_plots <-  list(mod_response = globals$response, 
                                       plot_title = globals$response_labels, 
                                       variables = psych_analyses$signif_vars) %>% 
    pmap(plot_univar_forest, 
         data_inference = psych_analyses$mod_summary, 
         data_fit = psych_analyses$fit_stats) %>% 
    set_names(globals$response)

# END -----
  
  insert_tail()