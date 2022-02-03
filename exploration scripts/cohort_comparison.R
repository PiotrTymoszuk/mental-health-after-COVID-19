# compares the features of the cohorts.

  insert_head()
  
# container list ----
  
  eda_comp <- list()
  
# Descriptive statistics of the variables -----
  
  insert_msg('Descriptive statistics')
  
  eda_comp$desc_table <- eda_globals$analysis_tbl %>% 
    map(explore, 
        what = 'table') %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'north', 'south'))
  
# Comparison by Mann-Whitney or Chi-squared tests for the numeric and factor variables, respectively -----
  
  insert_msg('Testing for differences')
  
  eda_comp$comp_results <- compare_variables(eda_globals$analysis_tbl$north, 
                                             eda_globals$analysis_tbl$south, 
                                             variables = eda_globals$globals_tbl$variable, 
                                             what = 'eff_size', 
                                             types = eda_globals$globals_tbl$eff_size, 
                                             pub_styled = TRUE, 
                                             adj_method = 'BH', 
                                             ci = FALSE) %>% 
    mutate(plot_sub = paste(significance, eff_size, sep = ', '))
  
# A common table with descriptive statistics and testing results ------
  
  insert_msg('A common table with the cohort characteristic')
  
  eda_comp$cohort_characteristic <- left_join(eda_comp$desc_table, 
                                              eda_comp$comp_results, 
                                              by = 'variable')
  
# Plotting the comparisons, p values presented in the plot captions ------
  
  insert_msg('Plotting the comparisons')

  eda_comp$plots <- list(variable = eda_globals$globals_tbl$variable, 
                         plot_title = translate_var(eda_globals$globals_tbl$variable, short = FALSE), 
                         plot_subtitle = eda_comp$comp_results$plot_sub) %>% 
    pmap(plot_variable, 
         eda_globals$analysis_tbl$north, 
         eda_globals$analysis_tbl$south, 
         data_names = globals$cohort_labs, 
         scale = 'percent', 
         cust_theme = globals$common_theme) %>% 
    set_names(eda_globals$globals_tbl$variable)

  eda_comp$plots <- eda_comp$plots %>% 
    map(~.x + scale_fill_manual(values = unname(globals$cohort_colors)))
  
# END -----
  
  insert_tail()