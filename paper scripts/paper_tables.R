# This script generates table for the manuscript ------

  insert_head()

# data containers -----

  paper_tables <- list()
  suppl_tables <- list()
  
# Table 1 - 3: baseline characteristic, CoV course and psychosocial features of the study populations ------
  
  insert_msg('Table 1 - 3: cohort characteristic')
  
  paper_tables$cohort_features <- eda_comp$cohort_characteristic %>% 
    select(variable, 
           north, 
           south, 
           test, 
           significance, 
           eff_size)

  ## splitting into single tables
  
  paper_tables <- eda_globals$globals_tbl %>% 
    dlply(.(target_table), function(x) x$variable) %>% 
    map(~filter(paper_tables$cohort_features, 
                variable %in% .x)) %>% 
    map(format_main_tbl)

  ## renaming
  
  paper_tables <- paper_tables %>% 
    map(set_names, 
        c('Variable', 'AT', 'IT', 'Test', 'pFDR', 'Effect size'))

# Supplementary Table S1: study variables used in model construction ------
  
  insert_msg('Table S1: study variables used in model constrtuction')
  
  suppl_tables$modeling_vars <- globals$var_lexicon %>% 
    filter(modeling_variable == 'yes') %>% 
    select(label, 
           label_short,
           levels, 
           description) %>% 
    set_names(c('Variable label', 
                'Short label', 
                'Variable strata', 
                'Description'))

# Supplementary Table S2: results of univariate modeling -----
  
  insert_msg('Table S2: results of univariate modeling')
  
  suppl_tables$uni_modeling <- psych_analyses$mod_summary[c('cohort', 
                                                            'response', 
                                                            'variable', 
                                                            'level', 
                                                            'n', 
                                                            'n_complete', 
                                                            'estimate', 
                                                            'lower_ci', 
                                                            'upper_ci', 
                                                            'p_adj')] %>% 
    left_join(psych_analyses$fit_stats[c('cohort', 
                                         'response', 
                                         'variable', 
                                         'adj_rsq')], 
              by = c('cohort', 'response', 'variable'))
  
  suppl_tables$uni_modeling <- suppl_tables$uni_modeling %>% 
    mutate(cohort = globals$cohort_labs[cohort], 
           response = globals$response_labels[response], 
           variable = translate_var(variable), 
           variable = ifelse(level == 'yes' | is.na(level), 
                             variable, 
                             paste(variable, level, sep = ': ')), 
           estimate = paste0(signif(estimate, 2), ' [', 
                             signif(lower_ci, 2), ' - ', 
                             signif(upper_ci, 2), ']'), 
           adj_rsq = signif(adj_rsq, 2), 
           significance = ifelse(p_adj < 0.05, 
                                 paste('p =', signif(p_adj, 2)), 
                                 paste0('ns (p = ', signif(p_adj, 2), ')'))) %>% 
    select(cohort, 
           response, 
           variable, 
           n, 
           n_complete, 
           estimate, 
           adj_rsq, 
           significance) %>% 
    set_names(c('Cohort', 
                'Response', 
                'Indep. variable', 
                'N variable level', 
                'Complete observations', 
                'exp \u03B2', 
                'R\u00B2', 
                'pFDR'))
  
# Supplementary Table S3: characteristic of the subsets with and without pre-CoV depression/anxiety, significant features ----
  
  insert_msg('Table S3. Characteristic of the population with and without depression/anxiety')
  
  suppl_tables$da_features <- pheno_da$descr_tbl %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y)) %>% 
    format_main_tbl %>% 
    mutate(cohort = globals$cohort_labs[cohort]) %>% 
    select(cohort, 
           variable, 
           da_negative, 
           da_positive, 
           test, 
           significance, 
           eff_size) %>% 
    set_names(c('Cohort', 'Variable', 'DA-negative', 'DA-positive', 'Test', 'pFDR', 'Effect size'))
  
# saving the tables ----
  
  insert_msg('Saving the tables in the disc')
  
  paper_tables %>% 
    set_names(paste('Table', 1:length(paper_tables))) %>% 
    write_xlsx('./paper/tables.xlsx')

  suppl_tables %>% 
    set_names(paste('Table S', 1:length(suppl_tables), sep = '')) %>% 
    write_xlsx('./paper/supplementary_tables.xlsx')
  
# END -----
  
  insert_tail()