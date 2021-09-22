# This script generates table for the manuscript ------

  insert_head()

# tools ------

  library(writexl)  

# data containers -----

  paper_tables <- list()
  suppl_tables <- list()
  
# Table 1 - 3: baseline characteristic, CoV course and psychosocial features of the study populations ------
  
  insert_msg('Table 1 - 3: cohort characteristic')
  
  paper_tables$cohort_features <- list(analysis_table = c(cohort_features$main_tables, 
                                                          list(psych_vars  = cohort_features$tables$psych_vars)), 
                                       hide_mean = c(T, T, F)) %>% 
    pmap(format_main_tbl)
  
  ## Table 1 and 2: no/yes removal, expanding the showtcuts
  
  paper_tables$cohort_features$baseline_vars <- paper_tables$cohort_features$baseline_vars %>% 
    mutate(Variable = car::recode(Variable, 
                                  "'Pre-CoV depression/anxiety' = 'Depression/anxiety before COVID-19'; 
                                  'Pre-CoV sleep disorders' = 'Sleep disorders before COVID-19'"), 
           Variable = stri_replace(Variable, 
                                   fixed = 'Sum', 
                                   replacement = 'Number'))  %>% 
    map_dfc(stri_replace, 
            regex = 'no:\\s{1}.*\\nyes:\\s{1}', 
            replacement = '') 
  
  paper_tables$cohort_features$course_vars <- paper_tables$cohort_features$course_vars %>% 
    mutate(Variable = stri_replace(Variable, 
                                   fixed = 'NC', 
                                   replacement = 'neurocognitive symptoms'), 
           Variable = stri_replace(Variable, 
                                   fixed = 'persist.', 
                                   replacement = 'persistent'), 
           Variable = stri_replace(Variable, 
                                   fixed = '#', 
                                   replacement = 'Number of')) %>% 
    map_dfc(stri_replace, 
            regex = 'no:\\s{1}.*\\nyes:\\s{1}', 
            replacement = '')
  
  paper_tables$cohort_features$psych_vars <- paper_tables$cohort_features$psych_vars %>% 
    mutate(Variable = car::recode(Variable, 
                                  "'OMH score' = 'Overall Mental Health Score'; 
                                  'QoL score' = 'Quality of Life Score';
                                  'DPR score ' = 'Depression Score'; 
                                  'DPR+' = 'Depression Screening-positive'; 
                                  'ANX score' = 'Anxiety score'; 
                                  'ANX+' = 'Anxiety Screening-positive'; 
                                  'Stress score' = 'Psychosocial Stress Score'")) %>% 
    map_dfc(stri_replace, 
            regex = 'no:\\s{1}.*\\nyes:\\s{1}', 
            replacement = '')

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
  
# Supplementary Table S2: supplementary cohort characteristic and COVID-19 course -----
  
  insert_msg('Table S2: supplementary cohort characteristic of the cohort and COVID-19 course')
  
  suppl_tables$suppl_cohort_features <- cohort_features$supplements %>% 
    reduce(rbind) %>% 
    format_main_tbl
  
  suppl_tables$suppl_cohort_features <- suppl_tables$suppl_cohort_features %>% 
    mutate(Variable = car::recode(Variable, 
                                  "'Freq. resp. infections' = '> 2 respiratory infections per year'; 
                                  'Freq. bact. Infections' = '> 2 bacterial infections per year'; 
                                  'Observation time' = 'Time between survey and diagnosis'"))  %>% 
    map_dfc(stri_replace, 
            regex = 'no:\\s{1}.*\\nyes:\\s{1}', 
            replacement = '') 
  
# Supplementary Table S3: results of univariate modeling -----
  
  insert_msg('Table S3: results of univariate modeling')
  
  suppl_tables$uni_modeling <- psych_analyses$summary_tbl %>% 
    format_res_tbl %>% 
    select(`Co-variate`, 
           Response, 
           Cohort, 
           N, 
           `Exp. estimate`, 
           `2.5% CI`, 
           `97.5% CI`, 
           Significance) %>% 
    mutate(`Co-variate` = stri_replace(`Co-variate`, 
                                       fixed = ': yes', 
                                       replacement = ''))

# Supplementary Table S4: pooled results of univariate modeling -----
  
  insert_msg('Table S4: pooled results of uni-variate modeling')
  
  suppl_tables$pooled_univariate <- psych_analyses$pooled_summary %>% 
    select( - p_adj) %>% 
    ddply(.(response), 
          filter, 
          !duplicated(parameter)) %>% 
    as_tibble %>% 
    mutate(p_adj = p.adjust(p_value, 'BH')) %>% 
    format_res_tbl %>% 
    arrange(`Co-variate`, 
            Response) %>% 
    select(`Co-variate`, 
           Response, 
           `Exp. estimate`, 
           `2.5% CI`, 
           `97.5% CI`, 
           Significance)
  
# Supplementary Table S5: frequency of modeling features in the mental disorder risk clusters -----
  
  insert_msg('Table S5: frequency of the modeling features in the mental disorder risk clusters')
  
  suppl_tables$clust_freq <- clust_char$prevalence_signif %>% 
    mutate(cohort = car::recode(cohort, "'north' = 'Austria'; 'south' = 'Italy'"), 
           significance = ifelse(p_adj >= 0.05, 'ns', 
                                 paste('p =', signif(p_adj, 2))), 
           variable = translate_var(variable), 
           frequency = paste0(signif(percent, 3), 
                              '% (', 
                              n, 
                              ')')) %>% 
    select(variable, 
           strata, 
           cohort, 
           split_var, 
           frequency, 
           total_n, 
           significance) %>% 
    set_names(c('Variable', 
                'Strata', 
                'Cohort', 
                'Cluster', 
                'Frequency', 
                'N', 
                'Significance'))
  
# saving the tables ----
  
  insert_msg('Saving the tables in the disc')
  
  paper_tables$cohort_features[c('baseline_vars', 
                                 'course_vars', 
                                 'psych_vars')] %>% 
    set_names(paste('Table', 1:3)) %>% 
    write_xlsx('./paper/tables/tables.xlsx')
  
  suppl_tables %>% 
    set_names(paste('Table S', 1:5, sep = '')) %>% 
    write_xlsx('./paper/supplementary tables/supplementary_tables.xlsx')
  
# END -----
  
  insert_tail()