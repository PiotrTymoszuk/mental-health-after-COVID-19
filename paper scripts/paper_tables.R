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
  
# Supplementary Table S5: prevalence of PHQ depression and anxiety in the strata of interest -----
  
  insert_msg('Table S5: prevalence of PHQ depression and anxiety in the strata of interest')
  
  suppl_tables$da_prevalence <- das_prevalnce$result_tbl %>% 
    mutate(variable = translate_var(variable, short = F),
           strata = split_var) %>% 
    map_dfc(stri_replace, 
            fixed = ' da_significance (', 
            replacement = '/n(') %>%
    select(variable, 
           strata, 
           cohort, 
           phq_anxiety_positive, 
           phq_depression_positive, 
           phq_anxiety_significance, 
           phq_depression_significance) %>% 
    mutate(cohort = ifelse(cohort == 'north', 
                           'Austria', 
                           'Italy'), 
           variable = stri_replace(variable, 
                                   fixed = 'depression/anxiety', 
                                   replacement = 'depr/anx.')) %>% 
    set_names(c('Variable',
                'Strata', 
                'Cohort', 
                'Anx. prevalence', 
                'Depr. prevalence',
                'Anx. significance', 
                'Depr. significance'))
  
# Supplementary Table S6: frequency of modeling features in the mental disorder risk clusters -----
  
  insert_msg('Table S6: frequency of the modeling features in the mental disorder risk clusters')
  
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
    set_names(paste('Table S', 1:6, sep = '')) %>% 
    write_xlsx('./paper/supplementary tables/supplementary_tables.xlsx')
  
# END -----
  
  insert_tail()