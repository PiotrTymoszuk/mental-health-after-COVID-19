# functions ----

  extract_beta <- function(modeling_summary, co_variate, response = NULL, signif_digits = 3) {
    
    ## extracts the beta with CI for the paper text

    if(!is.null(response)) {
      
      beta_tbl <- modeling_summary %>% 
        filter(Response == response, 
               `Co-variate` == co_variate)
      
      beta_txt <- paste('\u03B2 = ', 
                        signif(beta_tbl$`Exp. estimate`, signif_digits), 
                        ' [95%CI: ', 
                        signif(beta_tbl$`2.5% CI`, signif_digits), 
                        ' \u2013 ', 
                        signif(beta_tbl$`97.5% CI`, signif_digits), 
                        ']', sep = '')
      
      return(beta_txt)
      
    } else {
      
      beta_lst <- c('OMH score', 
                    'QoL score', 
                    'ANX score', 
                    'DPR score') %>% 
        map(extract_beta, 
            modeling_summary = modeling_summary, 
            co_variate = co_variate, 
            signif_digits = signif_digits) %>% 
        set_names(c('OMH', 
                    'QoL', 
                    'ANX', 
                    'DPR'))
      
      beta_txt <- map2_chr(beta_lst, 
                           names(beta_lst), 
                           function(x, y) paste(y, x, sep = ': ')) %>% 
        paste(collapse = ', ')
      
      return(beta_txt)
      
    }
    
  } 
  
# pooled betas for the text -----
  
  txt_betas <- c('Stress score: 4Q', 
                 'Stress score: 3Q', 
                 '# acute symptoms: 4Q', 
                 '# persistent symptoms: >3', 
                 'Imp. concentration acute COVID-19: yes', 
                 'Forgetfulness acute COVID-19: yes', 
                 'Confusion acute COVID-19: yes', 
                 'Pre-CoV depression/anxiety: yes') %>% 
    map(extract_beta,
        modeling_summary = suppl_tables$pooled_univariate) %>% 
    set_names(c('Stress score: 4Q', 
                'Stress score: 3Q', 
                '# acute symptoms: 4Q', 
                '# persistent symptoms: >3', 
                'Imp. concentration acute COVID-19: yes', 
                'Forgetfulness acute COVID-19: yes', 
                'Confusion acute COVID-19: yes', 
                'Pre-CoV depression/anxiety: yes'))
  
# Long covid and relapse in DA ------
  
  long_cov_da <- cov_data %>% 
    map(analyze_feature, 
        variable = 'long_covid', 
        split_var = 'depression_burnout')
  
  relapse_da <- cov_data %>% 
    map(analyze_feature, 
        variable = 'relapse', 
        split_var = 'depression_burnout')
  
  convalescence_da <- cov_data %>% 
    map(analyze_feature, 
        variable = 'complete_covelescence', 
        split_var = 'depression_burnout')
  
# Shiny data export ------
  
  save(cov_data, 
       file = 'survey_data.RData')
  
  clust_assignment <- partclust$clust_results %>% 
    map(~.x$assignment) %>% 
    map(select, 
        ID, clust_name)
  
  save(clust_assignment, 
       file = 'clust_assign.RData')
  
