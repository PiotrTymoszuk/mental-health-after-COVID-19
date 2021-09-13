# A mother script executing the particular analyses

# tools -----

  c('./tools/sys_tools.R', 
    './tools/cov_project_tools.R', 
    './tools/cov_project_globals.R', 
    './tools/counting_tools.R', 
    './tools/lm_qc_tools.R', 
    './tools/clust_tools.R') %>% 
  walk(source)

# executable scripts -----

  c('./analysis scripts/symptom_da_sex.R', 
    './analysis scripts/mqp_das_factors_interest.R', 
    './analysis scripts/das_positive_interest.R', 
    './analysis scripts/participant_clustering.R', 
    './analysis scripts/cluster_characteristic.R') %>% 
    walk(source)

# END -----