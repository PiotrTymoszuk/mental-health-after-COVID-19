# A mother script executing the particular analyses

# tools -----

  c('./tools/cov_project_tools.R', 
    './tools/cov_project_globals.R', 
    './tools/clust_tools2.R') %>% 
  source_all(message = TRUE, 
             crash = TRUE)

  library(exda)
  library(mgcv)

# executable scripts -----

  c('./analysis scripts/participant_clustering.R', 
    './analysis scripts/cluster_mental_scoring.R', 
    './/analysis scripts/cluster_features.R', 
    './analysis scripts/obs_time.R',
    './analysis scripts/survey_date.R', 
    './analysis scripts/overlap.R', 
    './analysis scripts/da_phenotyping.R') %>% 
    source_all(message = TRUE, 
               crash = TRUE)

# END -----