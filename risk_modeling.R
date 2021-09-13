# A mother script executing the particular analyses

# tools -----

  c('./tools/sys_tools.R', 
    './tools/cov_project_tools.R', 
    './tools/cov_project_globals.R') %>% 
    walk(source)

# executable scripts ----

  c('./modeling scripts/rf_modeling.R', 
    './modeling scripts/rf_plots.R', 
    './modeling scripts/psych_soc_measures.R') %>% 
    walk(source)

# END -----