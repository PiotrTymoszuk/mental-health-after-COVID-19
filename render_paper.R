# A mother script executing the table, figure and paper rendering scripts

# tools ----

  c('./tools/sys_tools.R', 
    './tools/cov_project_tools.R', 
    './tools/cov_project_globals.R') %>% 
    walk(source)

# executable scripts -----

  c('./paper scripts/cohort_characteristic.R', 
    './paper scripts/paper_tables.R', 
    './paper scripts/paper_figures.R', 
    './paper scripts/render_paper.R') %>% 
    walk(source)

# END -----