# A mother script executing the table, figure and paper rendering scripts

# tools ----

  c('./tools/cov_project_tools.R', 
    './tools/cov_project_globals.R') %>% 
    source_all(source, crash = TRUE, message = TRUE)

  library(writexl)
  library(figur)
  library(cowplot)
  library(Cairo)
  library(bookdown)
  library(knitr)
  library(rmarkdown)
  library(flextable)

# executable scripts -----

  c('./paper scripts/paper_tables.R', 
    './paper scripts/main_figures.R', 
    './paper scripts/suppl_figures.R', 
    './paper scripts/export_chunks.R', 
    './paper scripts/deploy_paper.R') %>% 
    source_all(source, crash = TRUE, message = TRUE)

# END -----