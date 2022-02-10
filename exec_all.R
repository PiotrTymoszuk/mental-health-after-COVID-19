# This program executes the scripts of the COVID psychosocial project

  library(soucer) ## available from https://github.com/PiotrTymoszuk/soucer

# executing the scripts ----
  
  exec_log <- source_all(c('data_import.R', 
                           'data_exploration.R', 
                           'risk_modeling.R', 
                           'data_analyses.R', 
                           'render_paper.R', 
                           'shiny_export.R'), 
                         crash = FALSE, 
                         message = TRUE)
  
  print(exec_log)
  
  write_tsv(exec_log, 
            'exec_log.log')

  save.image()
  
# END ----