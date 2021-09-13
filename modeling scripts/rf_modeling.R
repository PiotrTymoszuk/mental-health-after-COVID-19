# This script identifies relevant factors connected with the scoring of (1) mental health, quality of life,
# physical performance loss and (2) the scores of depression, anxiety or stress
# screening by PHQ. Modeling with random Forests.Verification: 10 fold CV


  insert_head()
  
# tools ------
  
  library(doParallel)
  library(caret)

# data container ----

  rforest <- list()

# globals: responses, families and variables -----

  insert_msg('random forest modeling globals')

  ## responses

  rforest$response <- globals$var_lexicon %>% 
    filter(response == 'yes') %>% 
    .$variable

  ## independent variables

  rforest$variables <- globals$var_lexicon %>% 
    filter(modeling_variable == 'yes') %>% 
    .$variable

# analysis tables and n number vectors -----

  insert_msg('Analysis tables')

  ## ready to use table for training and predictions, complete cases only

  rforest$inp_tbl <- cov_data %>% 
    map(function(x) rforest$response %>% 
          map(function(y) select(x, 
                                 ID, 
                                 all_of(c(y, rforest$variables)))) %>% 
          map(function(y) filter(y, 
                                 complete.cases(y))) %>% 
          map(column_to_rownames, 
              'ID') %>% 
          set_names(rforest$response))
  
  rforest$n_numbers <-  rforest$inp_tbl %>% 
    map(function(x) x %>% 
          map(nrow))
  
# training the models in the North Tyrol and south Tyrol cohorts -------

  insert_msg('Model training')
  
  rforest$analysis_train_north <- list(response = rforest$response, 
                                       train_data = rforest$inp_tbl$north) %>% 
    pmap(train_cv_rf, 
         mtry_vec = 500) %>% 
    set_names(rforest$response)
  
  rforest$analysis_train_south <- list(response = rforest$response, 
                                       train_data = rforest$inp_tbl$south) %>% 
    pmap(train_cv_rf, 
         mtry_vec = 500) %>% 
    set_names(rforest$response)
  
# model predictions in the training cohorts -----
  
  insert_msg('Model predictions in the training cohorts')

  rforest$predictions_north <- list(caret_model = rforest$analysis_train_north, 
                                    response = rforest$response, 
                                    new_data = rforest$inp_tbl$north) %>% 
    pmap(predict_cv_rf)
  
  rforest$predictions_south <- list(caret_model = rforest$analysis_train_south, 
                                    response = rforest$response, 
                                    new_data = rforest$inp_tbl$south) %>% 
    pmap(predict_cv_rf)

# model predictions in the test cohorts -----
  
  insert_msg('Model predictions in the test cohorts')

  rforest$predictions_test_north <- list(caret_model = rforest$analysis_train_north, 
                                         response = rforest$response, 
                                         new_data = rforest$inp_tbl$south) %>% 
    pmap(predict_cv_rf)
  
  rforest$predictions_test_south <-  list(caret_model = rforest$analysis_train_south, 
                                          response = rforest$response, 
                                          new_data = rforest$inp_tbl$north) %>% 
    pmap(predict_cv_rf)


# obtaining the model component impact tables for the training cohorts ------
  
  insert_msg('Traning cohort model components and their impact')
  
  rforest$train_summary <- rforest[c('analysis_train_north', 
                                     'analysis_train_south')] %>% 
    map(function(x) x %>% 
          map(extract_rf_summary) %>% 
          map2(., 
               names(.), 
               function(y, z) mutate(y, response = z))) %>% 
    map2(., 
         c('north', 'south'), 
         function(x, y) x %>% 
           map(mutate, 
               cohort = y)) %>% 
    set_names(c('north', 'south'))

  ## adding the n numbers
  
  rforest$train_summary <- map2(rforest$train_summary, 
                                rforest$n_numbers, 
                                function(x, y) map2(x, y, 
                                                    function(a, b) mutate(a, n_complete = b)))
  
# vectors with top20 most influential factors -----
  
  insert_msg('Top 20 most influential factors')
  
  rforest$top_factors <- rforest$train_summary %>% 
    map(function(x) x %>% 
          map(top_n, 20, delta_mse) %>% 
          map(function(y) y$variable))

# obtaining common summaries for plotting -----
  
  insert_msg('Common summaries for ploting')
  
  rforest$cmm_train_summary <- map2(rforest$train_summary$north, 
                                    rforest$train_summary$south, 
                                    rbind)
  
# END -----
  
  insert_msg()