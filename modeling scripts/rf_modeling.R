# This script identifies relevant factors connected with the scoring of (1) mental health, quality of life,
# physical performance loss and (2) the scores of depression, anxiety or stress
# screening by PHQ. Modeling with random Forests.Verification: 10 fold CV. AT serves as a training cohort, IT is the test.

  insert_head()

# data container ----

  rforest <- list()

# globals: responses, formulas and variables -----

  insert_msg('random forest modeling globals')

  ## formulas
  
  rforest$formulas <- globals$response %>% 
    map(~paste(.x, paste(globals$variables, collapse = '+'), sep = '~')) %>% 
    map(as.formula) %>%
    set_names(globals$response)

# analysis tables -----

  insert_msg('Analysis tables')

  ## complete cases only
  
  rforest$inp_tbl <- cov_data %>% 
    map(function(cohort) globals$response %>% 
          map(~select(cohort, ID, all_of(c(.x, globals$variables)))) %>% 
          map(~filter(.x, complete.cases(.x))) %>% 
          set_names(globals$response))
  
  ## min/max scaling of the numeric variables
  
  rforest$inp_tbl$north <- rforest$inp_tbl$north %>% 
    map(~map_dfc(.x, function(x) if(is.numeric(x)) min_max(x) else x))
  
  rforest$inp_tbl$south <- rforest$inp_tbl$south %>% 
    map(~map_dfc(.x, function(x) if(is.numeric(x)) min_max(x) else x))
  
  ## ID to row names
  
  rforest$inp_tbl$north <- rforest$inp_tbl$north %>% 
    map(column_to_rownames, 'ID')
  
  rforest$inp_tbl$south <- rforest$inp_tbl$south %>% 
    map(column_to_rownames, 'ID')

# training the models in the North Tyrol cohort -------

  insert_msg('Model training')
  
  set.seed(1234)
  
  registerDoParallel(7)
  
  rforest$models_train_north <- list(form = rforest$formulas, 
                                     data = rforest$inp_tbl$north) %>% 
    pmap(caret:::train.formula, 
         method = 'ranger', 
         metric = 'RMSE', 
         importance = 'impurity_corrected', 
         tuneGrid = data.frame(mtry = c(50, 100, 200), 
                               splitrule = 'variance', 
                               min.node.size = c(rep(10, 3), rep(20, 3), rep(40, 3), rep(60, 3))), 
         trControl = trainControl(method = 'cv', 
                                  number = 10, 
                                  returnData = TRUE,
                                  returnResamp = 'final',
                                  savePredictions = 'final'))
  
  stopImplicitCluster()

  ## converting to caretx objects enabling for extended predictions with the test data set
  
  rforest$models_train_north <- rforest$models_train_north %>% 
    map(caretx)
  
# raw model predictions in training, CV and test data sets ----
  
  insert_msg('Model predictions')
  
  rforest$predictions <- list(object = rforest$models_train_north, 
                              newdata = rforest$inp_tbl$south) %>% 
    pmap(predict)

# raw model prediction stats -----
  
  insert_msg('Model prediction stats')
  
  rforest$pred_stats <- list(object = rforest$models_train_north, 
                             newdata = rforest$inp_tbl$south) %>% 
    pmap(summary, 
         plain = TRUE) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))
  
# model calibration by quantile GAM ----
  
  insert_msg('Model calibration')

  rforest$calibrated_preds <- list(x = rforest$models_train_north, 
                                   newdata = rforest$inp_tbl$south) %>% 
    pmap(calibration, 
         qu = seq(0.4, 0.7, by = 0.05))

# calibrated model prediction stats -----
  
  insert_msg('Calibrated model prediction stats')
  
  rforest$calibrated_pred_stats <- rforest$calibrated_preds %>% 
    map(~.x[c('train', 'cv', 'test')]) %>% 
    map(~map(.x, summary) %>% 
          map2_dfr(., names(.), ~mutate(.x, prediction = .y))) %>% 
    map2_dfr(., names(.), ~mutate(.x, response = .y))

# obtaining the model component impact tables for the training cohorts ------
  
  insert_msg('Traning cohort model components and their impact')
  
  rforest$train_summary <- rforest$models_train_north %>% 
    map(extract_rf_importance)

# vectors with top20 most influential factors -----
  
  insert_msg('Top 20 most influential factors')
  
  rforest$top_factors <- rforest$train_summary %>% 
    map(top_n, 20, delta_mse) %>% 
    map(~.x$variable) %>% 
    map(unique)
  
  rforest$cmm_factors <- rforest$top_factors %>% 
    reduce(intersect)

# END -----
  
  insert_tail()